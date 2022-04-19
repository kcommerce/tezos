(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Costlang
open Maths
module NMap = Stats.Finbij.Make (Free_variable)

type constrnt = Full of (Costlang.affine * measure)

and measure = Measure of vector

type problem =
  | Non_degenerate of {
      lines : constrnt list;
      input : matrix;
      output : matrix;
      nmap : NMap.t;
    }
  | Degenerate of {predicted : matrix; measured : matrix}

type solution = {
  mapping : (Free_variable.t * Maths.vector) list;
  weights : matrix;
}

type solver =
  | Ridge of {alpha : float; normalize : bool}
  | Lasso of {alpha : float; normalize : bool; positive : bool}
  | NNLS
  | Bayesian of {
      burn_in : int;
      nsamples : int;
      subsample : int;
      seed : int option;
    }

(* -------------------------------------------------------------------------- *)

(* Establish bijection between variable names and integer dimensions *)
let establish_bijection (lines : constrnt list) : NMap.t =
  let elements =
    List.fold_left
      (fun set line ->
        match line with
        | Full ({linear_comb; _}, _quantity) ->
            Free_variable.Sparse_vec.fold
              (fun elt _count set -> Free_variable.Set.add elt set)
              linear_comb
              set)
      Free_variable.Set.empty
      lines
  in
  NMap.of_list (Free_variable.Set.elements elements)

let line_list_to_ols (lines : constrnt list) =
  let nmap = establish_bijection lines in
  let lcount = List.length lines in
  let inputs = Array.make_matrix lcount (NMap.support nmap) 0.0 in
  let outputs = Array.make_matrix lcount 1 0.0 in
  (* initialize inputs *)
  List.iteri
    (fun i line ->
      match line with
      | Full (affine, Measure vec) ->
          Free_variable.Sparse_vec.iter
            (fun variable multiplicity ->
              let dim = NMap.idx_exn nmap variable in
              inputs.(i).(dim) <- multiplicity)
            affine.linear_comb ;
          let vec = Vector.map (fun qty -> qty -. affine.const) vec in
          outputs.(i) <- vector_to_array vec)
    lines ;
  Tezos_stdlib_unix.Utils.display_progress_end () ;
  (matrix_of_array_array inputs, matrix_of_array_array outputs, nmap)

(* -------------------------------------------------------------------------- *)
(* Computing prediction error *)

type error_statistics = {
  average : float;
  total_l1 : float;
  total_l2 : float;
  avg_l1 : float;
  avg_l2 : float;
  underestimated_measured : float;
}

let pp_error_statistics fmtr err_stat =
  Format.fprintf
    fmtr
    "@[<v 2>{ average = 1/N ∑_i tᵢ - pᵢ = %f;@,\
     total error (L1) = ∑_i |tᵢ - pᵢ| = %f;@,\
     total error (L2) = sqrt(∑_i (tᵢ - pᵢ)²) = %f;@,\
     average error (L1) = 1/N L1 error = %f;@,\
     average error (L2) = 1/N L2 error = %f;@,\
     underestimated = 1/N card{ tᵢ > pᵢ } = %f%% }@]"
    err_stat.average
    err_stat.total_l1
    err_stat.total_l2
    err_stat.avg_l1
    err_stat.avg_l2
    err_stat.underestimated_measured

let compute_error_statistics ~(predicted : matrix) ~(measured : matrix) =
  assert (Linalg.Tensor.Int.equal (Matrix.idim predicted) (Matrix.idim measured)) ;
  assert (Maths.col_dim predicted = 1) ;
  let predicted = vector_to_array (Matrix.col predicted 1) in
  let measured = vector_to_array (Matrix.col measured 1) in
  let error = Array.map2 ( -. ) measured predicted in
  let rows = Array.length error in
  let n = float_of_int rows in
  let arr = Array.init rows (fun i -> error.(i)) in
  let average = Array.fold_left ( +. ) 0.0 arr /. n in
  let total_l1 = Array.map abs_float arr |> Array.fold_left ( +. ) 0.0 in
  let total_l2 =
    let squared_sum =
      Array.map (fun x -> x *. x) arr |> Array.fold_left ( +. ) 0.0
    in
    sqrt squared_sum
  in
  let avg_l1 = total_l1 /. n in
  let avg_l2 = total_l2 /. n in
  let underestimated_measured =
    let indic_under = Array.map (fun x -> if x > 0.0 then 1.0 else 0.0) arr in
    Array.fold_left ( +. ) 0.0 indic_under /. n
  in
  {average; total_l1; total_l2; avg_l1; avg_l2; underestimated_measured}

(* -------------------------------------------------------------------------- *)
(* Making problems *)

let make_problem_from_workloads :
    type workload.
    data:(workload * vector) list ->
    overrides:(Free_variable.t -> float option) ->
    evaluate:(workload -> Eval_to_vector.size Eval_to_vector.repr) ->
    problem =
 fun ~data ~overrides ~evaluate ->
  (match data with
  | [] ->
      Stdlib.failwith
        "Inference.make_problem_from_workloads: empty workload data"
  | _ -> ()) ;
  let line_count = List.length data in
  let model_progress =
    Benchmark_helpers.make_progress_printer
      Format.err_formatter
      line_count
      "Applying model to workload data"
  in
  (* This function has to _preserve the order of workloads_. *)
  let lines =
    List.fold_left
      (fun lines (workload, measures) ->
        model_progress () ;
        let res = Eval_to_vector.prj (evaluate workload) in
        let res = Hash_cons_vector.prj res in
        let affine = Eval_linear_combination_impl.run overrides res in
        (* We hardcode determinization of the empirical timing distribution
           using the median statistic. *)
        let line = Full (affine, Measure measures) in
        line :: lines)
      []
      data
  in
  Format.eprintf "@." ;
  let lines = List.rev lines in
  if
    List.for_all
      (fun (Full (affine, _)) ->
        Free_variable.Sparse_vec.is_empty affine.linear_comb)
      lines
  then
    let (predicted, measured) =
      List.map (fun (Full (affine, Measure vec)) -> (affine.const, vec)) lines
      |> List.split
    in
    let measured =
      matrix_of_array_array (Array.of_list (List.map vector_to_array measured))
    in
    let predicted =
      matrix_of_array_array
        (Array.of_list predicted |> Array.map (fun x -> [|x|]))
    in
    Degenerate {predicted; measured}
  else
    let (input, output, nmap) = line_list_to_ols lines in
    Non_degenerate {lines; input; output; nmap}

let make_problem :
    data:'workload Measure.workload_data ->
    model:'workload Model.t ->
    overrides:(Free_variable.t -> float option) ->
    problem =
 fun ~data ~model ~overrides ->
  let data =
    List.map (fun {Measure.workload; measures} -> (workload, measures)) data
  in
  match model with
  | Model.Packaged {conv; model} ->
      let module M = (val model) in
      let module M = Model.Instantiate (Eval_to_vector) (M) in
      make_problem_from_workloads ~data ~overrides ~evaluate:(fun workload ->
          M.model (conv workload))
  | Model.Preapplied {model} ->
      make_problem_from_workloads ~data ~overrides ~evaluate:(fun workload ->
          let module A = (val model workload) in
          let module I = A (Eval_to_vector) in
          I.applied)

(* -------------------------------------------------------------------------- *)
(* Exporting/importing problems/solutions to CSV *)

let fv_to_string fv = Format.asprintf "%a" Free_variable.pp fv

let to_list_of_rows (m : matrix) : float list list =
  let cols = Maths.col_dim m in
  let rows = Maths.row_dim m in
  List.init ~when_negative_length:() rows (fun r ->
      List.init ~when_negative_length:() cols (fun c -> Matrix.get m (c, r))
      |> WithExceptions.Result.get_ok ~loc:__LOC__)
  |> WithExceptions.Result.get_ok ~loc:__LOC__

let model_matrix_to_csv (m : matrix) (nmap : NMap.t) : Csv.csv =
  let cols = Maths.col_dim m in
  let names =
    List.init ~when_negative_length:() cols (fun i ->
        fv_to_string (NMap.nth_exn nmap i))
    |> (* number of column cannot be negative *)
    WithExceptions.Result.get_ok ~loc:__LOC__
  in
  let rows = to_list_of_rows m in
  let rows = List.map (List.map string_of_float) rows in
  names :: rows

let timing_matrix_to_csv colname (m : matrix) : Csv.csv =
  let rows = to_list_of_rows m in
  let rows = List.map (List.map string_of_float) rows in
  [colname] :: rows

let problem_to_csv : problem -> Csv.csv = function
  | Non_degenerate {input; output; nmap; _} ->
      let model_csv = model_matrix_to_csv input nmap in
      let timings_csv = timing_matrix_to_csv "timings" output in
      Csv.concat model_csv timings_csv
  | Degenerate {predicted; measured} ->
      let predicted_csv = timing_matrix_to_csv "predicted" predicted in
      let measured_csv = timing_matrix_to_csv "timings" measured in
      Csv.concat predicted_csv measured_csv

let solution_to_csv : solution -> (Maths.vector -> float) -> Csv.csv option =
 fun {mapping; _} estimator ->
  match mapping with
  | [] -> None
  | _ ->
      let headers = List.map (fun (fv, _) -> fv_to_string fv) mapping
      and row =
        List.map (fun (_, vec) -> Float.to_string (estimator vec)) mapping
      in
      Some [headers; row]

(* -------------------------------------------------------------------------- *)
(* Solving problems *)

(* Create a [matrix] overlay over a Python matrix.
   Note how we switch from row major to column major in
   order to comply to [Linalg]'s defaults. *)
let of_scipy m =
  let r = Scikit_matrix.dim1 m in
  let c = Scikit_matrix.dim2 m in
  Matrix.make (Linalg.Tensor.Int.rank_two c r) @@ fun (c, r) ->
  Scikit_matrix.get m r c

(* Convert a matrix overlay to a Python matrix. *)
let to_scipy m =
  let cols = Maths.col_dim m in
  let rows = Maths.row_dim m in
  Scikit_matrix.init ~lines:rows ~cols ~f:(fun l c -> Matrix.get m (c, l))

let wrap_python_solver ~input ~output solver =
  (* Scipy's solvers expect a column vector on output. *)
  let output =
    Matrix.of_col
    @@ map_rows
         (fun row ->
           Stats.Emp.quantile (module Float) (vector_to_array row) 0.5)
         output
  in
  let input = to_scipy input in
  let output = to_scipy output in
  solver input output |> of_scipy

let ridge ~alpha ~normalize ~input ~output =
  wrap_python_solver ~input ~output (fun input output ->
      Scikit.LinearModel.ridge ~alpha ~normalize ~input ~output ())

let lasso ~alpha ~normalize ~positive ~input ~output =
  wrap_python_solver ~input ~output (fun input output ->
      Scikit.LinearModel.lasso ~alpha ~normalize ~positive ~input ~output ())

let nnls ~input ~output =
  wrap_python_solver ~input ~output (fun input output ->
      Scikit.LinearModel.nnls ~input ~output)

module Inference = Dagger.Lmh_incremental_inference

(* Returns the [(min, max)] of a [float Seq.t ]*)
let extremal_values seq =
  Seq.fold_left
    (fun (min, max) x -> (Float.min min x, Float.max max x))
    (infinity, neg_infinity)
    seq

(* [support ~x0 ~walk_std f scoring rng_state] computes the approximate support
   of [scoring \circ f]. [x0, walk_std] are respectively the initial position and
   the standard deviation of the random walk exploring the support.
 *)
let support ~x0 ~walk_std f scoring rng_state =
  let open Inference in
  let open Infix in
  let model =
    let+ (x, _) =
      map_log_score
        (let+ x = sample (Stats_dist.brownian ~start:x0 ~std:walk_std) in
         (x, f x))
        scoring
    in
    x
  in
  let (min, max) =
    Inference.stream_samples model rng_state
    |> Benchmark_helpers.take 10000
    |> extremal_values
  in
  (* Ensure inclusion of starting point in interval *)
  (Float.min min x0, Float.max max x0)

(* [bayesian_linear_regression] is a solver that estimates the uncertainty around the provided
   [solution]. *)
let bayesian_linear_regression (solution : matrix) (input : matrix)
    (output : matrix) rng_state =
  let open Dagger in
  let open Inference in
  let open Infix in
  let cols = Maths.col_dim input in
  let rows = Maths.row_dim input in

  assert (Maths.row_dim output = rows) ;

  (* We expect the solution to be deterministic, as provided by eg the lasso solver. *)
  assert (Maths.col_dim solution = 1) ;
  assert (Maths.row_dim solution = cols) ;

  (* [noise_std] is how noisy the measurement process is,
     on average.

     This makes a modelling assumption on the shape of
     the distribution at each measurement: we assume
     that most of the mass is around a peak centered
     on the median and that outliers are all in the
     upper tail of the distribution. (This is what we
     observe empirically).
  *)
  let noise_std =
    sqrt
      ((Maths.map_rows
          (fun vec ->
            let arr = Maths.vector_to_array vec in
            let seq = Array.to_seq arr in
            let median = Stats.Emp.quantile (module Float) arr 0.5 in
            let (min, _) = extremal_values seq in
            let delta = abs_float (median -. min) in
            let filtered =
              Seq.filter (fun x -> abs_float (x -. median) <= delta) seq
            in
            let std =
              Stats.Emp.Float.empirical_variance (Array.of_seq filtered)
            in
            sqrt std)
          output
       |> Maths.Vector.map (fun x -> x *. x)
       |> Maths.Vector.reduce ( +. ) 0.0)
      /. float_of_int rows)
  in

  (* Take median timing as target *)
  let output =
    Maths.map_rows
      (fun vec ->
        Stats.Emp.quantile (module Float) (Maths.vector_to_array vec) 0.5)
      output
    (* Evaluate median now. *)
    |> Maths.vector_to_array
    |> Maths.vector_of_array
  in

  (* This is how noisy the data is, irrespective of [noise_std]. *)
  let data_std =
    sqrt
      ((Maths.Vector.sub
          (Maths.Matrix.col (Maths.Matrix.mm input solution) 0)
          output
       |> Maths.Vector.map (fun x -> x *. x)
       |> Maths.Vector.reduce ( +. ) 0.0)
      /. float_of_int rows)
  in

  let total_std = sqrt ((data_std *. data_std) +. (noise_std *. noise_std)) in

  let predictor_function =
    (* We avoid reallocating storage at each call of [predictor_function]. *)
    let predicted_storage = Array.make_matrix rows 1 0.0 in
    let predicted_out = Maths.out_matrix_of_array_array predicted_storage in
    let predicted = Maths.matrix_of_array_array predicted_storage in

    fun parameters ->
      let vec = Maths.vector_of_array parameters in
      let params = Matrix.of_col vec in
      mm_ predicted_out input params ;
      Matrix.col predicted 0
  in

  let score_function parameters predicted =
    if Array.exists (fun x -> x < 0.0) parameters then Log_space.zero
    else
      let acc = ref Log_space.one in
      assert (Maths.vec_dim predicted = rows) ;
      let exception Reject in
      try
        for r = 0 to rows - 1 do
          let predicted = Vector.unsafe_get predicted r in
          let output = Vector.unsafe_get output r in
          let score =
            Stats.Pdfs.gaussian ~mean:predicted ~std:total_std output
          in
          if score = 0.0 then raise Reject ;
          acc := Log_space.mul !acc (Log_space.of_float score)
        done ;
        !acc
      with Reject -> Log_space.zero
  in

  (* [support i] approximates the support of the posterior on the [i]th paramter.
     It does so by exploring the value of [score_function] for parameter [i]
     when holding all other parameters equal to [solution]. *)
  let support =
    let solution = Maths.vector_to_array (Maths.Matrix.col solution 0) in
    fun i ->
      let p = Float.max solution.(i) 0.1 in
      support
        ~x0:p
        ~walk_std:p
        (fun p ->
          let parameters = Array.copy solution in
          parameters.(i) <- p ;
          (parameters, predictor_function parameters))
        (fun (_p, (parameters, predicted)) ->
          score_function parameters predicted)
        rng_state
  in

  let prior i =
    (* We take a uniform measure on an interval around the estimate. *)
    let (low, high) = support i in
    sample (Stats_dist.flat low high)
  in

  let+ (parameters, _) =
    let samples =
      let+ (parameters, predicted) =
        map_array
          (Array.init cols (fun i ->
               let i =
                 (* There's a bug in [map_array]: it reverses the order of elements.
                    We circumvent it here. Remove this on next update of prbnmcn-dagger. *)
                 cols - i - 1
               in
               prior i))
        @@ fun parameters -> (parameters, predictor_function parameters)
      in
      (parameters, predicted)
    in
    map_log_score samples (fun (parameters, predicted) ->
        score_function parameters predicted)
  in
  (parameters, total_std)

let solve_problem : problem -> solver -> solution =
 fun problem solver ->
  match problem with
  | Degenerate _ -> {mapping = []; weights = empty_matrix}
  | Non_degenerate {input; output; nmap; _} ->
      let weights =
        match solver with
        | Ridge {alpha; normalize} -> ridge ~alpha ~normalize ~input ~output
        | Lasso {alpha; normalize; positive} ->
            lasso ~alpha ~normalize ~positive ~input ~output
        | NNLS -> nnls ~input ~output
        | Bayesian {burn_in; nsamples; subsample; seed} ->
            let weights =
              lasso ~alpha:1.0 ~positive:true ~normalize:false ~input ~output
            in
            let rng_state =
              match seed with
              | None -> Dagger.RNG.make_self_init ()
              | Some seed -> Dagger.RNG.make [|seed|]
            in
            let posterior =
              bayesian_linear_regression weights input output rng_state
            in
            let samples =
              Inference.stream_samples posterior rng_state
              |> Benchmark_helpers.seq_progress Format.std_formatter "sampling"
              |> Benchmark_helpers.drop burn_in
              |> Benchmark_helpers.take_one_every_k ~n:nsamples ~k:subsample
            in
            let dimension = Maths.col_dim input in
            let weights = Array.make_matrix dimension nsamples 0.0 in
            let stds = Array.make nsamples 0.0 in
            Benchmark_helpers.iteri
              (fun i (sample, std) ->
                assert (Array.length sample = dimension) ;
                stds.(i) <- std ;
                for j = 0 to dimension - 1 do
                  weights.(j).(i) <- sample.(j)
                done)
              samples ;
            let avg_std = Maths.(average (vector_of_array stds)) in
            Format.printf "Estimated std around fit: %f@." avg_std ;
            matrix_of_array_array weights
      in
      let lines = Maths.row_dim weights in
      if lines <> NMap.support nmap then
        let cols = Maths.col_dim weights in
        let dims = Format.asprintf "%d x %d" lines cols in
        let err =
          Format.asprintf
            "Inference.solve_problem: solution dimensions (%s) mismatch that \
             of given problem"
            dims
        in
        Stdlib.failwith err
      else
        let mapping =
          NMap.fold
            (fun variable dim acc ->
              let param = Matrix.row weights dim in
              (variable, param) :: acc)
            nmap
            []
        in
        {mapping; weights}
