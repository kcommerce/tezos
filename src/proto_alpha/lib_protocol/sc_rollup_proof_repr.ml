(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

open Sc_rollup_repr

type pvm_ops = {
  eval :
    (Raw_level_repr.t * Z.t * string) option ->
    Context.tree ->
    (Context.tree * unit) Lwt.t;
  expect_input :
    Context.tree -> (Context.tree * (Raw_level_repr.t * Z.t) option) Lwt.t;
}

type t =
  | Computation_step of {
      step : Context.Proof.tree Context.Proof.t;
      not_input : Context.Proof.tree Context.Proof.t;
    }
  | Input_step of {
      step : Context.Proof.tree Context.Proof.t;
      input : Context.Proof.tree Context.Proof.t;
      inbox : Sc_rollup_inbox_repr.Proof.t;
    }
  | Blocked_step of {
      input : Context.Proof.tree Context.Proof.t;
      inbox : Sc_rollup_inbox_repr.Proof.t;
    }

let encoding =
  Data_encoding.(
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"Proof of a normal computation step"
          (Tag 0)
          (tup2
             Context.Proof_encoding.V2.Tree32.tree_proof_encoding
             Context.Proof_encoding.V2.Tree32.tree_proof_encoding)
          (function
            | Computation_step {step; not_input} -> Some (step, not_input)
            | _ -> None)
          (fun (step, not_input) -> Computation_step {step; not_input});
        case
          ~title:"Proof of an input step"
          (Tag 1)
          (tup3
             Context.Proof_encoding.V2.Tree32.tree_proof_encoding
             Context.Proof_encoding.V2.Tree32.tree_proof_encoding
             Sc_rollup_inbox_repr.Proof.encoding)
          (function
            | Input_step {step; input; inbox} -> Some (step, input, inbox)
            | _ -> None)
          (fun (step, input, inbox) -> Input_step {step; input; inbox});
        case
          ~title:"Proof that the PVM is blocked"
          (Tag 2)
          (tup2
             Context.Proof_encoding.V2.Tree32.tree_proof_encoding
             Sc_rollup_inbox_repr.Proof.encoding)
          (function
            | Blocked_step {input; inbox} -> Some (input, inbox) | _ -> None)
          (fun (input, inbox) -> Blocked_step {input; inbox});
      ])

let pp ppf proof =
  match proof with
  | Computation_step _ -> Format.fprintf ppf "Computation step proof"
  | Input_step _ -> Format.fprintf ppf "Input step proof"
  | Blocked_step _ -> Format.fprintf ppf "Blocked step proof"

let start p =
  match p with
  | Computation_step x -> State_hash.of_kinded_hash x.step.before
  | Input_step x -> State_hash.of_kinded_hash x.step.before
  | Blocked_step x -> State_hash.of_kinded_hash x.input.before

let stop p =
  match p with
  | Computation_step x -> Some (State_hash.of_kinded_hash x.step.after)
  | Input_step x -> Some (State_hash.of_kinded_hash x.step.after)
  | Blocked_step _ -> None

let kinded_hash_equal a b =
  match (a, b) with
  | (`Node x, `Node y) -> Context_hash.equal x y
  | (`Value x, `Value y) -> Context_hash.equal x y
  | _ -> false

let tree_proof proof f =
  Lwt.map (Result.map_error (fun _ -> ())) @@ Context.verify_tree_proof proof f

let from_option x = Lwt_syntax.return @@ Result.of_option ~error:() x

let valid pvm_ops snapshot commit_level p =
  let open Lwt_result_syntax in
  match p with
  | Computation_step {step; not_input} ->
      let hashes_match = kinded_hash_equal step.before not_input.before in
      let* _ = tree_proof step (pvm_ops.eval None) in
      let* (_, not_input_result) = tree_proof not_input pvm_ops.expect_input in
      return (hashes_match && Option.is_none not_input_result)
  | Input_step {step; input; inbox} ->
      let hashes_match = kinded_hash_equal step.before input.before in
      let* (_, inbox_location) = tree_proof input pvm_ops.expect_input in
      let* (l, n) = from_option inbox_location in
      let* (l, n, payload_opt) =
        Sc_rollup_inbox_repr.Proof.valid (l, Z.succ n) snapshot inbox
      in
      let* payload = from_option payload_opt in
      let* _ = tree_proof step (pvm_ops.eval (Some (l, n, payload))) in
      return (hashes_match && not (Raw_level_repr.equal l commit_level))
  | Blocked_step {input; inbox} -> (
      let* (_, inbox_location) = tree_proof input pvm_ops.expect_input in
      let* loc = from_option inbox_location in
      let* (l, _, payload_opt) =
        Sc_rollup_inbox_repr.Proof.valid loc snapshot inbox
      in
      match payload_opt with
      | Some _ -> return @@ Raw_level_repr.equal l commit_level
      | None -> return true)
