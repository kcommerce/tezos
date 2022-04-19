(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(* An override file stores a mapping from variables to inferred values. *)

type t = Maths.vector Free_variable.Map.t

let add_into_map name duration map =
  let name = Free_variable.of_string name in
  if Free_variable.Map.mem name map then
    Format.eprintf
      "Override.add_into_map: warning, %a already bound@."
      Free_variable.pp
      name ;
  Free_variable.Map.add name duration map

let load_file ~filename map =
  let lines = Csv.import ~filename () in
  let (header, rows) =
    match lines with
    | [] | [_] -> Stdlib.failwith "Override.load_file: invalid csv"
    | header :: rows -> (header, rows)
  in
  let rows =
    List.to_seq rows
    |> Seq.map (fun l ->
           List.to_seq l
           |> Seq.map (fun x ->
                  try float_of_string x
                  with Failure _ ->
                    Stdlib.failwith ("Override.load_file: invalid coeff " ^ x))
           |> Array.of_seq)
    |> Array.of_seq
  in
  let mat = Maths.matrix_of_array_array rows in
  let seq =
    List.mapi
      (fun col var -> (Free_variable.of_string var, Maths.Matrix.col mat col))
      header
    |> List.to_seq
  in
  Free_variable.Map.add_seq seq map

let load ~filenames : t =
  List.fold_left
    (fun map filename -> load_file ~filename map)
    Free_variable.Map.empty
    filenames
