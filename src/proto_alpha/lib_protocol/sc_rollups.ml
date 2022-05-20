(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Kind = struct
  (*

      Each time we add a data constructor to [t], we also need:
      - to extend [Sc_rollups.all] with this new constructor ;
      - to update [Sc_rollups.kind_of_string] and [encoding] ;
      - to update [Sc_rollups.pvm_proof] and [pvm_proof_encoding].

  *)
  type t = Example_arith

  let example_arith_case =
    Data_encoding.(
      case
        ~title:"Example_arith smart contract rollup kind"
        (Tag 0)
        unit
        (function Example_arith -> Some ())
        (fun () -> Example_arith))

  let encoding = Data_encoding.union ~tag_size:`Uint16 [example_arith_case]

  let equal x y = match (x, y) with Example_arith, Example_arith -> true

  let pp fmt kind =
    match kind with Example_arith -> Format.fprintf fmt "Example_arith"
end

module PVM = struct
  type boot_sector = string

  module type S = sig
    val name : string

    val parse_boot_sector : string -> boot_sector option

    val pp_boot_sector : Format.formatter -> boot_sector -> unit

    include Sc_rollup_PVM_sem.S
  end

  type t = (module S)
end

let all = [Kind.Example_arith]

let kind_of_string = function "arith" -> Some Kind.Example_arith | _ -> None

let example_arith_pvm = (module Sc_rollup_arith.ProtocolImplementation : PVM.S)

let of_kind = function Kind.Example_arith -> example_arith_pvm

let kind_of (module M : PVM.S) =
  match kind_of_string M.name with
  | Some k -> k
  | None ->
      failwith
        (Format.sprintf "The module named %s is not in Sc_rollups.all." M.name)

let from ~name = Option.map of_kind (kind_of_string name)

let all_names =
  List.map
    (fun k ->
      let (module M : PVM.S) = of_kind k in
      M.name)
    all

let string_of_kind k =
  let (module M) = of_kind k in
  M.name

let pp fmt k = Format.fprintf fmt "%s" (string_of_kind k)

module type PVM_with_proof = sig
  include Sc_rollup_PVM_sem.S

  val proof : proof
end

type pvm_with_proof =
  | Unencodable of (module PVM_with_proof)
  | Arith_pvm_with_proof of
      (module PVM_with_proof
         with type proof = Sc_rollup_arith.ProtocolImplementation.proof)

let pvm_with_proof_module p =
  match p with
  | Unencodable p -> p
  | Arith_pvm_with_proof p ->
      let (module P) = p in
      (module struct
        include P
      end : PVM_with_proof)

let pvm_with_proof_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Arithmetic PVM with proof"
        (Tag 0)
        Sc_rollup_arith.ProtocolImplementation.proof_encoding
        (function
          | Arith_pvm_with_proof pvm ->
              let (module P : PVM_with_proof
                    with type proof =
                      Sc_rollup_arith.ProtocolImplementation.proof) =
                pvm
              in
              Some P.proof
          | _ -> None)
        (fun proof ->
          let module P = struct
            include Sc_rollup_arith.ProtocolImplementation

            let proof = proof
          end in
          Arith_pvm_with_proof (module P));
    ]
