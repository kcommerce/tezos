(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2021 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

(** Testing
    -------
    Component:  Protocol (tx rollup l2)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "tx rollup l2"
    Subject:    test the layer-2 implementation of transaction rollup
*)

open Tztest
open Tx_rollup_l2_helpers

let test_l2_operation_size () =
  let open Protocol.Tx_rollup_l2_batch.V1 in
  let encode_content op =
    Data_encoding.Binary.to_bytes_exn operation_content_encoding op
  in
  let decode_content buffer =
    Data_encoding.Binary.of_bytes_exn operation_content_encoding buffer
  in
  let encode_operation op =
    Data_encoding.Binary.to_bytes_exn operation_encoding op
  in
  let decode_operation buffer =
    Data_encoding.Binary.of_bytes_exn operation_encoding buffer
  in
  let encode_transaction t =
    Data_encoding.Binary.to_bytes_exn transaction_encoding t
  in
  let decode_transaction buffer =
    Data_encoding.Binary.of_bytes_exn transaction_encoding buffer
  in

  let opc =
    {destination = Layer2 (Index 0l); ticket_hash = Index 1l; qty = 12L}
  in
  let buffer = encode_content opc in
  let opc' = decode_content buffer in

  Alcotest.(check int "smallest transfer content" 4 (Bytes.length buffer)) ;
  assert (opc = opc') ;

  let op = {signer = Index 2l; counter = 0L; contents = [opc]} in
  let buffer = encode_operation op in
  let op' = decode_operation buffer in

  Alcotest.(check int "smallest transfer" 7 (Bytes.length buffer)) ;
  assert (op = op') ;

  let t = [op] in
  let buffer = encode_transaction t in
  let t' = decode_transaction buffer in

  Alcotest.(check int "smallest transaction" 8 (Bytes.length buffer)) ;
  assert (t = t') ;

  return_unit

type Environment.Error_monad.error += Test

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/2362
   Use the Irmin store provided by [lib_context] for layer-2
   solutions, once available.
   As of now, we define a ad-hoc [STORAGE] implementation to run our
   tests, but eventually we need to actually make use of the same
   implementation as the transaction rollup node and the protocol. *)

(** [test_irmin_storage] checks that the implementation of [STORAGE]
    has the expected properties. *)
let test_irmin_storage () =
  let open Irmin_storage.Syntax in
  let store = empty_storage in

  let k1 = Bytes.of_string "k1" in
  let k2 = Bytes.of_string "k2" in
  let v1 = Bytes.of_string "v1" in
  let v2 = Bytes.of_string "v2" in

  (* 1. get (set store k1 v1) k1 == Some v1 *)
  let* store = Irmin_storage.set store k1 v1 in
  let* v1' = Irmin_storage.get store k1 in
  assert (v1' = Some v1) ;

  (* 2. k1 != k2 -> get (set store k2 v2) k1 = get store k1*)
  let* store = Irmin_storage.set store k2 v2 in
  let* v1'' = Irmin_storage.get store k1 in
  assert (v1' = v1'') ;

  (* 3. catch (fail e) f return == e *)
  let* e = catch (fail Test) (fun _ -> assert false) return in
  assert (e = Test) ;

  return_unit

let wrap_test t () =
  t () >|= function
  | Ok x -> Ok x
  | Error err -> Error [Environment.Ecoproto_error err]

let tests =
  [
    tztest "test layer-2 operation encoding size" `Quick test_l2_operation_size;
    tztest "test irmin storage" `Quick @@ wrap_test test_irmin_storage;
  ]
