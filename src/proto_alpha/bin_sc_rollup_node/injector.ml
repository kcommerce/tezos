(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol.Alpha_context
open Injector_sigs

type tag = Publish | Add_messages | Cement

module Parameters :
  PARAMETERS with type rollup_node_state = Node_context.t and type Tag.t = tag =
struct
  type rollup_node_state = Node_context.t

  let events_section = ["sc_rollup"]

  module Tag : TAG with type t = tag = struct
    type t = tag

    let compare = Stdlib.compare

    let string_of_tag = function
      | Publish -> "publish"
      | Add_messages -> "add_messages"
      | Cement -> "cement"

    let pp ppf t = Format.pp_print_string ppf (string_of_tag t)

    let encoding : t Data_encoding.t =
      let open Data_encoding in
      string_enum
        (List.map
           (fun t -> (string_of_tag t, t))
           [Publish; Add_messages; Cement])
  end

  (* TODO: to determine
     Very coarse approximation for the number of operation we
     expect for each block *)
  let table_estimated_size : Tag.t -> int = function
    | Publish -> 1
    | Add_messages -> 100
    | Cement -> 1

  let fee_parameter {Node_context.fee_parameter; _} _ = fee_parameter

  (* Below are dummy values that are only used to approximate the
     size. It is thus important that they remain above the real
     values if we want the computed size to be an over_approximation
     (without having to do a simulation first). *)
  (* TODO:
     See TORU issue: https://gitlab.com/tezos/tezos/-/issues/2812
     check the size, or compute them wrt operation kind *)
  let approximate_fee_bound _ _ =
    {
      fee = Tez.of_mutez_exn 3_000_000L;
      counter = Z.of_int 500_000;
      gas_limit = Gas.Arith.integral_of_int_exn 500_000;
      storage_limit = Z.of_int 500_000;
    }

  (* TODO: Decide if some batches must have all the operation to succeed. See
     {!Injector_sigs.Parameter.batch_must_succeed}. *)
  let batch_must_succeed _ = `At_least_one

  let ignore_failing_operation :
      type kind.
      kind manager_operation -> [`Ignore_keep | `Ignore_drop | `Don't_ignore] =
    function
    | _ -> `Don't_ignore

  (** Returns [true] if an included operation should be re-queued for injection
    when the block in which it is included is reverted (due to a
    reorganization). *)
  let requeue_reverted_operation (type kind) _
      (operation : kind manager_operation) =
    let open Lwt_syntax in
    match operation with
    | Sc_rollup_publish _ ->
        (* Commitments are always produced on finalized blocks. They don't need
           to be recomputed. *)
        return_true
    | Sc_rollup_cement _ ->
        (* TODO *)
        return_true
    | Sc_rollup_add_messages _ ->
        (* TODO *)
        return_true
    | _ -> return_true
end

include Injector_functor.Make (Parameters)
