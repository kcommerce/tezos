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

(** XXX *)

open Sc_rollup_repr

(** XXX: ... *)
type pvm_ops = {
  eval :
    (Raw_level_repr.t * Z.t * string) option ->
    Context.tree ->
    (Context.tree * unit) Lwt.t;
  expect_input :
    Context.tree -> (Context.tree * (Raw_level_repr.t * Z.t) option) Lwt.t;
}

(** There are three cases for a refutation game proof:

    [Computation_step]: a simple step in the PVM that doesn't involve
    any interaction with the inbox.

    [Input_step]: a step in which the PVM 'reads' from the inbox. This
    will include a proof that the machine is in a blocked state and a
    proof that the next message to be read is correct. The inbox proof
    part of this will refer to the [inbox_snapshot] stored in the game
    type (see {!Sc_rollup_game_repr.t}).

    [Blocked_step]: similar to an input step, this is a step where the
    machine is in a blocked state. However, it includes a proof that
    there are no further messages in the inbox at the current level.
    This means the machine is genuinely blocked and the [stop] state
    of this [Proof] will be [None]. *)
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

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

(** The state hash of the machine before the step. *)
val start : t -> State_hash.t

(** The state hash of the machine after the step. *)
val stop : t -> State_hash.t option

(** Check the validity of a proof. This function requires

    [pvm_ops] - the operations [eval] and [expect_input], as implemented
    by the PVM being used in this rollup.

    [inbox] - the snapshot of the inbox that appears in the game. It is
    used to verify the [inbox] proofs in [Input_step] and [Blocked_step]
    proofs.

    [commit_level] - the inbox level of the commit being refuted. This
    is needed to check validity of [Input_step] and [Blocked_step]; the
    [inbox] proof tells us the message and its level. We must make sure
    that level is lower than [commit_level] in an [Input_step], or equal
    to [commit_level] in a [Blocked_step]. *)
val valid :
  pvm_ops ->
  Sc_rollup_inbox_repr.t ->
  Raw_level_repr.t ->
  t ->
  (bool, unit) result Lwt.t
