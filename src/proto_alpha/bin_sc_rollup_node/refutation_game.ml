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

(** This module implements the game refutation logic of the rollup
   node.

   The node monitors the L1 to determine whether it needs to take
   actions regarding refutations. There are three mutually exclusive
   events to consider:

    - a new refutation step has been taken by the opponent: in this
   case, the rollup node must inject the next move in the game ;

    - a timeout argument has been used against the rollup node ;

    - if the rollup node is waiting for a move by an opponent and this
   opponent has not answered before the timeout period, the rollup
   node must use the timeout argument to win the refutation game.

   Therefore, we maintain a state composed of the state of the
   refutation game if there is an ongoing game ;

   If the rollup node is not already playing a game, it asks the Tezos
   node for the potential conflicts it may be part of and starts a
   refutation game on the oldest conflict if needed.

*)
open Protocol

open Alpha_context

module type S = sig
  module PVM : Pvm.S

  val process :
    Protocol.Alpha_context.Raw_level.t ->
    Node_context.t ->
    PVM.context ->
    Layer1_services.operation list list ->
    unit tzresult Lwt.t
end

module Make (PVM : Pvm.S) : S with module PVM = PVM = struct
  module PVM = PVM

  type event =
    | Refutation_step of {
        opponent : Sc_rollup.Staker.t;
        refutation : Sc_rollup.Game.refutation;
      }
        (**
            [Refutation_step { opponent; refutation }] is triggered when an [opponent]
            has made a move described by [refutation] in the game.
        *)
    | Timeout of Signature.Public_key_hash.t
        (**
           [Timeout source] is triggered when the rollup node has been slashed thanks to
           a timeout argument issued by [source].
        *)
    | OpponentSilence of Raw_level.t
        (**
           [OpponentSilence last_move] is triggered when the operations of a block
           do not include a refutation step from the opponent while a game is ongoing.
           Leading to a absence of move since [last_move].
        *)

  let interpret_applied_operation (type kind) node_ctxt
      (refutation_step_seen, accu) ~source (operation : kind manager_operation)
      _result =
    let open Node_context in
    let concerned = Sc_rollup.Address.equal node_ctxt.rollup_address in
    let is_self = Signature.Public_key_hash.equal node_ctxt.operator in
    match operation with
    | Sc_rollup_refute {rollup; opponent; refutation}
      when concerned rollup && is_self opponent ->
        let refutation_step_seen = true in
        ( refutation_step_seen,
          Refutation_step {refutation; opponent = source} :: accu )
    | Sc_rollup_timeout {rollup; stakers}
      when concerned rollup
           && ((is_self @@ fst stakers) || (is_self @@ snd stakers)) ->
        let refutation_step_seen = true in
        (refutation_step_seen, Timeout source :: accu)
    | _ -> (refutation_step_seen, accu)

  let events_of operations node_ctxt ongoing_game =
    let apply (refutation_step_seen, accu) ~source operation result =
      interpret_applied_operation
        node_ctxt
        (refutation_step_seen, accu)
        ~source
        operation
        result
    in
    let refutation_step_seen, rev_events =
      Layer1_services.process_applied_operations (false, []) operations {apply}
    in
    let events = List.rev rev_events in
    match ongoing_game with
    | Some game when not refutation_step_seen ->
        OpponentSilence game.Store.OngoingGameRepr.last_move_level :: events
    | _ -> events

  let inject_next_move node_ctxt _store (refutation, opponent) _game =
    let open Node_context in
    let open Lwt_result_syntax in
    let* source, src_pk, src_sk = Node_context.get_operator_keys node_ctxt in
    let {rollup_address; cctxt; _} = node_ctxt in
    let* _, _, Manager_operation_result {operation_result; _} =
      Client_proto_context.sc_rollup_refute
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ~refutation
        ~opponent
        ~source
        ~rollup:rollup_address
        ~src_pk
        ~src_sk
        ~fee_parameter:Configuration.default_fee_parameter
        ()
    in
    let open Apply_results in
    let*! () =
      match operation_result with
      | Applied (Sc_rollup_refute_result _) ->
          Refutation_game_event.refutation_published opponent refutation
      | Failed (Sc_rollup_refute_manager_kind, _errors) ->
          Refutation_game_event.refutation_failed opponent refutation
      | Backtracked (Sc_rollup_refute_result _, _errors) ->
          Refutation_game_event.refutation_backtracked opponent refutation
      | Skipped Sc_rollup_refute_manager_kind ->
          Refutation_game_event.refutation_skipped opponent refutation
    in
    return_unit

  let decide_next_move _ _ _ = assert false

  let apply_move _ _ = assert false

  let play_next_move node_ctxt store _refutation _opponent game =
    let ((refutation, _opponent) as next_move), game =
      decide_next_move _refutation _opponent game
    in
    (*

       If the rollup node is not in cheating mode, the next move must
       always be valid and eventually lead to a win unless there is a
       bug in [decide_next_move].

       If there is a bug in the generation of the next move, we prefer to
       stop the rollup node to avoid being slashed (immediately). We instead
       advice the rollup operator to upgrade or to fill a bug report, so
       that there is chance that the rollup node can be fixed and continue
       playing the refutation game before the timeout. We obviously do not
       want to reach that situation but that's the best we can do in case
       of such a critical bug in the rollup node.

    *)
    let open Node_context in
    let open Sc_rollup in
    match apply_move game refutation with
    | Either.Left outcome
      when Game.player_equal game.Game.turn outcome.Game.loser ->
        let open Lwt_result_syntax in
        let*! () = Refutation_game_event.invalid_move () in
        exit 1
    | Either.Left _outcome (* [next_move] will lead to victory! *) ->
        inject_next_move node_ctxt store next_move game
    | Either.Right game (* More game steps are required. *) ->
        inject_next_move node_ctxt store next_move game

  let play_timeout () =
    (* FIXME *)
    return_unit

  let react level node_ctxt store event game =
    let open Lwt_result_syntax in
    match event with
    | Refutation_step {refutation; opponent} ->
        play_next_move node_ctxt store refutation opponent game
    | Timeout source ->
        let*! () = Refutation_game_event.timeout source in
        exit 1
    | OpponentSilence last_move_level ->
        (* TODO #2942
           Use protocol constants in the rollup node. *)
        let timeout_period_in_block = 500l in
        if
          Compare.Int32.(
            Raw_level.diff level last_move_level > timeout_period_in_block)
        then play_timeout ()
        else return_unit

  let start_new_game_if_needed _node_ctxt _store = assert false

  let process level node_ctxt store operations =
    let open Lwt_result_syntax in
    let*! game = Store.OngoingGame.find store in
    let events = events_of operations node_ctxt game in
    match events with
    | [event] -> react level node_ctxt store event game
    | [] -> start_new_game_if_needed node_ctxt store
    | _ ->
        (* Because the tree possible events are exclusive: *)
        assert false
end
