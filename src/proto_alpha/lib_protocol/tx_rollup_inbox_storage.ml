(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

type error +=
  | Tx_rollup_inbox_does_not_exist of Tx_rollup_repr.t * Raw_level_repr.t

let append_message :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Tx_rollup_message_repr.t ->
    (int * Raw_context.t) tzresult Lwt.t =
 fun ctxt rollup message ->
  let level = (Raw_context.current_level ctxt).level in
  Storage.Tx_rollup.Inbox_cumulated_size.find (ctxt, level) rollup
  >>=? fun (ctxt, msize) ->
  let message_size = Tx_rollup_message_repr.size message in
  let new_size = Option.value ~default:0 msize + message_size in
  Storage.Tx_rollup.Inbox_rev_contents.find (ctxt, level) rollup
  >>=? fun (ctxt, mcontents) ->
  Storage.Tx_rollup.Inbox_rev_contents.add
    (ctxt, level)
    rollup
    (Tx_rollup_message_repr.hash message :: Option.value ~default:[] mcontents)
  >>=? fun (ctxt, _, _) ->
  Storage.Tx_rollup.Inbox_cumulated_size.add (ctxt, level) rollup new_size
  >>=? fun (ctxt, _, _) -> return (message_size, ctxt)

let get_level :
    Raw_context.t -> [`Current | `Level of Raw_level_repr.t] -> Raw_level_repr.t
    =
 fun ctxt -> function
  | `Current -> (Raw_context.current_level ctxt).level
  | `Level lvl -> lvl

let messages_opt :
    Raw_context.t ->
    level:[`Current | `Level of Raw_level_repr.t] ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_message_repr.hash list option) tzresult Lwt.t =
 fun ctxt ~level tx_rollup ->
  let level = get_level ctxt level in
  Storage.Tx_rollup.Inbox_rev_contents.find (ctxt, level) tx_rollup
  >>=? function
  | (ctxt, Some rev_contents) -> return (ctxt, Some (List.rev rev_contents))
  | (ctxt, None) ->
      (*
        Prior to returning [None], we check whether or not the
        transaction rollup address is valid, to raise the appropriate
        if need be.
       *)
      Tx_rollup_state_storage.assert_exist ctxt tx_rollup >>=? fun ctxt ->
      return (ctxt, None)

let messages :
    Raw_context.t ->
    level:[`Current | `Level of Raw_level_repr.t] ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_message_repr.hash list) tzresult Lwt.t =
 fun ctxt ~level tx_rollup ->
  messages_opt ctxt ~level tx_rollup >>=? function
  | (ctxt, Some messages) -> return (ctxt, messages)
  | (_, None) ->
      fail (Tx_rollup_inbox_does_not_exist (tx_rollup, get_level ctxt level))

let size :
    Raw_context.t ->
    level:[`Current | `Level of Raw_level_repr.t] ->
    Tx_rollup_repr.t ->
    (Raw_context.t * int) tzresult Lwt.t =
 fun ctxt ~level tx_rollup ->
  let level = get_level ctxt level in
  Storage.Tx_rollup.Inbox_cumulated_size.find (ctxt, level) tx_rollup
  >>=? function
  | (ctxt, Some cumulated_size) -> return (ctxt, cumulated_size)
  | (ctxt, None) ->
      (*
        Prior to raising an error related to the missing inbox, we
        check whether or not the transaction rollup address is valid,
        to raise the appropriate if need be.
       *)
      Tx_rollup_state_storage.assert_exist ctxt tx_rollup >>=? fun _ctxt ->
      fail (Tx_rollup_inbox_does_not_exist (tx_rollup, level))

let find :
    Raw_context.t ->
    level:[`Current | `Level of Raw_level_repr.t] ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_inbox_repr.t option) tzresult Lwt.t =
 fun ctxt ~level tx_rollup ->
  let open Tx_rollup_inbox_repr in
  (*
    [messages_opt] checks whether or not [tx_rollup] is valid, so
    we do not have to do it here.
   *)
  messages_opt ctxt ~level tx_rollup >>=? function
  | (ctxt, Some contents) ->
      size ctxt ~level tx_rollup >>=? fun (ctxt, cumulated_size) ->
      return (ctxt, Some {cumulated_size; contents})
  | (ctxt, None) -> return (ctxt, None)

let get :
    Raw_context.t ->
    level:[`Current | `Level of Raw_level_repr.t] ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_inbox_repr.t) tzresult Lwt.t =
 fun ctxt ~level tx_rollup ->
  (*
    [inbox_opt] checks whether or not [tx_rollup] is valid, so we
    don’t have to do it here.
   *)
  find ctxt ~level tx_rollup >>=? function
  | (ctxt, Some res) -> return (ctxt, res)
  | (_, None) ->
      fail (Tx_rollup_inbox_does_not_exist (tx_rollup, get_level ctxt level))

(* Error registration *)

let () =
  let open Data_encoding in
  (* Tx_rollup_inbox_does_not_exist *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_inbox_does_not_exist"
    ~title:"Missing transaction rollup inbox"
    ~description:"The transaction rollup does not have an inbox at this level"
    ~pp:(fun ppf (addr, level) ->
      Format.fprintf
        ppf
        "Transaction rollup %a does not have an inbox at level %a"
        Tx_rollup_repr.pp
        addr
        Raw_level_repr.pp
        level)
    (obj2
       (req "tx_rollup_address" Tx_rollup_repr.encoding)
       (req "raw_level" Raw_level_repr.encoding))
    (function
      | Tx_rollup_inbox_does_not_exist (rollup, level) -> Some (rollup, level)
      | _ -> None)
    (fun (rollup, level) -> Tx_rollup_inbox_does_not_exist (rollup, level))
