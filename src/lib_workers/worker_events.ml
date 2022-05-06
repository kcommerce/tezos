(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type S = sig
  type view

  type critical_error

  val request : (view * Worker_types.request_status) Internal_event.Simple.t

  val terminated : unit Internal_event.Simple.t

  val triggering_shutdown : unit Internal_event.Simple.t

  val crashed : critical_error Internal_event.Simple.t

  val started : unit Internal_event.Simple.t

  val started_for : string Internal_event.Simple.t

  val emit : 'a Internal_event.Simple.t -> 'a -> unit Lwt.t
end

module type CRITICAL_ERROR = sig
  type t

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end

module Make
    (N : Worker_intf.NAME)
    (R : Worker_intf.REQUEST)
    (C : CRITICAL_ERROR) =
struct
  include Internal_event.Simple

  type view = R.view

  type critical_error = C.t

  let base_name = String.concat "-" N.base

  let section = N.base

  let request_error =
    declare_3
      ~section
      ~name:("request_" ^ base_name)
      ~msg:"{view} {request_status} {errors}"
      ~level:Notice
      ~pp1:R.pp
      ~pp2:Worker_types.pp_status
      ~pp3:C.pp
      ("view", R.encoding)
      ("request_status", Worker_types.request_status_encoding)
      ("errors", C.encoding)

  let request_no_errors =
    declare_2
      ~section
      ~name:("request_no_errors" ^ base_name)
      ~msg:"{view} {request_status}"
      ~level:Notice
      ~pp1:R.pp
      ~pp2:Worker_types.pp_status
      ("view", R.encoding)
      ("request_status", Worker_types.request_status_encoding)

  let terminated =
    declare_0
      ~section
      ~name:("terminate_" ^ base_name)
      ~msg:(Format.asprintf "worker terminated [%s]" base_name)
      ~level:Info
      ()

  let triggering_shutdown =
    declare_0
      ~section
      ~name:("triggering_" ^ base_name)
      ~msg:"triggering shutting down"
      ~level:Debug
      ()

  let crashed =
    declare_1
      ~section
      ~name:("crashed_" ^ base_name)
      ~msg:(Format.asprintf "worker crashed [%s]: {error}" base_name)
      ~level:Error
      ~pp1:C.pp
      ("error", C.encoding)

  let started =
    declare_0
      ~section
      ~name:("started_" ^ base_name)
      ~msg:"worker started"
      ~level:Info
      ()

  let started_for =
    declare_1
      ~section
      ~name:("started_for_" ^ base_name)
      ~msg:"worker started for {name}"
      ~level:Info
      ("name", Data_encoding.string)
end
