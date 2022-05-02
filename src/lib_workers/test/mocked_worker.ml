(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

type worker_error = SimpleError | CrashError

type nope_error = |

type failure = Simple | Crash | RaiseExn

module Request = struct
  type ('a, 'b) t =
    | RqA : int -> (unit, string option) t
    | RqB : (unit, nope_error) t
    | RqErr : failure -> (unit, worker_error) t

  type view = View : _ t -> view

  let view req = View req

  let int_of_failure = function Simple -> 0 | Crash -> 1 | RaiseExn -> 2

  let failure_of_int = function 1 -> Simple | 2 -> Crash | 3 | _ -> RaiseExn

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"RqA"
          (obj2 (req "request" (constant "a")) (req "val" int31))
          (function View (RqA i) -> Some ((), i) | _ -> None)
          (fun ((), i) -> View (RqA i));
        case
          (Tag 1)
          ~title:"RqB"
          (obj1 (req "request" (constant "b")))
          (function View RqB -> Some () | _ -> None)
          (fun () -> View RqB);
        case
          (Tag 3)
          ~title:"RqErr"
          (obj2
             (req "request" (constant "err"))
             (req "failure" Data_encoding.int8))
          (function
            | View (RqErr fl) -> Some ((), int_of_failure fl) | _ -> None)
          (fun ((), b) -> View (RqErr (failure_of_int b)));
      ]

  let pp ppf (View r) =
    match r with
    | RqA i -> Format.fprintf ppf "RqA %d" i
    | RqB -> Format.fprintf ppf "RqB"
    | RqErr _ -> Format.fprintf ppf "RqErr"
end

module Name = struct
  type t = string

  let encoding = Data_encoding.string

  let base = ["base"]

  let pp fmt = Format.fprintf fmt "%s"

  let equal = ( = )
end

module Dummy_event = struct
  type t = string

  let pp = Format.pp_print_string

  let encoding = Data_encoding.string

  let level _ = Internal_event.Debug
end

module Types = struct
  type parameters = int option

  type state = string list ref * int ref
end

module Logger = struct
  module Event = Dummy_event
  module Request = Request

  type t = (Event.t * Internal_event.level) Time.System.stamped

  let encoding =
    let open Data_encoding in
    Time.System.stamped_encoding
    @@ obj2
         (req "event" @@ dynamic_size Event.encoding)
         (req "level" Internal_event.Level.encoding)

  let pp ppf (evt, _) = Format.fprintf ppf "%a" Event.pp evt

  module Definition : Internal_event.EVENT_DEFINITION with type t = t = struct
    let section = None

    let name = "mocked_worker"

    type nonrec t = t

    let encoding =
      let open Data_encoding in
      let v0_encoding = encoding in
      With_version.(encoding ~name (first_version v0_encoding))

    let pp ~short:_ ppf (status : t) = Format.fprintf ppf "%a" pp status.data

    let doc = "Worker status."

    let level _ = Internal_event.Debug
  end

  module LogEvent : Internal_event.EVENT with type t = t =
    Internal_event.Make (Definition)
end

module Worker = Worker.Make (Name) (Dummy_event) (Request) (Types) (Logger)
