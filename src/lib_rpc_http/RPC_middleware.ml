(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022                                                  FIXME *)
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

(* FIXME very hacky, proof-of-concept, etc, etc *)
let transform_callback ~callback ~conn ~req ~body =
  let open Lwt_syntax in
  let* answer = callback conn req body in
  let open Cohttp in
  let uri = Request.uri req in
  let answer_has_not_found_status = function
    | `Expert (response, _) | `Response (response, _) ->
        Cohttp.Response.status response = `Not_found
  in
  let answer_to_string = function
    | `Expert (response, _) | `Response (response, _) ->
        Format.asprintf "response:\n  %a" Cohttp.Response.pp_hum response
  in
  let () = Stdlib.print_endline @@ answer_to_string answer in
  if answer_has_not_found_status answer then
    let () = Stdlib.print_endline @@ "Error Not_found, overriding location" in
    let existing = Uri.to_string uri in
    Stdlib.print_endline @@ "existing: " ^ existing;
    Stdlib.print_endline @@ "host: " ^ (Uri.host uri |> Stdlib.Option.get);
    Stdlib.print_endline @@ "path: " ^ Uri.path uri;
    let overriding = "http://localhost:18731" ^ Uri.path uri in
    let headers = Cohttp.Header.of_list [("location", overriding)] in
    Stdlib.print_endline ("Overriding with: " ^ overriding);
    let status = `Moved_permanently in
    let response = Cohttp.Response.make ~status ~headers () in
    Lwt.return (`Response (response, Cohttp_lwt.Body.empty))
  else
    Lwt.return answer
