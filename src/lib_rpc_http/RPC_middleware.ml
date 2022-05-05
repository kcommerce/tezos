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
let transform_callback callback conn req body =
  let open Lwt_syntax in
  let* answer = callback conn req body in
  let open Cohttp in
  let uri = Request.uri req in
  let answer_has_not_found_status = function
    | `Expert (response, _) | `Response (response, _) ->
        Cohttp.Response.status response = `Not_found
  in
  if answer_has_not_found_status answer then
    let overriding = "http://localhost:18731" ^ Uri.path uri in
    let headers = Cohttp.Header.of_list [("location", overriding)] in
    let response =
      Cohttp.Response.make ~status:`Moved_permanently ~headers ()
    in
    Lwt.return (`Response (response, Cohttp_lwt.Body.empty))
  else Lwt.return answer

let rpc_middleware = Resto_cohttp_server.Server.{transform_callback}
