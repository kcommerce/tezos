(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

let read_chain_id validator chain =
  let open Lwt_syntax in
  let distributed_db = Validator.distributed_db validator in
  let store = Distributed_db.store distributed_db in
  match chain with
  | None -> Lwt.return_none
  | Some chain ->
      let* v = Chain_directory.get_chain_id store chain in
      Lwt.return_some v

let inject_block validator ?force ?chain bytes operations =
  let open Lwt_result_syntax in
  let*! chain_id = read_chain_id validator chain in
  let* (hash, block) =
    Validator.validate_block validator ?force ?chain_id bytes operations
  in
  return
    ( hash,
      let* _ = block in
      return_unit )

(* While injecting several operations, if one injection failed, we
   have to report the error. To avoid using recursive errors we do the
   reporting as follows:

   - We wrap the error into [Injection_operations_error]

   - If injecting the operation [oph] succeeded we use
   [Injection_operation_succeed_case oph]

   - If injecting the operation [oph] failed with [err], we use
   [Injection_operation_error_case oph] followed by [err].    
*)
type Error_monad.error += Injection_operations_error

type Error_monad.error += Injection_operation_succeed_case of Operation_hash.t

type Error_monad.error += Injection_operation_error_case of Operation_hash.t

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"injection_operations_error"
    ~title:"Injection operations error"
    ~description:
      "While injecting several operations at once, one or several injections \
       failed."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "While injecting several operations, one or several injections failed. \
         Errors are the one below in the trace.")
    unit
    (function Injection_operations_error -> Some () | _ -> None)
    (function () -> Injection_operations_error) ;
  register_error_kind
    `Permanent
    ~id:"injection_operation_succeed"
    ~title:"Injection operation succeed"
    ~description:
      "The injection of this operation succeed among a list of injections \
       containing at least one error."
    ~pp:(fun ppf oph ->
      Format.fprintf ppf "Injection of %a succeeded." Operation_hash.pp oph)
    (obj1 (req "oph" Operation_hash.encoding))
    (function Injection_operation_succeed_case oph -> Some oph | _ -> None)
    (function oph -> Injection_operation_succeed_case oph) ;
  register_error_kind
    `Permanent
    ~id:"injection_operation_error"
    ~title:"Injection operation error"
    ~description:
      "The injection of this operation failed. Error is after in the list"
    ~pp:(fun ppf oph ->
      Format.fprintf
        ppf
        "Injection of %a failed. Error is next."
        Operation_hash.pp
        oph)
    (obj1 (req "oph" Operation_hash.encoding))
    (function Injection_operation_error_case oph -> Some oph | _ -> None)
    (function oph -> Injection_operation_error_case oph)

let inject_operation validator ~force ?chain bytes =
  let open Lwt_result_syntax in
  let*! chain_id = read_chain_id validator chain in
  let t =
    match Data_encoding.Binary.of_bytes_opt Operation.encoding bytes with
    | None -> failwith "Can't parse the operation"
    | Some op -> Validator.inject_operation validator ~force ?chain_id op
  in
  let hash = Operation_hash.hash_bytes [bytes] in
  Lwt.return (hash, t)

let inject_operations validator ~force ?chain bytes_list =
  let open Lwt_syntax in
  let* (hashes, promises) =
    List.fold_left_s
      (fun (hashes, promises) bytes ->
        let* (hash, promise) = inject_operation validator ~force ?chain bytes in
        return (hash :: hashes, promise :: promises))
      ([], [])
      bytes_list
  in
  let hashes_array = Array.of_list hashes in
  let fold_errors (has_failed, result, n) promise_result =
    let oph = Array.get hashes_array n in
    match promise_result with
    | Ok () ->
        (has_failed, Injection_operation_succeed_case oph :: result, n + 1)
    | Error trace ->
        (* The list will be reversed. *)
        (true, trace @ Injection_operation_error_case oph :: result, n + 1)
  in
  let join_results l =
    let (has_failed, result, _) = List.fold_left fold_errors (false, [], 0) l in
    if not has_failed then Ok ()
    else Result_syntax.fail (Injection_operations_error :: List.rev result)
  in
  let t = Lwt.map join_results (Lwt.all promises) in
  return (hashes, t)

let inject_protocol store proto =
  let open Lwt_result_syntax in
  let proto_bytes = Data_encoding.Binary.to_bytes_exn Protocol.encoding proto in
  let hash = Protocol_hash.hash_bytes [proto_bytes] in
  let validation =
    let*! b = Updater.compile hash proto in
    match b with
    | false -> failwith "Compilation failed (%a)" Protocol_hash.pp_short hash
    | true -> (
        let*! o = Store.Protocol.store store hash proto in
        match o with
        | None ->
            failwith
              "Previously registered protocol (%a)"
              Protocol_hash.pp_short
              hash
        | Some _ -> return_unit)
  in
  Lwt.return (hash, validation)

let build_rpc_directory validator =
  let open Lwt_result_syntax in
  let distributed_db = Validator.distributed_db validator in
  let state = Distributed_db.store distributed_db in
  let dir : unit RPC_directory.t ref = ref RPC_directory.empty in
  let register0 s f =
    dir := RPC_directory.register !dir s (fun () p q -> f p q)
  in
  let inject_operation ~force q contents =
    let*! (hash, wait) =
      inject_operation validator ~force ?chain:q#chain contents
    in
    let* () = if q#async then return_unit else wait in
    return hash
  in
  let inject_operations q contents =
    let*! (hashes, wait) =
      inject_operations validator ~force:q#force ?chain:q#chain contents
    in
    let* () = if q#async then return_unit else wait in
    return hashes
  in
  register0 Injection_services.S.block (fun q (raw, operations) ->
      let* (hash, wait) =
        inject_block validator ?chain:q#chain ~force:q#force raw operations
      in
      let* () = if q#async then return_unit else wait in
      return hash) ;
  register0 Injection_services.S.operation (inject_operation ~force:false) ;
  register0
    Injection_services.S.private_operation
    (inject_operation ~force:true) ;
  register0 Injection_services.S.private_operations inject_operations ;
  register0 Injection_services.S.protocol (fun q protocol ->
      let*! (hash, wait) = inject_protocol state protocol in
      let* () = if q#async then return_unit else wait in
      return hash) ;
  !dir
