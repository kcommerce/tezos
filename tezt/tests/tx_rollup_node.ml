(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Tx_rollup_node
   Invocation:   dune exec tezt/tests/main.exe -- --file tx_rollup_node.ml
   Subject:      Various test scenarios for the Tx rollup node
*)

module Rollup = Rollup.Tx_rollup
module Rollup_node = Rollup_node.Tx_node

let get_block_hash block_json = JSON.(block_json |-> "hash" |> as_string)

let node_rpc node service =
  let* opt_get = RPC.Curl.get () in
  match opt_get with
  | None -> return None
  | Some get ->
      let url = Format.asprintf "%s/%s" (Rollup_node.endpoint node) service in
      let* result = get ~url in
      return (Some result)

(* This function computes an inbox from the messages stored by the
   rollup node.  The way the inbox is computed is to use helpers
   exposed by the protocol. *)
let get_node_inbox ?(block = "head") node client =
  let* json = node_rpc node @@ "block/" ^ block ^ "/inbox" in
  match json with
  | None ->
      return Rollup.{inbox_length = 0; cumulated_size = 0; merkle_root = ""}
  | Some json ->
      let parse_message json =
        if JSON.(is_null (json |-> "batch")) then
          Test.fail "This case is not handled yet"
        else JSON.(json |-> "batch" |> as_string |> fun x -> `Batch (`Hex x))
      in
      let messages =
        JSON.(
          json |-> "contents" |> as_list
          |> List.map (fun x -> x |-> "message" |> parse_message))
      in
      Rollup.compute_inbox_from_messages messages client

let get_rollup_parameter_file ~protocol =
  let enable_tx_rollup = [(["tx_rollup_enable"], Some "true")] in
  let base = Either.right (protocol, None) in
  Protocol.write_parameter_file ~base enable_tx_rollup

(* Wait for the [injection_success] event from the rollup node batcher. *)
let wait_for_injection_success_event node =
  Rollup_node.wait_for node "injection_success.v0" (fun _ -> Some ())

(* Check that all messages in the inbox have been successfully applied. *)
let check_inbox_success (inbox : Rollup_node.Inbox.t) =
  let ( |->? ) json field =
    let res = JSON.(json |-> field) in
    match JSON.unannotate res with `Null -> None | _ -> Some res
  in
  List.iteri
    (fun i msg ->
      let result =
        (* Pair of result and withdraws *)
        JSON.(msg.Rollup_node.Inbox.result |=> 0)
      in
      match result |->? "deposit_result" with
      | None ->
          (* Not a deposit, must be a batch *)
          let results =
            JSON.(
              result |->? "batch_v1_result" |> Option.get |-> "results"
              |> as_list)
          in
          List.iteri
            (fun j tr_json ->
              match JSON.(tr_json |=> 1 |> as_string_opt) with
              | Some "transaction_success" -> (* OK *) ()
              | _ ->
                  Test.fail
                    "Transaction at position %d of batch %d failed: %s"
                    j
                    i
                    (JSON.encode tr_json))
            results
      | Some result -> (
          match result |->? "deposit_success" with
          | Some _ -> (* OK *) ()
          | None ->
              Test.fail
                "Deposit at position %d failed: %s"
                i
                (JSON.encode result)))
    inbox.contents

(* Checks that the configuration is stored and that the  required
   fields are present. *)
let test_node_configuration =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: configuration"
    ~tags:["tx_rollup"; "configuration"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      (* Originate a rollup with a given operator *)
      let*! tx_rollup_hash = Client.Tx_rollup.originate ~src:operator client in
      let* block_hash = RPC.get_block_hash client in
      let tx_rollup_node =
        Rollup_node.create
          ~rollup_id:tx_rollup_hash
          ~rollup_genesis:block_hash
          ~operator
          client
          node
      in
      let* filename =
        Rollup_node.config_init tx_rollup_node tx_rollup_hash block_hash
      in
      Log.info "Tx_rollup configuration file was successfully created" ;
      let () =
        let open Ezjsonm in
        let req = ["operator"; "rollup_id"; "rpc_addr"] in
        (* TODO: add optional args checks *)
        match from_channel @@ open_in filename with
        | `O fields ->
            List.iter
              (fun k ->
                if List.exists (fun (key, _v) -> String.equal k key) fields then
                  ()
                else Test.fail "unexpected configuration field")
              req
        | _ -> Test.fail "Unexpected configuration format"
      in
      unit)

let init_and_run_rollup_node ~operator node client =
  let*! tx_rollup_hash = Client.Tx_rollup.originate ~src:operator client in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node 2 in
  Log.info "Tx_rollup %s was successfully originated" tx_rollup_hash ;
  let* block_hash = RPC.get_block_hash client in
  let tx_node =
    Rollup_node.create
      ~rollup_id:tx_rollup_hash
      ~rollup_genesis:block_hash
      ~operator
      client
      node
  in
  let* _ = Rollup_node.config_init tx_node tx_rollup_hash block_hash in
  let* () = Rollup_node.run tx_node in
  Log.info "Tx_rollup node is now running" ;
  let* () = Rollup_node.wait_for_ready tx_node in
  Lwt.return (tx_rollup_hash, tx_node)

(* Checks that the tx_node is ready after originating an associated
   rollup key. *)
let test_tx_node_origination =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: test if the node is ready"
    ~tags:["tx_rollup"; "ready"; "originate"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* _tx_node = init_and_run_rollup_node ~operator node client in
      unit)

(* Checks that an inbox received by the tx_rollup node is well stored
   and available in a percistent way. *)
let test_tx_node_store_inbox =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: store inbox"
    ~tags:["tx_rollup"; "store"; "inbox"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let*! rollup = Client.Tx_rollup.originate ~src:operator client in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 2 in
      let* block_hash = RPC.get_block_hash client in
      let tx_node =
        Rollup_node.create
          ~rollup_id:rollup
          ~rollup_genesis:block_hash
          ~operator
          client
          node
      in
      let* _ = Rollup_node.config_init tx_node rollup block_hash in
      let* () = Rollup_node.run tx_node in
      (* Submit a batch *)
      let (`Batch content) = Rollup.make_batch "tezos_l2_batch_1" in
      let*! () =
        Client.Tx_rollup.submit_batch ~content ~rollup ~src:operator client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 3 in
      let* node_inbox_1 = get_node_inbox ~block:"0" tx_node client in
      let*! expected_inbox_1 = Rollup.get_inbox ~rollup ~level:0 client in
      (* Ensure that stored inboxes on daemon's side are equivalent of
         inboxes returned by the rpc call. *)
      Check.(Some node_inbox_1 = expected_inbox_1)
        (Check.option Rollup.Check.inbox)
        ~error_msg:
          "Unexpected inbox computed from the rollup node. Expected %R. \
           Computed %L" ;
      let (`Batch content) = Rollup.make_batch "tezos_l2_batch_2" in
      let*! () =
        Client.Tx_rollup.submit_batch ~content ~rollup ~src:operator client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 4 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 4 in
      let* node_inbox_2 = get_node_inbox ~block:"1" tx_node client in
      let*! expected_inbox_2 = Rollup.get_inbox ~rollup ~level:1 client in
      (* Ensure that stored inboxes on daemon side are equivalent of inboxes
         returned by the rpc call. *)
      Check.(Some node_inbox_2 = expected_inbox_2)
        (Check.option Rollup.Check.inbox)
        ~error_msg:
          "Unexpected inbox computed from the rollup node. Expected %R. \
           Computed %L" ;
      (* Stop the node and try to get the inbox once again*)
      let* () = Rollup_node.terminate tx_node in
      let* () = Rollup_node.run tx_node in
      let* () = Rollup_node.wait_for_ready tx_node in
      let*! inbox_after_restart = Rollup.get_inbox ~rollup ~level:1 client in
      Check.(Some node_inbox_2 = inbox_after_restart)
        (Check.option Rollup.Check.inbox)
        ~error_msg:
          "Unexpected inbox computed from the rollup node. Expected %R. \
           Computed %L" ;
      unit)

let raw_tx_node_rpc_post node ~url data =
  let* rpc = RPC.Curl.post () in
  match rpc with
  | None -> assert false
  | Some curl ->
      let url = Printf.sprintf "%s/%s" (Rollup_node.rpc_addr node) url in
      curl ~url data

let tx_client_inject_transaction ~tx_node ?failswith transaction signature =
  let open Tezos_protocol_alpha.Protocol in
  let signed_tx_json =
    JSON.annotate ~origin:"signed_l2_transaction"
    @@ `O
         [
           ( "transaction",
             Data_encoding.Json.construct
               Tx_rollup_l2_batch.V1.transaction_encoding
               transaction );
           ( "signature",
             Data_encoding.Json.construct
               Tx_rollup_l2_context_sig.signature_encoding
               signature );
         ]
  in
  let* json =
    raw_tx_node_rpc_post
      tx_node
      ~url:"queue/injection/transaction"
      signed_tx_json
  in
  match failswith with
  | None -> (
      try return (JSON.as_string json)
      with _ ->
        Test.fail "Transaction injection failed with: %s" (JSON.encode json))
  | Some expected_id ->
      let error_id = JSON.(json |> as_list |> List.hd |-> "id" |> as_string) in
      Check.((error_id = expected_id) string)
        ~error_msg:"Injection should have failed with %R but failed with %L" ;
      (* Dummy value for operation hash *)
      return ""

(* Returns the ticket hash, if any, of a given operation. *)
let get_ticket_hash_from_op op =
  let metadata = JSON.(op |-> "contents" |=> 0 |-> "metadata") in
  let result =
    JSON.(metadata |-> "internal_operation_results" |=> 0 |-> "result")
  in
  let kind =
    JSON.(
      metadata |-> "internal_operation_results" |=> 0 |-> "parameters"
      |-> "entrypoint" |> as_string)
  in
  let deposit = "deposit" in
  if not String.(equal kind deposit) then
    Test.fail
      "The internal operation was expected to be a %s but is a %s"
      deposit
      kind ;
  let status = JSON.(result |-> "status" |> as_string_opt) in
  match status with
  | Some v when String.(equal v "applied") ->
      JSON.(result |-> "ticket_hash" |> as_string)
  | None | Some _ -> Test.fail "The contract origination failed"

let get_ticket_hash_from_deposit (d : Rollup_node.Inbox.message) : string =
  JSON.(d.message |-> "deposit" |-> "ticket_hash" |> as_string)

(* The contract is expecting a parameter of the form:
   (Pair tx_rollup_txr1_address tx_rollup_tz4_address) *)
let make_tx_rollup_deposit_argument txr1 tz4 =
  "( Pair " ^ "\"" ^ txr1 ^ "\" \"" ^ tz4 ^ "\")"

type commitment_info = {
  tx_level : int;
  roots : string list;
  context_hashes : string list;
  inbox_merkle_root : string;
  predecessor : string option;
}

(** Build a {!commitment_info} for the current inbox but do not inject the
    operation.

    Note that the field [commitment_info.context_hashes] is not used
    in the commitment but is necessary for subsequent rejections. *)
let build_commitment_info ~tx_level ~tx_rollup_hash ~tx_node ~client =
  let*! inbox_opt =
    Rollup.get_inbox ~rollup:tx_rollup_hash ~level:tx_level client
  in
  let inbox = match inbox_opt with Some x -> x | None -> failwith "foo" in
  let inbox_merkle_root = inbox.merkle_root in
  let* rollup_inbox = Rollup_node.Client.get_inbox ~tx_node ~block:"head" in
  let context_hashes =
    List.map
      (fun x -> x.Rollup_node.Inbox.l2_context_hash.tree_hash)
      rollup_inbox.contents
  in
  let* roots =
    Lwt_list.map_p
      (fun context_hash ->
        let*! root =
          Rollup.message_result_hash
            ~context_hash
            ~withdraw_list_hash:Constant.tx_rollup_empty_withdraw_list
            client
        in
        return root)
      context_hashes
  in
  let* predecessor =
    if tx_level > 0 then
      let*! prev_commitment_opt =
        Rollup.get_commitment
          ~rollup:tx_rollup_hash
          ~level:(tx_level - 1)
          client
      in
      match prev_commitment_opt with
      | None -> failwith "foo"
      | Some x -> return (Some x.commitment_hash)
    else return None
  in
  return {tx_level; roots; context_hashes; predecessor; inbox_merkle_root}

type rejection_info = {
  proof : string;
  tx_level : int;
  message : string;
  position : int;
  path : string;
  rejected_message_result_hash : string;
  rejected_message_result_path : string;
  context_hash : string;
  withdraw_list_hash : string;
  agreed_message_result_path : string;
}

(** Build a rejection for the current inbox but do not inject the
    operation.

    Note that if [message_pos] is [0], we need information from the previous
    commitment: [agreed_context_hash] and [agreed_message_result_path]. If
    they are absent in this case, the function will fail.

    TODO/TORU: the withdraw_list_hash is currently always the empty list hash.
*)
let build_rejection ~tx_level ~tx_node ~message_pos ~client ?agreed_context_hash
    ?agreed_message_result_path commitment_info : rejection_info Lwt.t =
  let* rollup_inbox = Rollup_node.Client.get_inbox ~tx_node ~block:"head" in
  let* hashes =
    Lwt_list.map_p
      (fun content ->
        let message = content.Rollup_node.Inbox.message in
        let buffer = `Hex JSON.(message |-> "batch" |> as_string) in
        let*! message_hash =
          Rollup.message_hash ~message:(`Batch buffer) client
        in
        return message_hash)
      rollup_inbox.contents
  in
  let message =
    List.nth rollup_inbox.contents message_pos |> fun content ->
    JSON.encode content.message
  in
  let* message_path =
    let*! message_path =
      Rollup.inbox_merkle_tree_path
        ~message_hashes:hashes
        ~position:message_pos
        client
    in
    return (JSON.encode message_path)
  in
  let* (agreed_context_hash, agreed_message_result_path) =
    if message_pos = 0 && tx_level = 0 then
      return (Constant.tx_rollup_empty_l2_context, "[]")
    else if message_pos = 0 then
      (* If the message position is 0, we need information from the previous
         commitment. *)
      return
        (Option.get agreed_context_hash, Option.get agreed_message_result_path)
    else
      (* Else, we take the information from the previous message in the inbox. *)
      let agreed_context_hash =
        List.nth commitment_info.context_hashes (message_pos - 1)
      in
      let*! agreed_result_path =
        Rollup.commitment_merkle_tree_path
          ~message_result_hashes:
            (List.map (fun x -> `Hash x) commitment_info.roots)
          ~position:(message_pos - 1)
          client
      in
      return (agreed_context_hash, JSON.encode agreed_result_path)
  in
  let rejected_message_result_hash =
    List.nth commitment_info.roots message_pos
  in
  let* rejected_message_result_path =
    let*! rejected_message_result_path =
      Rollup.commitment_merkle_tree_path
        ~message_result_hashes:
          (List.map (fun x -> `Hash x) commitment_info.roots)
        ~position:message_pos
        client
    in
    return (JSON.encode rejected_message_result_path)
  in
  let* proof =
    let* proof =
      Rollup_node.Client.get_merkle_proof
        ~tx_node
        ~block:"head"
        ~message_pos:(string_of_int message_pos)
    in
    return (JSON.encode proof)
  in
  return
    {
      proof;
      tx_level;
      message;
      position = message_pos;
      path = message_path;
      rejected_message_result_hash;
      rejected_message_result_path;
      context_hash = agreed_context_hash;
      withdraw_list_hash = Constant.tx_rollup_empty_withdraw_list;
      agreed_message_result_path;
    }

(* FIXME/TORU: we should merge this into Tezos_crypto.Bls *)
module Bls_public_key_hash = struct
  include
    Tezos_crypto.Blake2B.Make
      (Tezos_crypto.Base58)
      (struct
        let name = "Bls12_381.Public_key_hash"

        let title = "A Bls12_381 public key hash"

        let b58check_prefix = "\006\161\166" (* tz4(36) *)

        let size = Some 20
      end)
end

let generate_bls_addr ?alias:_ _client =
  (* FIXME/TORU: we should use the the client to generate keys *)
  let seed =
    let rng_state = Random.State.make_self_init () in
    Bytes.init 32 (fun _ -> char_of_int @@ Random.State.int rng_state 255)
  in
  let sk = Bls12_381.Signature.generate_sk seed in
  let pk = Bls12_381.Signature.MinPk.derive_pk sk in
  let pkh =
    Bls_public_key_hash.hash_bytes [Bls12_381.Signature.MinPk.pk_to_bytes pk]
  in
  Log.info
    "A new BLS key was generated: %s"
    (Bls_public_key_hash.to_b58check pkh) ;
  (pkh, pk, sk)

let check_tz4_balance ~tx_node ~block ~ticket_id ~tz4_address ~expected_balance
    =
  let* tz4_balance =
    Rollup_node.Client.get_balance ~tx_node ~block ~ticket_id ~tz4_address
  in
  Check.(
    ( = )
      tz4_balance
      expected_balance
      int
      ~error_msg:
        (Format.sprintf
           "The balance of %s was expected to be %d instead of %d."
           tz4_address
           expected_balance
           tz4_balance)) ;
  unit

(* Checks that the a ticket can be transfered from the L1 to the rollup. *)
let test_ticket_deposit_from_l1_to_l2 =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: deposit ticket from l1 to l2"
    ~tags:["tx_rollup"; "deposit"; "ticket"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* (tx_rollup_hash, tx_node) =
        init_and_run_rollup_node ~operator node client
      in
      let* contract_id =
        Client.originate_contract
          ~alias:"rollup_deposit"
          ~amount:Tez.zero
          ~src:"bootstrap1"
          ~prg:"file:./tezt/tests/contracts/proto_alpha/tx_rollup_deposit.tz"
          ~init:"Unit"
          ~burn_cap:Tez.(of_int 1)
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      Log.info
        "The tx_rollup_deposit %s contract was successfully originated"
        contract_id ;
      let (bls_pkh, _, _) = generate_bls_addr client in
      let bls_pkh_str = Bls_public_key_hash.to_b58check bls_pkh in
      let arg = make_tx_rollup_deposit_argument tx_rollup_hash bls_pkh_str in
      (* This smart contract call will transfer 100_000 tickets to the
         given address. *)
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg
          client
      in
      let* () = Client.bake_for client in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 4 in
      (* Get the operation containing the ticket transfer. We assume
         that only one operation is issued in this block. *)
      let* inbox = Rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit (List.hd inbox.contents) in
      Log.info "Ticket %s was successfully emitted" ticket_id ;
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_str
          ~expected_balance:100_000
      in
      unit)

let sign_transaction sks txs =
  let open Tezos_protocol_alpha.Protocol in
  let buf =
    Data_encoding.Binary.to_bytes_exn
      Tx_rollup_l2_batch.V1.transaction_encoding
      txs
  in
  List.map (fun sk -> Bls12_381.Signature.MinPk.Aug.sign sk buf) sks

let sign_one_transaction sk txs =
  let open Tezos_protocol_alpha.Protocol in
  let buf =
    Data_encoding.Binary.to_bytes_exn
      Tx_rollup_l2_batch.V1.transaction_encoding
      txs
  in
  Bls12_381.Signature.MinPk.Aug.sign sk buf

let craft_tx ~counter ~signer ~dest ~ticket qty =
  let open Tezos_protocol_alpha.Protocol in
  let qty = Tx_rollup_l2_qty.of_int64_exn qty in
  let l2_addr = Tx_rollup_l2_address.of_b58check_exn dest in
  let destination = Indexable.from_value l2_addr in
  let ticket_hash =
    Indexable.from_value
      (Tezos_protocol_alpha.Protocol.Alpha_context.Ticket_hash.of_b58check_exn
         ticket)
  in
  let content =
    Tx_rollup_l2_batch.V1.Transfer {destination; ticket_hash; qty}
  in
  let signer = Indexable.from_value signer in
  Tx_rollup_l2_batch.V1.{signer; counter; contents = [content]}

let aggregate_signature_exn signatures =
  match Bls12_381.Signature.MinPk.aggregate_signature_opt signatures with
  | Some res -> res
  | None -> invalid_arg "aggregate_signature_exn"

let batch signatures contents =
  let open Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.V1 in
  let aggregated_signature = aggregate_signature_exn signatures in
  {aggregated_signature; contents}

let craft_batch
    (transactions :
      ( 'signer,
        'content )
      Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.V1.transaction
      list) sks =
  let signatures =
    List.map2 (fun txs sk -> sign_transaction sk txs) transactions sks
    |> List.concat
  in
  batch signatures transactions

(* Checks that the a ticket can be transfered within the rollup. *)
let test_l2_to_l2_transaction =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: l2 to l2 transaction"
    ~tags:["tx_rollup"; "rollup"; "internal"; "transaction"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* (tx_rollup_hash, tx_node) =
        init_and_run_rollup_node ~operator node client
      in
      let* contract_id =
        Client.originate_contract
          ~alias:"rollup_deposit"
          ~amount:Tez.zero
          ~src:"bootstrap1"
          ~prg:"file:./tezt/tests/contracts/proto_alpha/tx_rollup_deposit.tz"
          ~init:"Unit"
          ~burn_cap:Tez.(of_int 1)
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      Log.info
        "The tx_rollup_deposit %s contract was successfully originated"
        contract_id ;
      (* Generating some identities *)
      let (bls_1_pkh, bls_pk_1, bls_sk_1) = generate_bls_addr client in
      let bls_pkh_1_str = Bls_public_key_hash.to_b58check bls_1_pkh in
      (* FIXME/TORU: Use the client *)
      let (bls_2_pkh, bls_pk_2, bls_sk_2) = generate_bls_addr client in
      let bls_pkh_2_str = Bls_public_key_hash.to_b58check bls_2_pkh in
      let (bls_3_pkh, _, _) = generate_bls_addr client in
      let bls_pkh_3_str = Bls_public_key_hash.to_b58check bls_3_pkh in
      let arg_1 =
        make_tx_rollup_deposit_argument tx_rollup_hash bls_pkh_1_str
      in
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg:arg_1
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 4 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 4 in
      let* inbox = Rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit (List.hd inbox.contents) in
      Log.info "Ticket %s was successfully emitted" ticket_id ;
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:100_000
      in
      let arg_2 =
        make_tx_rollup_deposit_argument tx_rollup_hash bls_pkh_2_str
      in
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg:arg_2
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 5 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 5 in
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2_str
          ~expected_balance:100_000
      in
      Log.info "Crafting a l2 transaction" ;
      (* FIXME/TORU: Use the client *)
      let tx1 =
        craft_tx
          ~counter:1L
          ~signer:(Bls_pk bls_pk_1)
          ~dest:bls_pkh_2_str
          ~ticket:ticket_id
          5L
      in
      Log.info "Crafting a first batch" ;
      let batch = craft_batch [[tx1]] [[bls_sk_1]] in
      let content =
        Hex.of_string
          (Data_encoding.Binary.to_string_exn
             Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.encoding
             (Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.V1 batch))
      in
      Log.info "Submitting the first batch" ;
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content
          ~rollup:tx_rollup_hash
          ~src:operator
          client
      in
      Log.info "Crafting a second batch" ;
      let tx2 =
        craft_tx
          ~counter:1L
          ~signer:(Bls_pk bls_pk_2)
          ~dest:bls_pkh_3_str
          ~ticket:ticket_id
          15L
      in
      let batch = craft_batch [[tx2]] [[bls_sk_2]] in
      let content =
        Hex.of_string
          (Data_encoding.Binary.to_string_exn
             Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.encoding
             (Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.V1 batch))
      in
      Log.info "Submitting the second batch" ;
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content
          ~rollup:tx_rollup_hash
          ~src:Constant.bootstrap2.public_key_hash
          client
      in
      Log.info "Baking the batch" ;
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 6 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 6 in
      (* The decoding fails because of the buggy JSON encoding. This
         line can be uncommented once it is fixed.

         let* _node_inbox = get_node_inbox tx_node client in *)
      (* Having two batches in the same inbox we can test that:
         1. The batches are applied in the correct order
         2. The apply supports multiple batches
      *)
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:99_995
      and* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2_str
          ~expected_balance:99_990
      and* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_3_str
          ~expected_balance:15
      in
      unit)

(* Checks that the rollup node can receive L2 transactions in its queue, batch
   them and inject them in the Tezos node. *)
let test_batcher =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: L2 batcher"
    ~tags:["tx_rollup"; "node"; "batcher"; "transaction"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* (tx_rollup_hash, tx_node) =
        init_and_run_rollup_node ~operator node client
      in
      let* contract_id =
        Client.originate_contract
          ~alias:"rollup_deposit"
          ~amount:Tez.zero
          ~src:"bootstrap1"
          ~prg:"file:./tezt/tests/contracts/proto_alpha/tx_rollup_deposit.tz"
          ~init:"Unit"
          ~burn_cap:Tez.(of_int 1)
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      Log.info
        "The tx_rollup_deposit %s contract was successfully originated"
        contract_id ;
      (* Genarating some identities *)
      let (bls_1_pkh, bls_pk_1, bls_sk_1) = generate_bls_addr client in
      let bls_pkh_1_str = Bls_public_key_hash.to_b58check bls_1_pkh in
      (* FIXME/TORU: Use the client *)
      let (bls_2_pkh, bls_pk_2, bls_sk_2) = generate_bls_addr client in
      let bls_pkh_2_str = Bls_public_key_hash.to_b58check bls_2_pkh in
      let arg_1 =
        make_tx_rollup_deposit_argument tx_rollup_hash bls_pkh_1_str
      in
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg:arg_1
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 4 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 4 in
      let* inbox = Rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit (List.hd inbox.contents) in
      Log.info "Ticket %s was successfully emitted" ticket_id ;
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:100_000
      in
      let arg_2 =
        make_tx_rollup_deposit_argument tx_rollup_hash bls_pkh_2_str
      in
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg:arg_2
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 5 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 5 in
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2_str
          ~expected_balance:100_000
      in
      Log.info
        "Crafting a l2 transaction: %s transfers 1 to %s"
        bls_pkh_1_str
        bls_pkh_2_str ;
      (* FIXME/TORU: Use the client *)
      let tx =
        craft_tx
          ~counter:1L
          ~signer:(Bls_pk bls_pk_1)
          ~dest:bls_pkh_2_str
          ~ticket:ticket_id
          1L
      in
      let signature = sign_one_transaction bls_sk_1 [tx] in
      Log.info "Submitting the L2 transaction" ;
      let* txh1 = tx_client_inject_transaction ~tx_node [tx] signature in
      Log.info "Successfully submitted L2 transaction %s" txh1 ;
      Log.info
        "Crafting a l2 transaction: %s transfers 5 to %s"
        bls_pkh_2_str
        bls_pkh_1_str ;
      let tx =
        craft_tx
          ~counter:1L
          ~signer:(Bls_pk bls_pk_2)
          ~dest:bls_pkh_1_str
          ~ticket:ticket_id
          5L
      in
      let signature = sign_one_transaction bls_sk_2 [tx] in
      Log.info "Submitting the L2 transaction" ;
      let* txh2 = tx_client_inject_transaction ~tx_node [tx] signature in
      Log.info "Successfully submitted L2 transaction %s" txh2 ;

      Log.info "Crafting a l2 transaction with wrong counter" ;
      let tx =
        craft_tx
          ~counter:5L
          ~signer:(Bls_pk bls_pk_2)
          ~dest:bls_pkh_1_str
          ~ticket:ticket_id
          5L
      in
      let signature = sign_one_transaction bls_sk_2 [tx] in
      Log.info "Submitting the bad counter L2 transaction" ;
      let* _ =
        tx_client_inject_transaction
          ~tx_node
          [tx]
          ~failswith:"proto.alpha.tx_rollup_operation_counter_mismatch"
          signature
      in

      Log.info "Crafting a l2 transaction with wrong signature" ;
      let tx =
        craft_tx
          ~counter:2L
          ~signer:(Bls_pk bls_pk_1)
          ~dest:bls_pkh_2_str
          ~ticket:ticket_id
          1L
      in
      let signature = sign_one_transaction bls_sk_2 [tx] in
      Log.info "Submitting the bad signature L2 transaction" ;
      let* _ =
        tx_client_inject_transaction
          ~tx_node
          [tx]
          ~failswith:"proto.alpha.tx_rollup_incorrect_aggregated_signature"
          signature
      in

      Log.info "Crafting a l2 transaction with too big amount" ;
      let tx =
        craft_tx
          ~counter:2L
          ~signer:(Bls_pk bls_pk_1)
          ~dest:bls_pkh_2_str
          ~ticket:ticket_id
          1_000_000L
      in
      let signature = sign_one_transaction bls_sk_1 [tx] in
      Log.info "Submitting the wrong amount L2 transaction" ;
      let* _ =
        tx_client_inject_transaction
          ~tx_node
          [tx]
          ~failswith:"proto.alpha.tx_rollup_balance_too_low"
          signature
      in
      Log.info "Checking rollup node queue" ;
      let* q = Rollup_node.Client.get_queue ~tx_node in
      let len_q = JSON.(q |> as_list |> List.length) in
      Check.((len_q = 2) int) ~error_msg:"Queue length is %L but should be %R" ;
      Log.info "Checking rollup node queue transactions" ;
      let* _t1 = Rollup_node.Client.get_transaction_in_queue ~tx_node txh1
      and* _t2 = Rollup_node.Client.get_transaction_in_queue ~tx_node txh2 in
      let* () = Client.bake_for client in
      let* () = Client.bake_for client in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 7 in
      let* inbox = Rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      check_inbox_success inbox ;
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:100_004
      and* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2_str
          ~expected_balance:99_996
      in

      let inject_tx ~counter ~from ~dest ?(amount = 1L) sk =
        let tx =
          craft_tx ~counter ~signer:from ~dest ~ticket:ticket_id amount
        in
        let signature = sign_one_transaction sk [tx] in
        tx_client_inject_transaction ~tx_node [tx] signature
      in
      let nbtxs1 = 70 in
      let injection_success_promise =
        wait_for_injection_success_event tx_node
      in
      Log.info "Injecting %d transactions to queue" nbtxs1 ;
      let* () =
        Lwt_list.iter_s
          (fun counter ->
            let* _ =
              inject_tx
                ~counter
                ~from:(Bls_pk bls_pk_1)
                ~dest:bls_pkh_2_str
                bls_sk_1
            in
            unit)
          (List.init nbtxs1 (fun i -> Int64.of_int (i + 2)))
      in
      let nbtxs2 = 30 in
      Log.info "Injecting %d transactions to queue" nbtxs2 ;
      let* () =
        Lwt_list.iter_s
          (fun counter ->
            let* _ =
              inject_tx
                ~counter
                ~from:(Bls_pk bls_pk_2)
                ~dest:bls_pkh_1_str
                bls_sk_2
            in
            unit)
          (List.init nbtxs2 (fun i -> Int64.of_int (i + 2)))
      in
      let* q = Rollup_node.Client.get_queue ~tx_node in
      let len_q = JSON.(q |> as_list |> List.length) in
      Check.((len_q = nbtxs1 + nbtxs2) int)
        ~error_msg:"Queue length is %L but should be %R" ;
      let* () = Client.bake_for client in
      Log.info "Waiting for injection on L1 to succeed" ;
      let* () = injection_success_promise in
      Log.info "Injection succeeded" ;
      let* () = Client.bake_for client in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 9 in
      let* inbox = Rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      check_inbox_success inbox ;
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:(100_004 - nbtxs1 + nbtxs2)
      and* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2_str
          ~expected_balance:(99_996 + nbtxs1 - nbtxs2)
      in
      unit)

(* Check that the rollup node is successfully doing reorganizations.
   To do so, we are going to:
   - create a branch of size one on node 1 (updating a L2 balance)
   - create a branch of size two on node 2 (with no L2 operations)
   - connecting node 1 and node 2
   - check that operation modifying the L2 balance was not applied
 *)
let test_reorganization =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: L2 rollup node reorganization"
    ~tags:["tx_rollup"; "node"; "reorganization"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let nodes_args = Node.[Connections 2; Synchronisation_threshold 0] in
      let* (node1, client1) =
        Client.init_with_protocol
          ~nodes_args
          ~parameter_file
          `Client
          ~protocol
          ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* (tx_rollup_hash, tx_node) =
        init_and_run_rollup_node ~operator node1 client1
      in
      let* contract_id =
        Client.originate_contract
          ~alias:"rollup_deposit"
          ~amount:Tez.zero
          ~src:"bootstrap1"
          ~prg:"file:./tezt/tests/contracts/proto_alpha/tx_rollup_deposit.tz"
          ~init:"Unit"
          ~burn_cap:Tez.(of_int 1)
          client1
      in
      let* () = Client.bake_for client1 in
      let* _ = Node.wait_for_level node1 3 in
      Log.info
        "The tx_rollup_deposit %s contract was successfully originated"
        contract_id ;
      (* Genarating some identities *)
      let (bls_1_pkh, bls_pk_1, bls_sk_1) = generate_bls_addr client1 in
      let bls_pkh_1_str = Bls_public_key_hash.to_b58check bls_1_pkh in
      (* FIXME/TORU: Use the client *)
      let (bls_2_pkh, _, _) = generate_bls_addr client1 in
      let bls_pkh_2_str = Bls_public_key_hash.to_b58check bls_2_pkh in
      let arg_1 =
        make_tx_rollup_deposit_argument tx_rollup_hash bls_pkh_1_str
      in
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg:arg_1
          client1
      in
      let* () = Client.bake_for client1 in
      let* _ = Node.wait_for_level node1 4 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 4 in
      let* inbox = Rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit (List.hd inbox.contents) in
      (* Run the node that will be used to forge an alternative branch *)
      let* node2 = Node.init nodes_args in
      let* client2 = Client.init ~endpoint:Client.(Node node2) () in
      let* () = Client.Admin.connect_address client2 ~peer:node1 in
      let* _ = Node.wait_for_level node2 4 in
      Log.info "Nodes are synchronized, shutting down node 2" ;
      let* () = Node.terminate node2 in
      Log.info "Check that L2 balance is 100_000" ;
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:100_000
      in
      Log.info "crafting a branch of size 1 on node 1 with a L2 transfer" ;
      (* FIXME/TORU: Use the client *)
      let tx =
        craft_tx
          ~counter:1L
          ~signer:(Bls_pk bls_pk_1)
          ~dest:bls_pkh_2_str
          ~ticket:ticket_id
          10L
      in
      let batch = craft_batch [[tx]] [[bls_sk_1]] in
      let content_batch1 =
        Hex.of_string
          (Data_encoding.Binary.to_string_exn
             Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.encoding
             (Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.V1 batch))
      in
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content:content_batch1
          ~rollup:tx_rollup_hash
          ~src:operator
          client1
      in
      let* () = Client.bake_for client1 in
      let* _ = Node.wait_for_level node1 5 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 5 in
      Log.info "Check that L2 balance is now 99_990" ;
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:99_990
      in
      Log.info "Running the node2 in private mode and craft a branch of size 2" ;
      let* () = Node.run node2 (Node.Private_mode :: nodes_args) in
      let* () = Node.wait_for_ready node2 in
      let* () =
        Client.bake_for ~keys:[Constant.bootstrap2.public_key_hash] client2
      in
      let* () =
        Client.bake_for ~keys:[Constant.bootstrap2.public_key_hash] client2
      in
      let* _ = Node.wait_for_level node2 6 in
      Log.info "Reconnecting node 1 and 2" ;
      let* () = Client.Admin.trust_address client2 ~peer:node1 in
      let* () = Client.Admin.connect_address client2 ~peer:node1 in
      let* _ = Node.wait_for_level node1 6 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 6 in
      (* Check that the balance is untouched, that is to say that the
         rollup node had backtracked the operation from the
         alternative branch. *)
      Log.info "Check that L2 balance is back to 10" ;
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:100_000
      in
      unit)

let test_l2_proofs =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: reject messages at position 0 and 1"
    ~tags:["tx_rollup"; "node"; "proofs"; "rejection"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* (tx_rollup_hash, tx_node) =
        init_and_run_rollup_node ~operator node client
      in
      let* contract_id =
        Client.originate_contract
          ~alias:"rollup_deposit"
          ~amount:Tez.zero
          ~src:"bootstrap1"
          ~prg:"file:./tezt/tests/contracts/proto_alpha/tx_rollup_deposit.tz"
          ~init:"Unit"
          ~burn_cap:Tez.(of_int 1)
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      Log.info
        "The tx_rollup_deposit %s contract was successfully originated"
        contract_id ;
      (* Generating some identities *)
      let (pkh1, pk1, sk1) = generate_bls_addr client in
      let pkh1_str = Bls_public_key_hash.to_b58check pkh1 in
      let (pkh2, pk2, sk2) = generate_bls_addr client in
      let pkh2_str = Bls_public_key_hash.to_b58check pkh2 in
      let arg = make_tx_rollup_deposit_argument tx_rollup_hash pkh1_str in
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 4 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 4 in
      let* inbox = Rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit (List.hd inbox.contents) in
      Log.info "Ticket %s was successfully emitted" ticket_id ;
      Log.info "Commitment for rollup level: 0" ;
      let* {
             tx_level;
             roots;
             context_hashes = context_hashes_level0;
             inbox_merkle_root;
             predecessor;
           } =
        build_commitment_info ~tx_level:0 ~tx_rollup_hash ~tx_node ~client
      in
      let*! () =
        Client.Tx_rollup.submit_commitment
          ~level:tx_level
          ~roots
          ~inbox_merkle_root
          ~predecessor
          ~rollup:tx_rollup_hash
          ~src:Constant.bootstrap3.public_key_hash
          client
      in
      (* FIXME/TORU: Use the client *)
      let tx1 =
        craft_tx
          ~counter:1L
          ~signer:(Bls_pk pk1)
          ~dest:pkh2_str
          ~ticket:ticket_id
          5L
      in
      let batch1 = craft_batch [[tx1]] [[sk1]] in
      let content1 =
        Hex.of_string
          (Data_encoding.Binary.to_string_exn
             Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.encoding
             (Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.V1 batch1))
      in
      let tx2 =
        craft_tx
          ~counter:1L
          ~signer:(Bls_pk pk2)
          ~dest:pkh1_str
          ~ticket:ticket_id
          10L
      in
      let batch2 = craft_batch [[tx2]] [[sk2]] in
      let content2 =
        Hex.of_string
          (Data_encoding.Binary.to_string_exn
             Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.encoding
             (Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.V1 batch2))
      in
      Log.info "Submiting two batches" ;
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content:content1
          ~rollup:tx_rollup_hash
          ~src:operator
          client
      in
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content:content2
          ~rollup:tx_rollup_hash
          ~src:Constant.bootstrap2.public_key_hash
          client
      in
      Log.info "Baking the batches" ;
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 5 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 5 in
      Log.info "Commitment for rollup level: 1" ;
      let* ({
              tx_level;
              roots;
              context_hashes = _context_hashes_level1;
              inbox_merkle_root;
              predecessor;
            } as commitment_info) =
        build_commitment_info ~tx_level:1 ~tx_rollup_hash ~tx_node ~client
      in
      let*! () =
        Client.Tx_rollup.submit_commitment
          ~level:tx_level
          ~roots
          ~inbox_merkle_root
          ~predecessor
          ~rollup:tx_rollup_hash
          ~src:operator
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 6 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 6 in
      Log.info "Try to reject a good commitment at level 1, message 0" ;
      let last_prev_pos = List.length context_hashes_level0 - 1 in
      let agreed_context_hash = List.nth context_hashes_level0 last_prev_pos in
      let* agreed_message_result_path =
        let*! agreed_message_result_path =
          Rollup.commitment_merkle_tree_path
            ~message_result_hashes:
              (List.map (fun x -> `Hash x) commitment_info.roots)
            ~position:last_prev_pos
            client
        in
        return (JSON.encode agreed_message_result_path)
      in
      let* {
             proof;
             tx_level;
             message;
             position;
             path;
             rejected_message_result_hash;
             rejected_message_result_path;
             context_hash;
             withdraw_list_hash;
             agreed_message_result_path;
           } =
        build_rejection
          ~tx_level:1
          ~tx_node
          ~message_pos:0
          ~client
          ~agreed_context_hash
          ~agreed_message_result_path
          commitment_info
      in
      let*? process =
        Client.Tx_rollup.submit_rejection
          ~src:operator
          ~proof
          ~rollup:tx_rollup_hash
          ~level:tx_level
          ~message
          ~position
          ~path
          ~message_result_hash:rejected_message_result_hash
          ~rejected_message_result_path
          ~context_hash
          ~withdraw_list_hash
          ~agreed_message_result_path
          client
      in
      let* () =
        Process.check_error
          ~msg:(rex "proto.alpha.tx_rollup_proof_produced_rejected_state")
          process
      in
      Log.info "Try to reject a good commitment at level 1, message 1" ;
      let* {
             proof;
             tx_level;
             message;
             position;
             path;
             rejected_message_result_hash;
             rejected_message_result_path;
             context_hash;
             withdraw_list_hash;
             agreed_message_result_path;
           } =
        build_rejection
          ~tx_level:1
          ~tx_node
          ~message_pos:0
          ~client
          ~agreed_context_hash
          ~agreed_message_result_path
          commitment_info
      in
      let*? process =
        Client.Tx_rollup.submit_rejection
          ~src:operator
          ~proof
          ~rollup:tx_rollup_hash
          ~level:tx_level
          ~message
          ~position
          ~path
          ~message_result_hash:rejected_message_result_hash
          ~rejected_message_result_path
          ~context_hash
          ~withdraw_list_hash
          ~agreed_message_result_path
          client
      in
      let* () =
        Process.check_error
          ~msg:(rex "proto.alpha.tx_rollup_proof_produced_rejected_state")
          process
      in
      unit)

let register ~protocols =
  test_node_configuration protocols ;
  test_tx_node_origination protocols ;
  test_tx_node_store_inbox protocols ;
  test_ticket_deposit_from_l1_to_l2 protocols ;
  test_l2_to_l2_transaction protocols ;
  test_batcher protocols ;
  test_reorganization protocols ;
  test_l2_proofs protocols
