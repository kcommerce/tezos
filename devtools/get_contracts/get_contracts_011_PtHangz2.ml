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

module Main = Get_contracts.Make (struct
  include Tezos_raw_protocol_011_PtHangz2
  module Context = Raw_context

  type ('k, 'v) map = ('k, 'v) Script_typed_ir.map

  type ('a, 'r) lambda = ('a, 'r) Script_typed_ir.lambda

  type 'a ty = 'a Script_typed_ir.ty

  type context = Context.t

  module Error_monad =
    Tezos_protocol_environment_011_PtHangz2.Environment.Error_monad

  module Contract = struct
    type repr = Contract_repr.t

    let pp = Contract_repr.pp

    let is_implicit = Contract_repr.is_implicit

    let get_code = Storage.Contract.Code.get

    let get_storage = Storage.Contract.Storage.get

    let fold = Storage.Contract.fold
  end

  module Script = struct
    include Alpha_context.Script
    module Hash = Script_expr_hash

    let print_expr = Tezos_client_011_PtHangz2.Michelson_v1_printer.print_expr
  end

  module Translator = struct
    type toplevel = Script_ir_translator.toplevel

    type ex_ty = Script_ir_translator.ex_ty = Ex_ty : 'a ty -> ex_ty

    type type_logger = Script_ir_translator.type_logger

    let parse_ty (ctxt : Raw_context.t) ~legacy ~allow_lazy_storage
        ~allow_operation ~allow_contract ~allow_ticket script =
      let open Result_syntax in
      let+ ty, _ =
        Script_ir_translator.parse_ty
          (Obj.magic ctxt)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          script
      in
      ty

    let parse_data ?type_logger (ctxt : Raw_context.t) ~legacy ~allow_forged ty
        expr =
      let open Lwt_result_syntax in
      let+ data, _ =
        Script_ir_translator.parse_data
          ?type_logger
          (Obj.magic ctxt)
          ~legacy
          ~allow_forged
          ty
          expr
      in
      data

    let unparse_ty (ctxt : Raw_context.t) ty =
      let open Result_syntax in
      let+ expr, _ = Script_ir_translator.unparse_ty (Obj.magic ctxt) ty in
      expr

    let parse_toplevel (ctxt : Raw_context.t) ~legacy expr =
      let open Lwt_result_syntax in
      let+ toplevel, _ =
        Script_ir_translator.parse_toplevel (Obj.magic ctxt) ~legacy expr
      in
      toplevel
  end

  module Storage = struct
    type big_map_id = Storage.Big_map.id

    let id_to_z = Lazy_storage_kind.Big_map.Id.unparse_to_z

    let list_values = Storage.Big_map.Contents.list_values

    let get = Storage.Big_map.Value_type.get

    let fold = Storage.Big_map.fold
  end

  module Unparse_types =
    Tezos_protocol_plugin_011_PtHangz2.Plugin.RPC.Scripts.Unparse_types

  let wrap_tzresult =
    Tezos_protocol_environment_011_PtHangz2.Environment.wrap_tzresult

  let is_unpack = function
    | Michelson_v1_primitives.I_UNPACK -> true
    | _ -> false

  let lam_node (Script_typed_ir.Lam (_, node)) = node

  open Script_ir_translator

  let code_storage_type ({storage_type; _} : toplevel) = storage_type
end)

let () = Main.main ()
