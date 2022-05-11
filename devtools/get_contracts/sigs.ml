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

module Time = Time.Protocol

module type PROTOCOL = sig
  module Error_monad : sig
    type 'a tzresult
  end

  type ('k, 'v) map

  type ('arg, 'ret) lambda

  type 'a ty

  module Alpha_context : sig
    type t

    type context = t

    module Fitness : sig
      type t
    end

    module Timestamp : sig
      type time
    end
  end

  module Raw_context : sig
    type t

    val prepare :
      level:Int32.t ->
      predecessor_timestamp:Time.t ->
      timestamp:Time.t ->
      fitness:Fitness.t ->
      Environment_context.Context.t ->
      t Error_monad.tzresult Lwt.t

    val recover : t -> Environment_context.Context.t
  end

  module Script : sig
    type prim

    type expr = prim Micheline.canonical

    type lazy_expr = expr Data_encoding.lazy_t

    type node = (Micheline.canonical_location, prim) Micheline.node

    val expr_encoding : expr Data_encoding.t

    val print_expr : Format.formatter -> expr -> unit

    module Hash : sig
      type t

      val compare : t -> t -> int

      val to_b58check : t -> string

      val hash_bytes : ?key:bytes -> bytes list -> t
    end
  end

  module Contract : sig
    type repr

    val is_implicit : repr -> Signature.Public_key_hash.t option

    val pp : Format.formatter -> repr -> unit

    val get_code :
      Raw_context.t ->
      repr ->
      (Raw_context.t * Script.lazy_expr) Error_monad.tzresult Lwt.t

    val get_storage :
      Raw_context.t ->
      repr ->
      (Raw_context.t * Script.lazy_expr) Error_monad.tzresult Lwt.t

    val fold :
      Raw_context.t -> init:'a -> f:(repr -> 'a -> 'a Lwt.t) -> 'a Lwt.t
  end

  module Script_ir_translator : sig
    open Alpha_context

    type toplevel

    type ex_ty = Ex_ty : 'a ty -> ex_ty

    type type_logger

    val parse_ty :
      context ->
      legacy:bool ->
      allow_lazy_storage:bool ->
      allow_operation:bool ->
      allow_contract:bool ->
      allow_ticket:bool ->
      Script.node ->
      (ex_ty * context) Error_monad.tzresult

    val parse_data :
      ?type_logger:type_logger ->
      context ->
      legacy:bool ->
      allow_forged:bool ->
      'a ty ->
      Script.node ->
      ('a * context) Error_monad.tzresult Lwt.t

    val unparse_ty :
      context -> 'a ty -> (Script.node * context) Error_monad.tzresult

    val parse_toplevel :
      context ->
      legacy:bool ->
      Script.expr ->
      (toplevel * context) Error_monad.tzresult Lwt.t
  end

  module Storage : sig
    type big_map_id

    val id_to_z : big_map_id -> Z.t

    val list_values :
      ?offset:int ->
      ?length:int ->
      Raw_context.t * big_map_id ->
      (Raw_context.t * Script.expr list) Error_monad.tzresult Lwt.t

    val get :
      Raw_context.t -> big_map_id -> Script.expr Error_monad.tzresult Lwt.t

    val fold :
      Raw_context.t -> init:'a -> f:(big_map_id -> 'a -> 'a Lwt.t) -> 'a Lwt.t
  end

  module Unparse_types : sig
    type ex_lambda =
      | Ex_lambda : ('a, 'b) lambda ty * ('a, 'b) lambda -> ex_lambda

    val collect_lambda_tys : 'a ty -> ('a -> ex_lambda list) list

    val collect_lambda_tys_map :
      'tv ty -> (('tk, 'tv) map -> ex_lambda list) list
  end

  val code_storage_type : Script_ir_translator.toplevel -> Script.node

  val is_unpack : Script.prim -> bool

  val lam_node : (_, _) lambda -> Script.node

  val wrap_tzresult : 'a Error_monad.tzresult -> 'a tzresult
end
