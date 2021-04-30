(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module provides a set of abstractions to reason about the
    so-called “liquidity baking” feature[1].

    [1]: https://gitlab.com/tezos/tzip/-/blob/master/drafts/current/draft-liquidity_baking.md

    We remind that this feature is built upon three smart contracts:
    (1) a CPMM contract initially based on Dexter 2, and (2) two
    tokens contracts.

    Our purpose for Liquidity Baking is to easily express and test
    invariants regarding the execution of these contracts.  To that
    end, we have introduced a set of dedicated types to describe
    arbitrary contexts in terms of account balances (see
    [Liquidity_baking_machine.specs]), along with [build] functions
    that turn a description of a context into concrete states.

    In this module, we provide QCheck generators which allow to
    construct arbitrary specifications for states, and so-called
    scenarios ({i i.e.}, sequences of entrypoint calls). *)

open Liquidity_baking_machine

(** [arb_specs max_tzbtc max_liquidity] constructs arbitrary Liquidity
    Baking [specs] for an initial state, where at most [max_tzbtc] and
    [max_liquidity] are shared among an arbitrary number of implicit
    accounts. *)
val arb_specs : tzbtc -> liquidity -> specs QCheck.arbitrary

(** [arb_scenario max_tzbtc max_liquidity size] constructs arbitrary
    Liquidity Baking [specs] with a semantics similar to [arb_specs], along with sequences of {b valid}
    scenarios ({i i.e.}, sequences of entrypoint calls) of length
    [size]. By valid, we mean that running the scenario using a
    Liquidity baking machine initialized with the [specs] should
    succeed. *)
val arb_scenario :
  tzbtc -> liquidity -> int -> (specs * contract_id step list) QCheck.arbitrary
