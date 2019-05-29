open Gateio
open Fastrest

type error = {
  code: int ;
  message: string ;
} [@@deriving sexp]

val pp_print_error : Format.formatter -> error -> unit

type trade = {
  id: int64 ;
  orderid: int64 ;
  pair: Pair.t ;
  side: [`Buy | `Sell] ;
  price: float ;
  qty: float ;
  time: Ptime.t ;
} [@@deriving sexp]

val pp_print_trade : Format.formatter -> trade -> unit

val side_encoding : [`Buy | `Sell] Json_encoding.encoding

val trade_history : Pair.t -> (post_form, trade list, error) service

(* val time : (get, Ptime.t, string list) service
 * 
 * type 'a assoc = (string * 'a) list [@@deriving sexp]
 * 
 * val asset_pairs : (get, (string * Pair.t) list, string list) service
 * val account_balance : (post_form, float assoc, string list) service
 * val trade_balance : (post_form, Balance.t, string list) service
 * val closed_orders : int -> (post_form, Order.t assoc, string list) service
 * val ledgers : (post_form, Ledger.t assoc, string list) service *)
