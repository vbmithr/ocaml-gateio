open Gateio
open Fastrest

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

val trade_history : Pair.t -> (form, trade list) service
val trading_pairs : (form, Pair.t list) service

type balances = {
  available: (string * float) list ;
  locked: (string * float) list ;
}

val balances : (form, balances) service

type movement = {
  id: string;
  currency: string;
  address: string;
  amount: float;
  txid: string;
  timestamp: Ptime.t;
  status: [`Done];
}

type movements = {
  deposits: movement list;
  withdrawals: movement list;
}

val movements :
  ?start:Ptime.t -> ?stop:Ptime.t -> unit -> (form, movements) service
