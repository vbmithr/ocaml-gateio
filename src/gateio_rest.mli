open Gateio
open Fastrest

type trade = {
  id: int64 ;
  orderid: int64 ;
  pair: Pair.t ;
  side: Fixtypes.Side.t ;
  price: float ;
  qty: float ;
  time: Ptime.t ;
} [@@deriving sexp]

val pp_print_trade : Format.formatter -> trade -> unit

val trade_history : Pair.t -> (form, trade list) service
val trading_pairs : (form, Pair.t list) service

type balances = {
  available: (string * float) list ;
  locked: (string * float) list ;
}

val balances : (form, balances) service

type entry = {
  id: string;
  currency: string;
  address: string;
  amount: float;
  txid: string;
  timestamp: Ptime.t;
  status: [`Done];
} [@@deriving sexp]

val pp_entry : entry Fmt.t

type entries = {
  deposits: entry list;
  withdrawals: entry list;
}

val entries :
  ?start:Ptime.t -> ?stop:Ptime.t -> unit -> (form, entries) service
