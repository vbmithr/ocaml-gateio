type errorCode =
  | InvalidArg
  | InternalErr
  | ServiceUnavailable
  | MethodNotFound
  | ServiceTimeout
[@@deriving sexp]

type errMsg = {
  code: errorCode ;
  msg: string ;
}

type nbLevel = One | Five | Ten | Twenty | Thirty

type subscription =
  | Ticker of string list
  | Trades of string list
  | Quotes of (string * nbLevel) list

type trade = {
  id: int64 ;
  ts: Ptime.t ;
  price: float ;
  amount: float ;
  side: [`Buy | `Sell] ;
}

type level = { p: float; q: float }

type depth = {
  market: string ;
  kind: [`Partial | `Update] ;
  bids: level list ;
  asks: level list ;
}

type t =
  | Unsubscribe of int64 * [`Trades | `Quotes]
  | Subscribe of int64 * subscription
  | Ping of int64
  | Pong of int64
  | TimeReq of int64
  | TimeResp of { id: int64; ts: Ptime.t }
  | Success of int64
  | Err of { id: int64 ; err: errMsg }
  | Trades of string * trade list
  | Quotes of depth
[@@deriving sexp]

val pp : Format.formatter -> t -> unit
val encoding : t Json_encoding.encoding
