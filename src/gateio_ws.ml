open Sexplib.Std
open Gateio
open Json_encoding

type errorCode =
  | InvalidArg
  | InternalErr
  | ServiceUnavailable
  | MethodNotFound
  | ServiceTimeout
[@@deriving sexp]

let errorCode_encoding =
  let open Json_encoding in
  conv
    begin function
      | InvalidArg -> 1
      | InternalErr -> 2
      | ServiceUnavailable -> 3
      | MethodNotFound -> 4
      | ServiceTimeout -> 5
    end
    begin function
      | 1 -> InvalidArg
      | 2 -> InternalErr
      | 3 -> ServiceUnavailable
      | 4 -> MethodNotFound
      | 5 -> ServiceTimeout
      | _ -> invalid_arg "errorCode_encoding"
    end
    int

type errMsg = {
  code: errorCode ;
  msg: string ;
} [@@deriving sexp]

let errMsg_encoding =
  conv
    (fun { code ; msg } -> (code, msg))
    (fun (code, msg) -> { code ; msg })
    (obj2
       (req "code" errorCode_encoding)
       (req "message" string))

let request_encoding meth e =
  let open Json_encoding in
  conv
    (fun (id, params) -> (id, (), params))
    (fun (id, (), params) -> (id, params))
    (obj3
       (req "id" int53)
       (req "method" (constant meth))
       (req "params" (list e)))

let notification_encoding meth e =
  conv
    (fun id -> ((), (), id))
    (fun ((), (), id) -> id)
    (obj3
       (req "id" null)
       (req "method" (constant meth))
       (req "params" e))

let resp_ok_encoding e =
  conv
    (fun (id, v) -> (id, (), v))
    (function (id, (), v) -> id, v)
    (obj3
       (req "id" int53)
       (req "error" null)
       (req "result" e))

let resp_err_encoding =
  conv
    (fun (id, errMsg) -> id, errMsg, ())
    (function (id, errMsg, ()) -> (id, errMsg))
    (obj3
       (req "id" int53)
       (req "error" errMsg_encoding)
       (req "result" null))

type trade = {
  id: int64 ;
  ts: Ptime.t ;
  price: float ;
  amount: float ;
  side: Fixtypes.Side.t ;
} [@@deriving sexp]

let side_encoding =
  string_enum [
    "buy", Fixtypes.Side.Buy ;
    "sell", Sell ;
  ]

let trade_encoding =
  conv
    (fun { id ; ts; price; amount; side } -> (id, ts, price, amount, side))
    (fun (id, ts, price, amount, side) -> { id ; ts; price; amount; side })
    (obj5
       (req "id" int53)
       (req "time" Ptime.encoding)
       (req "price" Encoding.strfl)
       (req "amount" Encoding.strfl)
       (req "type" side_encoding))

type level = { p: float; q: float } [@@deriving sexp]

let level_encoding =
  conv
    (fun { p ; q } -> (p, q))
    (fun (p, q) -> { p ; q })
    (tup2 Encoding.strfl Encoding.strfl)

let depthobj_encoding =
  obj2
    (dft "bids" (list level_encoding) [])
    (dft "asks" (list level_encoding) [])

let kind_encoding =
  conv
    (function `Partial -> true | `Update -> false)
    (function true -> `Partial | false -> `Update)
    bool

type depth = {
  market: Pair.t ;
  kind: [`Partial | `Update] ;
  bids: level list ;
  asks: level list ;
} [@@deriving sexp]

let depth_encoding =
  conv
    (fun { market ; kind ; bids ; asks } -> (kind, (bids, asks), market))
    (fun (kind, (bids, asks), market) -> { market ; kind ; bids ; asks })
    (tup3 kind_encoding depthobj_encoding Pair.encoding)

let success_encoding = resp_ok_encoding (obj1 (req "status" (constant "success")))
let trade_encoding = notification_encoding "trades.update" (tup2 Pair.encoding (list trade_encoding))
let depth_encoding = notification_encoding "depth.update" depth_encoding

type nbLevel = One | Five | Ten | Twenty | Thirty [@@deriving sexp]
let nbLevel_encoding =
  conv
    (function One -> 1 | Five -> 5 | Ten -> 10 | Twenty -> 20 | Thirty -> 30)
    (function 1 -> One | 5 -> Five | 10 -> Ten | 20 -> Twenty | 30 -> Thirty | _ -> invalid_arg "nLevel_encoding")
    int

type subscription =
  | Ticker of Pair.t list
  | Trades of Pair.t list
  | Quotes of (Pair.t * nbLevel) list
[@@deriving sexp]

let quotes_spec_encoding =
  conv
    (fun (sym, lvl) -> (sym, lvl, ()))
    (fun (sym, lvl, ()) -> (sym, lvl))
    (tup3 Pair.encoding nbLevel_encoding (constant "0"))

type t =
  | Unsubscribe of int64 * [`Trades | `Quotes]
  | Subscribe of int64 * subscription
  | Ping of int64
  | Pong of int64
  | TimeReq of int64
  | TimeResp of { id: int64; ts: Ptime.t }
  | Success of int64
  | Err of { id: int64 ; err: errMsg }
  | Trades of Pair.t * trade list
  | Quotes of depth
[@@deriving sexp]

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

let encoding =
  union [
    case (request_encoding "trades.unsubscribe" unit)
      (function Unsubscribe (id, `Trades) -> Some (id, []) | _ -> None)
      (fun (id, _) -> Unsubscribe (id, `Trades)) ;
    case (request_encoding "depth.unsubscribe" unit)
      (function Unsubscribe (id, `Quotes) -> Some (id, []) | _ -> None)
      (fun (id, _) -> Unsubscribe (id, `Quotes)) ;
    case (request_encoding "ticker.subscribe" Pair.encoding)
      (function Subscribe (id, Ticker ts) -> Some (id, ts) | _ -> None)
      (fun (id, ts) -> Subscribe (id, Ticker ts)) ;
    case (request_encoding "trades.subscribe" Pair.encoding)
      (function Subscribe (id, Trades ts) -> Some (id, ts) | _ -> None)
      (fun (id, ts) -> Subscribe (id, Trades ts)) ;
    case (request_encoding "depth.subscribe" quotes_spec_encoding)
      (function Subscribe (id, Quotes qs) -> Some (id, qs) | _ -> None)
      (fun (id, qs) -> Subscribe (id, Quotes qs)) ;
    case resp_err_encoding
      (function Err { id ; err } -> Some (id, err) | _ -> None)
      (function (id, err) -> Err { id ; err }) ;
    case (request_encoding "server.ping" unit)
      (function Ping id -> Some (id, []) | _ -> None)
      (fun (id, _) -> Ping id) ;
    case (resp_ok_encoding (constant "pong"))
      (function Pong id -> Some (id, () ) | _ -> None)
      (fun (id, ()) -> Pong id) ;
    case (request_encoding "server.time" unit)
      (function TimeReq id -> Some (id, []) | _ -> None)
      (fun (id, _) -> TimeReq id) ;
    case (resp_ok_encoding Ptime.encoding)
      (function TimeResp { id; ts } -> Some (id, ts) | _ -> None)
      (fun (id, ts) -> TimeResp { id; ts }) ;
    case success_encoding
      (function Success id -> Some (id, ()) | _ -> None)
      (fun (id, ()) -> Success id) ;
    case trade_encoding
      (function Trades (s, ts) -> Some (s, ts) | _ -> None)
      (fun (s, ts) -> Trades (s, ts)) ;
    case depth_encoding
      (function Quotes q -> Some q | _ -> None)
      (fun q -> Quotes q) ;
  ]
