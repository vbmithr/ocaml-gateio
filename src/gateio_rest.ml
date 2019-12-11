open Core
open Gateio
open Fastrest
open Json_encoding

let base_url =
  Uri.make ~scheme:"https" ~host:"api.gateio.co" ()

let error_encoding =
  conv
    (fun _ -> assert false)
    (fun ((), code, message) -> Error.createf "%d: %s" code message)
    (obj3
       (req "result" (constant "false"))
       (req "code" int)
       (req "message" string))

let ok_encoding encoding =
  conv
    (fun a -> ((), "", 0), a)
    (fun (_, a) -> a)
    (merge_objs
       (obj3
          (req "result" (constant "true"))
          (req "message" string)
          (req "code" int))
       encoding)

let simple_ok encoding =
  conv
    (fun _ -> assert false)
    (fun ((), v) -> v)
    (merge_objs (obj1 (req "result" (constant "true"))) encoding)

let result_encoding encoding =
  union [
    case error_encoding (function Error a -> Some a | _ -> None) (fun a -> Error a) ;
    case (ok_encoding encoding) (function Ok a -> Some a | _ -> None) (fun a -> Ok a) ;
    case (simple_ok encoding) (function Ok a -> Some a | _ -> None) (fun a -> Ok a)
  ]

let auth srv { key ; secret ; _ } =
  let ps = match srv.params with
    | Form ps -> ps
    | Json (_,_) -> assert false in
  let encoded = Uri.encoded_of_query ps in
  let open Digestif in
  let sign =
    SHA512.(hmac_string ~key:secret encoded |> to_raw_string) in
  let `Hex sign_hex = Hex.of_string sign in
  let headers =
    Httpaf.Headers.of_list [
      "Key", key ;
      "Sign", sign_hex ;
    ] in
  { params = Form ps ; headers }

let trading_pairs =
  let encoding =
    conv
      (function
        | Ok vs -> List.map vs ~f:Pair.to_string
        | Error _ -> invalid_arg "trading_pair_encoding")
      (fun vs -> Ok (List.map vs ~f:Pair.of_string_exn))
      (list string) in
  get encoding
    (Uri.make ~scheme:"https" ~host:"data.gateio.co" ~path:"api2/1/pairs" ())

type trade = {
  id: int64 ;
  orderid: int64 ;
  pair: Pair.t ;
  side: Fixtypes.Side.t ;
  price: float ;
  qty: float ;
  time: Ptime.t ;
} [@@deriving sexp]

let side_encoding =
  conv (fun _ -> assert false)
    (function "buy" -> Fixtypes.Side.Buy | _ -> Sell)
    string

let trade_encoding =
  conv
    (fun { id ; orderid ; pair ; side ; price ; qty ; time } ->
       (), (id, orderid, pair, side, price, qty, time))
    (fun ((), (id, orderid, pair, side, price, qty, time)) ->
       { id ; orderid ; pair ; side ; price ; qty ; time })
    (merge_objs unit
       (obj7
          (req "tradeID" int53)
          (req "orderNumber" int53)
          (req "pair" Pair.encoding)
          (req "type" side_encoding)
          (req "rate" Encoding.strfl)
          (req "amount" Encoding.strfl)
          (req "time_unix" Ptime.encoding)))

let pp_print_trade ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_trade t)

let trade_history pair =
  post_form ~auth ~params:["currencyPair", [Pair.to_string pair]]
    (result_encoding (obj1 (req "trades" (list trade_encoding))))
    (Uri.with_path base_url "api2/1/private/tradeHistory")

type balances = {
  available: (string * float) list ;
  locked: (string * float) list ;
}

let bs =
  conv
    (fun _ -> assert false)
    (function
      | `O vs ->
        List.Assoc.map vs ~f:(function
            | `String s -> Float.of_string s
            | _ -> assert false)
      | _ -> assert false)
    any_ezjson_value

let b =
  conv
    (fun _ -> assert false)
    (fun (available, locked) -> { available; locked })
    (obj2 (req "available" bs) (req "locked" bs))

let balances =
  post_form ~auth
    (result_encoding b)
    (Uri.with_path base_url "api2/1/private/balances")

type entry = {
  id: string;
  currency: string;
  address: string;
  amount: float;
  txid: string;
  timestamp: Ptime.t;
  status: [`Done];
} [@@deriving sexp]

let pp_entry ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_entry t)

let status =
  string_enum [
    "DONE", `Done;
  ]

let entry =
  conv
    (fun _ -> assert false)
    (fun (id, currency, address, amount, txid, timestamp, status) ->
       { id; currency; address; amount; txid; timestamp; status })
    (obj7
       (req "id" string)
       (req "currency" string)
       (req "address" string)
       (req "amount" Encoding.strfl)
       (req "txid" string)
       (req "timestamp" Ptime.encoding)
       (req "status" status))

type entries = {
  deposits: entry list;
  withdrawals: entry list;
}

let movements =
  conv
    (fun _ -> assert false)
    (fun (deposits, withdrawals) -> { deposits; withdrawals })
    (obj2
       (req "deposits" (list entry))
       (req "withdraws" (list entry)))

let entries ?start ?stop () =
  let string_of_time time =
    Printf.sprintf "%.0f" (Ptime.to_float_s time) in
  let params =
    List.filter_opt
      [Option.map start ~f:(fun start -> "start", [string_of_time start]);
       Option.map stop ~f:(fun stop -> ("end", [string_of_time stop]));
      ] in
  post_form
    ~auth
    ~params
    (result_encoding movements)
    (Uri.with_path base_url "api2/1/private/depositsWithdrawals")
