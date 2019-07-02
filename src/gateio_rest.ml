open Core
open Gateio
open Fastrest

let base_url =
  Uri.make ~scheme:"https" ~host:"api.gateio.co" ()

type error = {
  code: int ;
  message: string ;
} [@@deriving sexp]

let pp_print_error ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_error t)

let error_encoding =
  let open Json_encoding in
  conv
    (fun { code; message } -> (), code, message)
    (fun ((), code, message) -> { code ; message })
    (obj3
       (req "result" (constant "false"))
       (req "code" int)
       (req "message" string))

let ok_encoding encoding =
  let open Json_encoding in
  conv
    (fun a -> ((), "", 0), a)
    (fun (_, a) -> a)
    (merge_objs
       (obj3
          (req "result" (constant "true"))
          (req "message" string)
          (req "code" int))
       encoding)

let result_encoding encoding =
  let open Json_encoding in
  union [
    case error_encoding (function Error a -> Some a | _ -> None) (fun a -> Error a) ;
    case (ok_encoding encoding) (function Ok a -> Some a | _ -> None) (fun a -> Ok a) ;
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

(* type error = {
 *   severity : [`E | `W] ;
 *   cat : string ;
 *   msg : string ;
 *   extra : string option ;
 * }
 * 
 * let time =
 *   let time_encoding =
 *     let open Json_encoding in
 *     conv
 *       (fun t -> (), Int64.of_float (Ptime.to_float_s t))
 *       (fun ((), t) -> match Ptime.of_float_s (Int64.to_float t) with
 *          | None -> invalid_arg "time_encoding"
 *          | Some t -> t)
 *       (merge_objs unit (obj1 (req "unixtime" int53)))
 *   in
 *   get (result_encoding time_encoding)
 *     (Uri.with_path base_url "0/public/Time")
 * 
 * 
 * type 'a assoc = (string * 'a) list [@@deriving sexp]
 * 
 * let balances_encoding =
 *   let open Json_encoding in
 *   conv
 *     (fun s -> `O (List.map ~f:(fun (k, v) -> (k, `Float v)) s))
 *     (function
 *       | `O vs ->
 *         List.map ~f:(function
 *             | k, `String v -> k, float_of_string v
 *             | _ -> invalid_arg "balance_encoding") vs
 *       | #Ezjsonm.value -> invalid_arg "balance_encoding")
 *     any_ezjson_value
 * 
 * let list_encoding encoding =
 *   let open Json_encoding in
 *   conv
 *     (fun s -> `O (List.map ~f:(fun (k, v) ->
 *          (k, Json_encoding.construct encoding v)) s))
 *     (function
 *       | `O vs ->
 *         List.map ~f:begin fun (k, v) ->
 *           k, Ezjsonm_encoding.destruct_safe encoding v
 *         end vs
 *       | #Ezjsonm.value -> invalid_arg "list_encoding")
 *     any_ezjson_value
 * 
 * let boxed_list_encoding name encoding =
 *   let open Json_encoding in
 *   conv (fun t -> t, 0l) (fun (t, _) -> t)
 *     (obj2
 *        (req name (list_encoding encoding))
 *        (req "count" int32))
 * 
 * let trade_encoding = boxed_list_encoding "trades" Filled_order.encoding
 * let closed_encoding = boxed_list_encoding "closed" Order.encoding
 * let ledger_encoding = boxed_list_encoding "ledger" Ledger.encoding
 * 
 * let asset_pairs =
 *   get (result_encoding (list_encoding Pair.encoding))
 *     (Uri.with_path base_url "0/public/AssetPairs")
 * 
 * let account_balance =
 *   post_form ~auth (result_encoding balances_encoding)
 *     (Uri.with_path base_url "0/private/Balance")
 * 
 * let trade_balance =
 *   post_form ~auth (result_encoding Balance.encoding)
 *     (Uri.with_path base_url "0/private/TradeBalance")
 * 
 * let closed_orders ofs =
 *   post_form ~auth ~params:["ofs", [Int.to_string ofs]]
 *     (result_encoding closed_encoding)
 *     (Uri.with_path base_url "0/private/ClosedOrders") *)

type trade = {
  id: int64 ;
  orderid: int64 ;
  pair: Pair.t ;
  side: [`Buy | `Sell] ;
  price: float ;
  qty: float ;
  time: Ptime.t ;
} [@@deriving sexp]

let side_encoding =
  let open Json_encoding in
  conv
    (function `Buy -> "buy" | `Sell -> "sell")
    (function "buy" -> `Buy | _ -> `Sell)
    string

let trade_encoding =
  let open Json_encoding in
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
    (result_encoding Json_encoding.(obj1 (req "trades" (list trade_encoding))))
    (Uri.with_path base_url "api2/1/private/tradeHistory")

(* let ledgers =
 *   post_form ~auth (result_encoding ledger_encoding)
 *     (Uri.with_path base_url "0/private/Ledgers") *)
