open Core
open Async

open Gateio_rest

module Cfg = struct
  type cfg = {
    key: string ;
    secret: string ;
    passphrase: string [@default ""];
    quote: (string * int) list [@default []];
  } [@@deriving sexp]

  type t = (string * cfg) list [@@deriving sexp]
end

let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
let cfg =
  List.Assoc.find_exn ~equal:String.equal
    (Sexplib.Sexp.load_sexp_conv_exn default_cfg Cfg.t_of_sexp) "GATEIO"

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug)

let wrap_request ?(speed=`Quick) n service =
  let auth = {
    Fastrest.key = cfg.Cfg.key ;
    secret = cfg.Cfg.secret ;
    meta = [] ;
  } in
  Alcotest_async.test_case n speed begin fun () ->
    Fastrest.request ~auth service |>
    Deferred.ignore_m
  end

let rest = [
  (* wrap_request "time" time ;
   * wrap_request "account_balance" account_balance ;
   * wrap_request "trade_balance" trade_balance ;
   * wrap_request "closed_orders" (closed_orders 0) ; *)
  wrap_request "trade_history" (trade_history {base="xtz"; quote="btc"}) ;
  wrap_request "trading_pairs" trading_pairs ;
  wrap_request "balances" balances ;
  wrap_request "entries" (entries ()) ;
]

let () =
  Alcotest.run "gateio" [
    "rest", rest ;
  ]
