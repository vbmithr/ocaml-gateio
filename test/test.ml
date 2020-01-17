open Core
open Async

open Gateio_rest

let key, secret =
  match String.split ~on:':' (Sys.getenv_exn "TOKEN_GATEIO") with
  | [key; secret] -> key, secret
  | _ -> assert false

let wrap_request ?(speed=`Quick) n service =
  let auth = {
    Fastrest.key = key ;
    secret = secret ;
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
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug) ;
  Alcotest.run "gateio" [
    "rest", rest ;
  ]
