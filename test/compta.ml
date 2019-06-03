open Core
open Async

let src = Logs.Src.create "kraken.compta"
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

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

let url = Uri.make ~scheme:"kdb" ~host:"localhost" ~port:5042 ()

let side_of_string = function "buy" -> `Buy | _ -> `Sell
let string_of_side = function `Buy -> "buy" | `Sell -> "sell"

let sides_encoding =
  let open Kx in
  conv
    (Array.map ~f:string_of_side)
    (Array.map ~f:side_of_string)
    (v sym)

open Gateio

let pairs_encoding =
  let open Kx in
  conv
    (Array.map ~f:Pair.to_string)
    (Array.map ~f:Pair.of_string_exn)
    (v sym)

let line =
  let open Kx in
  t7 (v timestamp) pairs_encoding (list (s char))
    sides_encoding (v sym) (v float) (v float)

open Gateio_rest

let insertFills w fills =
  let open Kx in
  let (times,syms,tids,sides,ordTypes,prices,qties) =
    List.fold_right fills ~init:([],[],[],[],[],[],[])
      ~f:begin fun { id; orderid = _; pair; side; price; qty; time }
        (times,syms,tids,sides,ordTypes,prices,qties) ->
        (time :: times,
         pair :: syms,
         Int64.to_string id :: tids,
         side :: sides,
         "" :: ordTypes,
         price :: prices,
         qty :: qties)
      end in
  let v =
    construct line Array.(of_list times,
                          of_list syms,
                          of_list tids,
                          of_list sides,
                          of_list ordTypes,
                          of_list prices,
                          of_list qties) in
  Pipe.write w ("upd", [|v|])

let main () =
  Kx_async.with_connection url ~f:begin fun _r w ->
    Fastrest.request
      ~auth:{ Fastrest.key = cfg.key ;
              secret = cfg.secret ;
              meta = [] } (Gateio_rest.trade_history {base="xtz"; quote="btc"}) >>= function
    | Error e ->
      Log_async.err begin fun m ->
        m "%a" (Fastrest.pp_print_error pp_print_error) e
      end
    | Ok fills ->
      insertFills w fills >>= fun () ->
      let len = List.length fills in
      Logs_async.app (fun m -> m "Found %d fills" len) >>= fun () ->
      Deferred.List.iter fills ~f:begin fun fill ->
        Log_async.app (fun m -> m "%a" pp_print_trade fill)
      end
  end >>= fun _ ->
  Deferred.unit

let () =
  Command.async ~summary:"Gate.io kdb+ compta" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
