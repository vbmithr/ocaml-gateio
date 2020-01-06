open Core
open Async

let src = Logs.Src.create "gateio.compta"
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

let side_of_string = function "buy" -> Fixtypes.Side.Buy | _ -> Sell
let string_of_side = function Fixtypes.Side.Buy -> "buy" | Sell -> "sell"

let sides =
  let open Kx in
  conv
    (Array.map ~f:string_of_side)
    (Array.map ~f:side_of_string)
    (v sym)

open Gateio

let pairs =
  let open Kx in
  conv
    (Array.map ~f:Pair.to_string)
    (Array.map ~f:Pair.of_string_exn)
    (v sym)

open Gateio_rest

let tradesw = Kx.(t9 (v timestamp) pairs (v sym) (v guid) (v guid) sides (v sym) (v float) (v float))

let uuid_of_int64 i =
  let buf = Bytes.make 16 '\x00' in
  EndianBytes.BigEndian.set_int64 buf 8 i ;
  Option.value_exn (Uuidm.of_bytes (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf))

let guids =
  Kx.(conv (Array.map ~f:uuid_of_int64) (fun _ -> assert false) (v guid))

let kx_of_fills trades =
  let len = List.length trades in
  let ids = Array.create ~len Uuidm.nil in
  let xchs = Array.create ~len "GIO" in
  let oids = Array.create ~len Uuidm.nil in
  let syms = Array.create ~len (Pair.create ~base:"" ~quote:"") in
  let times = Array.create ~len Ptime.epoch in
  let sides = Array.create ~len Fixtypes.Side.Buy in
  let pxs = Array.create ~len Float.nan in
  let qties = Array.create ~len Float.nan in
  let ordTypes = Array.create ~len "" in
  List.iteri trades ~f:begin fun i ({ id; orderid; pair; side; price; qty; time } : trade) ->
    times.(i) <- time ;
    ids.(i) <- uuid_of_int64 id ;
    oids.(i) <- uuid_of_int64 orderid ;
    syms.(i) <- pair ;
    sides.(i) <- side ;
    pxs.(i) <- price ;
    qties.(i) <- qty ;
  end ;
  Kx_async.create Kx.(t3 (a sym) (a sym) tradesw)
    ("upd", "trades", (times, syms, xchs, ids, oids, sides, ordTypes, pxs, qties))

let auth =
  { Fastrest.key = cfg.key ;
    secret = cfg.secret ;
    meta = [] }

let getFills w =
  Fastrest.request ~auth
    (Gateio_rest.trade_history {base="xtz"; quote="btc"}) >>= fun fills ->
  Pipe.write w (kx_of_fills fills) >>= fun () ->
  let len = List.length fills in
  Logs_async.app (fun m -> m "Found %d fills" len) >>= fun () ->
  Deferred.List.iter fills ~f:begin fun fill ->
    Log_async.app (fun m -> m "%a" pp_print_trade fill)
  end

let getLedgers _w =
  let rec inner _start =
    let latestTs = ref Ptime.epoch in
    Fastrest.request ~auth
      (Gateio_rest.entries ()) >>= fun { deposits; withdrawals } ->
    let len = List.(length deposits + length withdrawals) in
    (* Pipe.write w (kx_of_fills fills) >>= fun () -> *)
    Logs_async.app (fun m -> m "Found %d entries" len) >>= fun () ->
    Deferred.List.iter deposits ~f:begin fun e ->
      Log_async.app (fun m -> m "%a" pp_entry e)
    end >>= fun () ->
    Deferred.List.iter withdrawals ~f:begin fun e ->
      Log_async.app (fun m -> m "%a" pp_entry e)
    end >>= fun () ->
    if len > 0 then inner !latestTs else Deferred.unit
  in
  inner Ptime.epoch

let main () =
  Kx_async.with_connection url begin fun _ w ->
    getFills w >>= fun () ->
    getLedgers w
  end

let () =
  Command.async ~summary:"Gate.io kdb+ compta" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
