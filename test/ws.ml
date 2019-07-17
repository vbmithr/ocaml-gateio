open Core
open Async

open Gateio_ws

let src = Logs.Src.create "gateio.ws-test"
    ~doc:"Gateio API - WS test application"

let random_id () = Random.int64 10_000L

let process_user_cmd w =
  let process s =
    match String.split s ~on:' ' with
    | "untrades" :: chanid :: _ ->
      let chanid = Int64.of_string chanid in
      Pipe.write w (Unsubscribe (chanid, `Trades))
    | "unquotes" :: chanid :: _ ->
      let chanid = Int64.of_string chanid in
      Pipe.write w (Unsubscribe (chanid, `Quotes))
    | "ping" :: _ ->
      Pipe.write w (Ping (random_id ()))
    | "time" :: _ ->
      Pipe.write w (TimeReq (random_id ()))
    | "trades" :: pairs ->
      Pipe.write w (Subscribe (random_id (), Trades pairs))
    | "quotes" :: pairs ->
      let spec = List.map ~f:(fun p -> p, Thirty) pairs in
      Pipe.write w (Subscribe (random_id (), Quotes spec))
    | h :: _ ->
      Logs_async.err (fun m -> m "Unknown command %s" h)
    | [] ->
      Logs_async.err ~src (fun m -> m "Empty command")
  in
  let rec loop () = Reader.(read_line @@ Lazy.force stdin) >>= function
    | `Eof -> Deferred.unit
    | `Ok line -> process line >>= loop
  in
  loop ()

let main () =
  Gateio_ws_async.with_connection_exn begin fun r w ->
    let log_incoming msg =
      Logs_async.debug ~src (fun m -> m "%a" pp msg) in
    Deferred.all_unit [
      process_user_cmd w ;
      Pipe.iter r ~f:log_incoming
    ]
  end

let () =
  Command.async ~summary:"Kraken WS client" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
