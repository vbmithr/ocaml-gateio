open Core
open Async

open Gateio
open Gateio_ws

let src = Logs.Src.create "gateio.ws-test"
    ~doc:"Gateio API - WS test application"

let new_id =
  let c = ref 0L in
  fun () -> let ret = !c in c := Int64.succ !c ; ret

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
      Pipe.write w (Ping (new_id ()))
    | "time" :: _ ->
      Pipe.write w (TimeReq (new_id ()))
    | "trades" :: pairs ->
      let pairs = List.map pairs ~f:Pair.of_string_exn in
      Pipe.write w (Subscribe (new_id (), Trades pairs))
    | "quotes" :: pairs ->
      let spec = List.map ~f:(fun p -> Pair.of_string_exn p, Thirty) pairs in
      Pipe.write w (Subscribe (new_id (), Quotes spec))
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
  Fastws_async.with_connection url ~of_string ~to_string begin fun _ r w ->
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
      let () = Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
