open Core
open Async

open Gateio
open Gateio_ws

let url = Uri.make ~scheme:"https" ~host:"ws.gateio.ws" ~path:"v3/" ()

let src = Logs.Src.create "gateio.ws.async"
module Log = (val Logs.src_log src : Logs.LOG)
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let string_of_json encoding cmd =
  match Ezjsonm_encoding.construct encoding cmd with
  | `A _ | `O _ as a -> Ezjsonm.to_string a
  | _ -> invalid_arg "not a json document"

let incr64 r = r := Int64.succ !r

let connect ?ping () =
  Deferred.Or_error.map (Fastws_async.EZ.connect url)
    ~f:begin fun { r; w; cleaned_up } ->
      let pingid = ref Int64.(100_000L + (Random.int64 100_000L)) in
      let client_read = Pipe.map r ~f:begin fun msg ->
          Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
        end in
      let ws_read, client_write = Pipe.create () in
      don't_wait_for
        (Pipe.closed client_write >>| fun () -> Pipe.close w) ;
      begin match ping with
        | None -> ()
        | Some span ->
          Clock_ns.every' span ~stop:(Pipe.closed w) begin fun () ->
            let ping = string_of_json encoding (Ping !pingid) in
            incr64 pingid ;
            Log_async.debug (fun m -> m "-> %s" ping) >>= fun () ->
            Pipe.write w ping
          end
      end ;
      don't_wait_for @@
      Pipe.transfer ws_read w ~f:begin fun cmd ->
        let doc = string_of_json encoding cmd in
        Log.debug (fun m -> m "-> %s" doc) ;
        doc
      end ;
      (client_read, client_write, cleaned_up)
    end

let connect_exn ?ping () =
  connect ?ping () >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection ?ping f =
  Fastws_async.EZ.with_connection url ~f:begin fun r w ->
    let pingid = ref Int64.(100_000L + (Random.int64 100_000L)) in
    begin match ping with
      | None -> () ;
      | Some span ->
        Clock_ns.every' span ~stop:(Pipe.closed w) begin fun () ->
          let ping = string_of_json encoding (Ping !pingid) in
          incr64 pingid ;
          Log_async.debug (fun m -> m "-> %s" ping) >>= fun () ->
          Pipe.write w ping
        end
    end ;
    let r = Pipe.map r ~f:begin fun msg ->
        Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
      end in
    let ws_read, client_write = Pipe.create () in
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc = string_of_json encoding cmd in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end ;
    Monitor.protect
      (fun () -> f r client_write)
      ~finally:(fun () -> Pipe.close_read ws_read ; Deferred.unit)
  end

let with_connection_exn ?ping f =
  with_connection ?ping f >>= function
  | Error e -> Error.raise e
  | Ok a -> return a
