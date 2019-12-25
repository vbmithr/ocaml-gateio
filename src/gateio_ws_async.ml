open Core
open Async

open Gateio
open Gateio_ws

let src = Logs.Src.create "gateio.ws.async"
module Log = (val Logs.src_log src : Logs.LOG)
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let string_of_json encoding cmd =
  match Ezjsonm_encoding.construct encoding cmd with
  | `A _ | `O _ as a -> Ezjsonm.to_string a
  | _ -> invalid_arg "not a json document"

let incr64 r = r := Int64.succ !r

module T = struct
  type t = {
    r: Gateio_ws.t Pipe.Reader.t ;
    w: Gateio_ws.t Pipe.Writer.t ;
  }

  let create r w = { r; w }

  module Address = Uri_sexp

  let is_closed { r; w } = Pipe.(is_closed r && is_closed w)
  let close { r; w } =
    Pipe.close w ;
    Pipe.close_read r ;
    Deferred.unit
  let close_finished { r; w } =
    Deferred.all_unit [Pipe.closed r;
                       Pipe.closed w]
end
include T

let mk_client_read r =
  Pipe.map r ~f:begin fun msg ->
    Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
  end

let mk_client_write w =
  Pipe.create_writer begin fun ws_read ->
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc = string_of_json encoding cmd in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end
  end

let launch_ping w = function
  | None -> ()
  | Some span ->
    let pingid = ref Int64.(100_000L + (Random.int64 100_000L)) in
    Clock_ns.every' span ~stop:(Pipe.closed w) begin fun () ->
      let ping = string_of_json encoding (Ping !pingid) in
      incr64 pingid ;
      Log_async.debug (fun m -> m "-> %s" ping) >>= fun () ->
      Pipe.write w ping
    end

let connect ?ping url =
  Deferred.Or_error.map (Fastws_async.EZ.connect url)
    ~f:begin fun { r; w; _ } ->
      let client_write = mk_client_write w in
      (Pipe.closed client_write >>> fun () -> Pipe.close w) ;
      launch_ping w ping ;
      create (mk_client_read r) client_write
    end

module Persistent = struct
  include Persistent_connection_kernel.Make(T)

  let create' ~server_name ?on_event ?retry_delay =
    create ~server_name ?on_event ?retry_delay ~connect
end

let connect_exn ?ping url =
  connect ?ping url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection ?ping ~f url =
  Fastws_async.EZ.with_connection url ~f:begin fun r w ->
    launch_ping w ping ;
    f (mk_client_read r) (mk_client_write w)
  end

let with_connection_exn ?ping ~f url =
  with_connection ?ping ~f url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a
