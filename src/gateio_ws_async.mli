open Core
open Async

open Gateio_ws

val connect : ?ping:Time_ns.Span.t -> unit ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t,
   [ `Internal of exn | `WS of Fastws_async.error ]) result Deferred.t

val connect_exn : ?ping:Time_ns.Span.t -> unit ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t) Deferred.t

val with_connection :
  ?ping:Time_ns.Span.t ->
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) ->
  ('a, [ `Internal of exn
       | `User_callback of exn | `WS of Fastws_async.error ]) result Deferred.t

val with_connection_exn :
  ?ping:Time_ns.Span.t ->
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) -> 'a Deferred.t
