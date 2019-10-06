open Core
open Async

open Gateio_ws

val connect : ?ping:Time_ns.Span.t -> unit ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t) Deferred.Or_error.t

val connect_exn : ?ping:Time_ns.Span.t -> unit ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t) Deferred.t

val with_connection :
  ?ping:Time_ns.Span.t ->
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.Or_error.t

val with_connection_exn :
  ?ping:Time_ns.Span.t ->
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) -> 'a Deferred.t
