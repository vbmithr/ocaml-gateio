module Encoding : sig
  val strfl : float Json_encoding.encoding
  (** [flstring] is an encoder for a float encoded as a string *)

  val strint : int64 Json_encoding.encoding
  (** [intstring] is an encoder for a int encoded as a string *)

  (* val polo_bool : bool Json_encoding.encoding
   * (\** [bool] is an encoder for a bool. *\) *)
end

module Pair : sig
  type t = {
    base: string ;
    quote: string ;
  } [@@deriving sexp]

  module Set : Set.S with type elt := t
  module Map : Map.S with type key := t
  module Table : Hashtbl.S with type key := t

  val create : base:string -> quote:string -> t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_string : string -> t option
  val of_string_exn : string -> t
  val encoding : t Json_encoding.encoding
end

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span

  val t_of_sexp : Sexplib.Sexp.t -> Ptime.t
  val sexp_of_t : Ptime.t -> Sexplib.Sexp.t
  val encoding : t Json_encoding.encoding
end
