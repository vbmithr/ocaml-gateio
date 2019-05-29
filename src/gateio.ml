open Sexplib.Std

module Pair = struct
  type t = {
    base: string ;
    quote: string ;
  } [@@deriving sexp]

  let compare { base ; quote } { base = base' ; quote = quote' } =
    match String.compare base base' with
    | 0 -> String.compare quote quote'
    | n -> n

  let pp ppf { base ; quote } =
    Format.fprintf ppf "%s_%s" base quote

  let to_string { base ; quote } =
    base ^ "_" ^ quote

  let of_string s =
    match String.split_on_char '_' s with
    | [base ; quote] -> Some { base ; quote }
    | _ -> None

  let of_string_exn s =
    match String.split_on_char '_' s with
    | [base ; quote] -> { base ; quote }
    | _ -> invalid_arg "pair_of_string_exn"

  let encoding =
    let open Json_encoding in
    conv to_string of_string_exn string
end

module Encoding = struct
  open Json_encoding

  let strfl =
    union [
      case float (fun a -> Some a) (fun a -> a) ;
      case string (fun a -> Some (string_of_float a)) (fun a -> float_of_string a) ;
    ]

  let strint =
    union [
      case int53 (fun i -> Some i) (fun t -> t) ;
      case string (fun i -> Some (Int64.to_string i)) Int64.of_string
    ]

  (* let polo_bool =
   *   union [
   *     case bool (fun b -> Some b) (fun t -> t) ;
   *     case int
   *       (function true -> Some 1 | false -> Some 0)
   *       (function 0 -> false | _ -> true) ;
   *     case string
   *       (function true -> Some "1" | false -> Some "0")
   *       (function "0" -> false | _ -> true) ;
   *   ] *)
end

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)

  let encoding =
    let open Json_encoding in
    conv
      Ptime.to_float_s
      (fun ts -> match Ptime.of_float_s ts with
         | None -> invalid_arg "Ptime.encoding"
         | Some ts -> ts)
      Encoding.strfl
end
