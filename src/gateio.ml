open Sexplib.Std

module Ezjsonm_encoding = struct
  include Json_encoding.Make(Json_repr.Ezjsonm)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      Format.eprintf "%a@."
        (Json_encoding.print_error ?print_unknown:None) exn ;
      raise exn
end

module Pair = struct
  module T = struct
    type t = {
      base: string ;
      quote: string ;
    } [@@deriving sexp]

    let create ~base ~quote = { base; quote }

    let compare { base ; quote } { base = base' ; quote = quote' } =
      match String.compare base base' with
      | 0 -> String.compare quote quote'
      | n -> n

    let equal a b = compare a b = 0
    let hash = Hashtbl.hash
  end
  include T
  module Set = Set.Make(T)
  module Map = Map.Make(T)
  module Table = Hashtbl.Make(T)

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
      case int53 (fun a -> Some (Int64.of_float a)) (fun a -> Int64.to_float a) ;
      case string (fun a -> Some (string_of_float a)) (fun a -> float_of_string a) ;
    ]

  let strint =
    union [
      case int53 (fun i -> Some i) (fun t -> t) ;
      case string (fun i -> Some (Int64.to_string i)) Int64.of_string
    ]
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
