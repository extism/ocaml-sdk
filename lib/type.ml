module type S = sig
  type t

  val encode : t -> string
  val decode : Bigstringaf.t -> (t, Error.t) result
end

module String = struct
  type t = string

  let encode = Fun.id
  let decode x = Ok (Bigstringaf.to_string x)
end

module Bytes = struct
  type t = bytes

  let encode = Bytes.to_string

  let decode bs =
    let len = Bigstringaf.length bs in
    let bytes = Bytes.make len ' ' in
    Bigstringaf.blit_to_bytes bs ~src_off:0 bytes ~dst_off:0 ~len;
    Ok bytes
end

module Bigstring = struct
  type t = Bigstringaf.t

  let encode = Bigstringaf.to_string
  let decode bs = Ok bs
end

module Json = struct
  type t = Yojson.Safe.t

  let encode x = Yojson.Safe.to_string x

  let decode bs =
    try Ok (Yojson.Safe.from_string (Bigstringaf.to_string bs))
    with exc -> Error (`Msg (Printexc.to_string exc))
end