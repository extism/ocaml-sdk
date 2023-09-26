module type S = sig
  type t

  val encode : t -> string
  val decode : Bigstringaf.t -> (t, Error.t) result
end

module Unit = struct
  type t = unit

  let encode () = ""
  let decode _ = Ok ()
end

let unit = (module Unit : S with type t = unit)

module Int32 = struct
  type t = int32

  let encode i =
    let b = Bytes.create 4 in
    Bytes.set_int32_le b 0 i;
    String.of_bytes b

  let decode bs = Ok (Bigstringaf.get_int32_le bs 0)
end

let int32 = (module Int32 : S with type t = Int32.t)

module Int64 = struct
  type t = int64

  let encode i =
    let b = Bytes.create 4 in
    Bytes.set_int64_le b 0 i;
    String.of_bytes b

  let decode bs = Ok (Bigstringaf.get_int64_le bs 0)
end

let int64 = (module Int64 : S with type t = Int64.t)

module Int = struct
  type t = int

  let encode i = Int64.encode (Stdlib.Int64.of_int i)
  let decode bs = Int64.decode bs |> Result.map Stdlib.Int64.to_int
end

let int = (module Int : S with type t = int)

module String = struct
  type t = string

  let encode = Fun.id
  let decode x = Ok (Bigstringaf.to_string x)
end

let string = (module String : S with type t = string)

module Bytes = struct
  type t = bytes

  let encode = Bytes.to_string

  let decode bs =
    let len = Bigstringaf.length bs in
    let bytes = Bytes.make len ' ' in
    Bigstringaf.blit_to_bytes bs ~src_off:0 bytes ~dst_off:0 ~len;
    Ok bytes
end

let bytes = (module Bytes : S with type t = bytes)

module Bigstring = struct
  type t = Bigstringaf.t

  let encode = Bigstringaf.to_string
  let decode bs = Ok bs
end

let bigstring = (module Bigstring : S with type t = Bigstringaf.t)

module Json = struct
  type t = Yojson.Safe.t

  let encode x = Yojson.Safe.to_string x

  let decode bs =
    try Ok (Yojson.Safe.from_string (Bigstringaf.to_string bs))
    with exc -> Error (`Msg (Printexc.to_string exc))
end

let json = (module Json : S with type t = Yojson.Safe.t)
