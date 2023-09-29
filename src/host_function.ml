open Ctypes

type t = { pointer : unit ptr; params : Val.Array.t; results : Val.Array.t }

let params { params; _ } = params
let results { results; _ } = results
let param { params; _ } index = CArray.get params index
let result { results; _ } index = CArray.get results index
let set_result { results; _ } index v = CArray.set results index v

type memory_handle = { offs : Unsigned.UInt64.t; len : Unsigned.UInt64.t }

module Memory_handle = struct
  let memory ?(offs = Unsigned.UInt64.zero) t =
    Bindings.extism_current_plugin_memory t.pointer
    +@ Unsigned.UInt64.to_int offs

  let find t offs =
    let len = Bindings.extism_current_plugin_memory_length t.pointer offs in
    if Unsigned.UInt64.(equal zero len) then None else Some { offs; len }

  let alloc t len =
    let len = Unsigned.UInt64.of_int len in
    let offs = Bindings.extism_current_plugin_memory_alloc t.pointer len in
    { offs; len }

  let free t { offs; _ } =
    Bindings.extism_current_plugin_memory_free t.pointer offs

  let of_val t v =
    match Val.to_i64 v with
    | None -> None
    | Some v ->
        let offs = Unsigned.UInt64.of_int64 v in
        find t offs

  let of_val_exn t v =
    match of_val t v with
    | None -> invalid_arg "Memory_block.of_val_exn"
    | Some v -> v

  let to_val { offs; len = _ } = Val.of_i64 (Unsigned.UInt64.to_int64 offs)

  let get_bigstring t { offs; len } : Bigstringaf.t =
    let p = memory t ~offs in
    bigarray_of_ptr array1
      (Unsigned.UInt64.to_int len)
      Bigarray.Char
      (coerce (ptr uint8_t) (ptr char) p)

  let get_string t { offs; len } =
    let p = memory t ~offs in
    Ctypes.string_from_ptr
      (coerce (ptr uint8_t) (ptr char) p)
      ~length:(Unsigned.UInt64.to_int len)

  let set_bigstring t { offs; len } bs =
    let length = min (Unsigned.UInt64.to_int @@ len) (Bigstringaf.length bs) in
    let p = coerce (ptr uint8_t) (ptr char) @@ memory t ~offs in
    let x =
      bigarray_of_ptr array1 (Unsigned.UInt64.to_int len) Bigarray.Char p
    in
    Bigstringaf.blit bs ~src_off:0 x ~dst_off:0 ~len:length

  let set_string t { offs; len } s =
    let length = min (Unsigned.UInt64.to_int @@ len) (String.length s) in
    let p = coerce (ptr uint8_t) (ptr char) @@ memory t ~offs in
    let x =
      bigarray_of_ptr array1 (Unsigned.UInt64.to_int len) Bigarray.Char p
    in
    Bigstringaf.blit_from_string s ~src_off:0 x ~dst_off:0 ~len:length
end

let output_string t ?(index = 0) s =
  let mem = Memory_handle.alloc t (String.length s) in
  Memory_handle.set_string t mem s;
  Val.Array.(
    t.results.$[index] <- Val.of_i64 (Unsigned.UInt64.to_int64 mem.offs))

let output_bigstring t ?(index = 0) s =
  let mem = Memory_handle.alloc t (Bigstringaf.length s) in
  Memory_handle.set_bigstring t mem s;
  Val.Array.(
    t.results.$[index] <- Val.of_i64 (Unsigned.UInt64.to_int64 mem.offs))

let input_string ?(index = 0) t =
  let inp = Val.Array.(t.params.$[index]) in
  let mem = Memory_handle.of_val_exn t inp in
  Memory_handle.get_string t mem

let input_bigstring ?(index = 0) t =
  let inp = Val.Array.(t.params.$[index]) in
  let mem = Memory_handle.of_val_exn t inp in
  Memory_handle.get_bigstring t mem

let output (type a) t ?index (module C : Type.S with type t = a) (a : a) =
  let s = C.encode a in
  output_string t ?index s

let input (type a) t ?index (module C : Type.S with type t = a) =
  let bs = input_bigstring ?index t in
  C.decode bs
