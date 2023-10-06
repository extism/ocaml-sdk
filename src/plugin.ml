module Manifest = Extism_manifest

type t = {
  free_lock : Mutex.t;
  mutable pointer : unit Ctypes.ptr;
  mutable functions : Function.t list;
}

let set_config plugin = function
  | None -> true
  | Some config ->
      let config =
        Extism_manifest.yojson_of_config config |> Yojson.Safe.to_string
      in
      Bindings.extism_plugin_config plugin.pointer config
        (Unsigned.UInt64.of_int (String.length config))

let free t =
  let () = Mutex.lock t.free_lock in
  Fun.protect
    ~finally:(fun () -> Mutex.unlock t.free_lock)
    (fun () ->
      if not (Ctypes.is_null t.pointer) then
        let () = Bindings.extism_plugin_free t.pointer in
        t.pointer <- Ctypes.null)

let strlen ptr =
  let rec aux ptr len =
    let c = Ctypes.( !@ ) ptr in
    if c = char_of_int 0 then len else aux (Ctypes.( +@ ) ptr 1) (len + 1)
  in
  aux ptr 0

let get_errmsg ptr =
  if Ctypes.is_null ptr then "Call failed"
  else
    let length = strlen ptr in
    let s = Ctypes.string_from_ptr ~length ptr in
    let () = Bindings.extism_plugin_new_error_free ptr in
    s

let create ?config ?(wasi = false) ?(functions = []) wasm =
  let func_ptrs = List.map (fun x -> x.Function.pointer) functions in
  let arr = Ctypes.CArray.of_list Ctypes.(ptr void) func_ptrs in
  let n_funcs = Ctypes.CArray.length arr in
  let errmsg =
    Ctypes.(allocate (ptr char) (coerce (ptr void) (ptr char) null))
  in
  let pointer =
    Bindings.extism_plugin_new wasm
      (Unsigned.UInt64.of_int (String.length wasm))
      (Ctypes.CArray.start arr)
      (Unsigned.UInt64.of_int n_funcs)
      wasi errmsg
  in
  if Ctypes.is_null pointer then
    let s = get_errmsg (Ctypes.( !@ ) errmsg) in
    Error (`Msg s)
  else
    let t = { pointer; functions; free_lock = Mutex.create () } in
    let () = Gc.finalise_last (fun () -> free t) t in
    if not (set_config t config) then Error (`Msg "call to set_config failed")
    else Ok t

let create_exn ?config ?wasi ?functions wasm =
  create ?config ?wasi ?functions wasm |> Error.unwrap

let of_manifest ?wasi ?functions manifest =
  let data = Manifest.to_json manifest in
  create ?wasi ?functions data

let of_manifest_exn ?wasi ?functions manifest =
  of_manifest ?wasi ?functions manifest |> Error.unwrap

let%test "free plugin" =
  let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
  let plugin = of_manifest manifest |> Error.unwrap in
  free plugin;
  true

let call' f { pointer; _ } ~name input len =
  if Ctypes.is_null pointer then Error.throw (`Msg "Plugin already freed")
  else
    let rc = f pointer name input len in
    if rc <> 0l then
      match Bindings.extism_error pointer with
      | None -> Error (`Msg "extism_plugin_call failed")
      | Some msg -> Error (`Msg msg)
    else
      let out_len = Bindings.extism_plugin_output_length pointer in
      let ptr = Bindings.extism_plugin_output_data pointer in
      let buf =
        Ctypes.bigarray_of_ptr Ctypes.array1
          (Unsigned.UInt64.to_int out_len)
          Char ptr
      in
      Ok buf

let call_bigstring (t : t) ~name input =
  let len = Unsigned.UInt64.of_int (Bigstringaf.length input) in
  let ptr = Ctypes.bigarray_start Ctypes.array1 input in
  call' Bindings.extism_plugin_call t ~name ptr len

let call_bigstring_exn t ~name input =
  call_bigstring t ~name input |> Error.unwrap

let%test "call_bigstring" =
  let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
  let plugin = of_manifest manifest |> Error.unwrap in
  call_bigstring plugin ~name:"count_vowels"
    (Bigstringaf.of_string ~off:0 ~len:14 "this is a test")
  |> Error.unwrap |> Bigstringaf.to_string = "{\"count\": 4}"

let call_string (t : t) ~name input =
  let len = String.length input in
  call' Bindings.extism_plugin_call_s t ~name input (Unsigned.UInt64.of_int len)
  |> Result.map Bigstringaf.to_string

let call_string_exn t ~name input = call_string t ~name input |> Error.unwrap

let call (type a b) (module In : Type.S with type t = a)
    (module Out : Type.S with type t = b) t ~name (a : a) : (b, Error.t) result
    =
  let input = In.encode a in
  let len = String.length input in
  match
    call' Bindings.extism_plugin_call_s t ~name input
      (Unsigned.UInt64.of_int len)
  with
  | Ok x -> Out.decode x
  | Error e -> Error e

let call_exn a b t ~name input = call a b t ~name input |> Error.unwrap

let%test "call" =
  let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
  let plugin = of_manifest manifest |> Error.unwrap in
  call Type.string Type.string plugin ~name:"count_vowels" "this is a test"
  |> Error.unwrap = "{\"count\": 4}"

let%test "call_functions" =
  let open Val_type in
  let hello_world =
    Function.create "hello_world" ~params:[ I64 ] ~results:[ I64 ]
      ~user_data:"Hello again!"
    @@ fun plugin user_data ->
    let s = Host_function.input Type.string plugin |> Result.get_ok in
    let () = print_endline "Hello from OCaml!" in
    let () = print_endline user_data in
    let () = print_endline s in
    Host_function.output Type.json plugin (`Assoc [ ("count", `Int 999) ])
  in
  let functions = [ hello_world ] in
  let manifest = Manifest.(create [ Wasm.file "test/code-functions.wasm" ]) in
  let plugin = of_manifest manifest ~functions ~wasi:true |> Error.unwrap in
  let b =
    call Type.string Type.string plugin ~name:"count_vowels" "this is a test"
    |> Error.unwrap = "{\"count\":999}"
  in
  Gc.minor ();
  Gc.full_major ();
  b

let function_exists { pointer; _ } name =
  if Ctypes.is_null pointer then Error.throw (`Msg "Plugin already freed")
  else Bindings.extism_plugin_function_exists pointer name

let%test "function exists" =
  let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
  let plugin = of_manifest manifest |> Error.unwrap in
  function_exists plugin "count_vowels"
  && not (function_exists plugin "function_does_not_exist")

module Cancel_handle = struct
  type t = { inner : unit Ctypes.ptr }

  let cancel { inner } = Bindings.extism_plugin_cancel inner
end

let cancel_handle { pointer; _ } =
  if Ctypes.is_null pointer then Error.throw (`Msg "Plugin already freed")
  else Cancel_handle.{ inner = Bindings.extism_plugin_cancel_handle pointer }

let%test "cancel handle" =
  let manifest = Manifest.(create [ Wasm.file "test/loop.wasm" ]) in
  let plugin = of_manifest manifest |> Error.unwrap in
  let handle = cancel_handle plugin in
  let thr =
    Thread.create
      (fun () ->
        Thread.delay 1.0;
        Cancel_handle.cancel handle)
      ()
  in
  let _ = call Type.unit Type.unit plugin ~name:"infinite_loop" () in
  Thread.join thr;
  true

let id { pointer; _ } =
  if Ctypes.is_null pointer then Error.throw (`Msg "Plugin already freed")
  else
    let id = Bindings.extism_plugin_id pointer in
    let s = Ctypes.string_from_ptr id ~length:16 in
    Uuidm.unsafe_of_bytes s
