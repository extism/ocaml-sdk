module Manifest = Extism_manifest
module Error = Error
module Plugin = Plugin
module Function = Function
module Host_function = Host_function
module Type = Type
module Val = Val
module Val_type = Val_type

let extism_version = Bindings.extism_version

let with_plugin f p =
  Fun.protect ~finally:(fun () -> Plugin.free p) (fun () -> f p)

let%test "with_plugin" =
  let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
  let plugin = Plugin.of_manifest manifest |> Error.unwrap in
  let b =
    with_plugin
      (fun plugin ->
        Plugin.call Type.string Type.string plugin ~name:"count_vowels"
          "this is a test"
        |> Error.unwrap = "{\"count\":4,\"total\":4,\"vowels\":\"aeiouAEIOU\"}")
      plugin
  in
  Plugin.free plugin;
  Gc.minor ();
  Gc.full_major ();
  b

let%test _ = String.length (extism_version ()) > 0

let parse_level =
  Option.map (function
    | `Error -> "error"
    | `Warn -> "warn"
    | `Info -> "info"
    | `Debug -> "debug"
    | `Trace -> "trace"
    | `Filter f -> f)

let set_log_file ?level filename =
  Bindings.extism_log_file filename (parse_level level)

type drain_logs = unit -> unit

let mk_drain_logs f () =
  let fx s length =
    let s = Ctypes.string_from_ptr s ~length:(Ctypes.Uintptr.to_int length) in
    f s
  in
  Bindings.extism_log_drain fx

let set_log_custom ?level f =
  if Bindings.extism_log_custom (parse_level level) then
    let x = mk_drain_logs f in
    let () = Gc.finalise_last (mk_drain_logs f) x in
    x
  else Error.throw (`Msg "Unable to set custom logging")

let%test _ =
  let log_file =
    try String.length @@ Unix.getenv "TEST_LOG_FILE" > 0
    with Not_found -> false
  in
  if log_file then set_log_file ~level:`Trace "stderr"
  else
    let count = ref 0 in
    let _drain_logs =
      set_log_custom ~level:`Trace (fun s ->
          incr count;
          print_string s)
    in
    let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
    let plugin = Plugin.of_manifest manifest |> Error.unwrap in
    let _ =
      Plugin.call Type.string Type.string plugin ~name:"count_vowels"
        "this is a test"
    in
    (* Make sure logs are drained when drain_logs is garbage collected *)
    Gc.minor ();
    Gc.full_major ();
    !count > 0

let%test _ =
  let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
  let compiled = Plugin.Compiled.of_manifest_exn manifest in
  let plugin = Plugin.of_compiled_exn compiled in
  let _ =
    Plugin.call Type.string Type.string plugin ~name:"count_vowels"
      "this is a test"
  in
  Gc.minor ();
  Gc.full_major ();
  true
