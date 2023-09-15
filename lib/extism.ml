module Manifest = Extism_manifest
module Error = Error
module Plugin = Plugin
module Function = Function
module Host_function = Host_function
module Type = Type
module Val = Val

let extism_version = Bindings.extism_version

let with_plugin f p =
  Fun.protect ~finally:(fun () -> Plugin.free p) (fun () -> f p)

let%test "with_plugin" =
  let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
  let plugin = Plugin.of_manifest manifest |> Error.unwrap in
  let b =
    with_plugin
      (fun plugin ->
        Plugin.call plugin
          (module Type.String)
          (module Type.String)
          ~name:"count_vowels" "this is a test"
        |> Error.unwrap = "{\"count\": 4}")
      plugin
  in
  Plugin.free plugin;
  Gc.minor ();
  Gc.full_major ();
  b

let%test _ = String.length (extism_version ()) > 0

let set_log_file ?level filename =
  let level =
    Option.map
      (function
        | `Error -> "error"
        | `Warn -> "warn"
        | `Info -> "info"
        | `Debug -> "debug"
        | `Trace -> "trace")
      level
  in
  Bindings.extism_log_file filename level

let%test _ = set_log_file ~level:`Trace "stderr"
