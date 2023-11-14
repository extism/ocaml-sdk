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
    | `Trace -> "trace")

let set_log_file ?level filename =
  Bindings.extism_log_file filename (parse_level level)

let set_log_custom ?level () = Bindings.extism_log_custom (parse_level level)

let drain_logs f =
  let fx s length =
    let s = Ctypes.string_from_ptr s ~length:(Ctypes.Uintptr.to_int length) in
    f s
  in
  Bindings.extism_log_drain fx

let%test _ =
  let log_file =
    try String.length @@ Unix.getenv "TEST_LOG_FILE" > 0
    with Not_found -> false
  in
  if log_file then set_log_file ~level:`Trace "stderr"
  else
    let logs = ref [] in
    let ok = set_log_custom ~level:`Trace () in
    let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
    let plugin = Plugin.of_manifest manifest |> Error.unwrap in
    let _ =
      Plugin.call Type.string Type.string plugin ~name:"count_vowels"
        "this is a test"
    in
    let () = drain_logs (fun line -> logs := line :: !logs) in
    let () = Printf.printf "%d\n" (List.length !logs) in
    let () = List.iter print_string !logs in
    ok && List.length !logs > 0
