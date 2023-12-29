open Extism

let url =
  "https://github.com/extism/plugins/releases/latest/download/count_vowels.wasm"

module Typed_example = struct
  include Plugin.Typed.Init ()

  let count_vowels = fn_exn "count_vowels" Type.string Type.json
end

let () =
  let wasm = Manifest.Wasm.url url in
  let manifest = Manifest.create [ wasm ] in
  let plugin = Plugin.of_manifest_exn manifest in
  let plugin = Typed_example.of_plugin_exn plugin in
  let res = Typed_example.count_vowels plugin "this is a test" in
  print_endline (Yojson.Safe.to_string res)
