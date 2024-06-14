open Extism

let url =
  "https://github.com/extism/plugins/releases/latest/download/count_vowels.wasm"

module Typed_example = struct
  include Plugin.Typed.Init ()

  let count_vowels = exn @@ fn "count_vowels" Type.string Type.json
end

let run_thread pool ~sleep ~key ~input =
  Thread.create
    (fun () ->
      Thread.delay sleep;
      let plugin = Pool.get pool key in
      Pool.Instance.use plugin @@ fun p ->
      let plugin = Typed_example.of_plugin_exn p in
      let res = Typed_example.count_vowels plugin input in
      print_endline (Yojson.Safe.to_string res))
    ()

let () =
  let wasm = Manifest.Wasm.url url in
  let manifest = Manifest.create [ wasm ] in
  let pool = Pool.create 3 in
  Pool.add pool "a" (fun () -> Plugin.of_manifest_exn manifest);
  let threads =
    List.init 10 (fun i ->
        run_thread pool ~sleep:(float_of_int i *. 0.25) ~key:"a" ~input:"a")
  in
  List.iter Thread.join threads
