open Extism

(** Loads an entire directory of Extism plugins and allows them to be called by
    name. The [count_vowels] function in [wasm/code.wasm] can be called from the
    root of the project using:
    [dune exec ./examples/runner.exe wasm code count_vowels "this is a test"] *)

(** Create some host functions just to remain compatible with the wasm examples
    in this repo, both just return the input as-is *)
let functions =
  let id plugin () =
    let v = Host_function.param plugin 0 in
    Host_function.set_result plugin 0 v
  in
  let params = [ Val_type.ptr ] in
  let results = [ Val_type.ptr ] in
  [
    Function.create "hello_world" ~user_data:() ~params ~results id;
    Function.create "transform_string" ~user_data:() ~params ~results id;
  ]

(** Load all wasm plugins from a path, this returns the plugin name (the
    original filename without the extension) and the loaded plugin *)
let load_plugins path : (string * Plugin.t) Seq.t =
  Sys.readdir path |> Array.to_seq
  (* Find .wasm files *)
  |> Seq.filter (function
       | "." | ".." -> false
       | x -> Filename.check_suffix x ".wasm")
  (* Load the Extism plugins *)
  |> Seq.map (fun filename ->
         let name = Filename.chop_extension filename in
         let path = Filename.concat path filename in
         let manifest = Manifest.create [ Manifest.Wasm.file path ] in
         let plugin = Plugin.of_manifest_exn ~functions ~wasi:true manifest in
         (name, plugin))

(** Handle command line arguments *)
let () =
  if Array.length Sys.argv < 4 then
    Printf.eprintf
      "%s <DIR> <PLUGIN> <FUNCTION> [INPUT]\n\
       For example: `%s wasm code count_vowels \"this is a test\"`\n"
      Sys.argv.(0) Sys.argv.(0)
  else
    let path = Sys.argv.(1) in
    let plugin_name = Sys.argv.(2) in
    let function_name = Sys.argv.(3) in
    let input = if Array.length Sys.argv > 4 then Sys.argv.(4) else "" in
    let plugins = Hashtbl.of_seq (load_plugins path) in
    let plugin = Hashtbl.find plugins plugin_name in
    Plugin.call_string_exn plugin ~name:function_name input |> print_endline
