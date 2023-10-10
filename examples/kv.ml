(* $MDX part-begin=hostFnIntro *)
open Extism

let url =
  "https://github.com/extism/plugins/releases/latest/download/count_vowels_kvstore.wasm"

let wasm = Manifest.Wasm.url url
let manifest = Manifest.create [ wasm ]
(* $MDX part-end *)

(* $MDX part-begin=hostFnDef *)
let make_kv_plugin () =
  (* pretend this is Redis or something :) *)
  let kv_store = Hashtbl.create 8 in

  let kv_read =
    let open Val_type in
    Function.create "kv_read" ~params:[ I64 ] ~results:[ I64 ] ~user_data:()
    @@ fun plugin () ->
    let key = Host_function.input_string plugin in
    Printf.printf "Reading from key=%s\n" key;
    let value =
      try Hashtbl.find kv_store key
      with Not_found -> String.init 4 (fun _ -> char_of_int 0)
    in
    Host_function.output_string plugin value
  in

  let kv_write =
    let open Val_type in
    Function.create "kv_write" ~params:[ I64; I64 ] ~results:[] ~user_data:()
    @@ fun plugin () ->
    print_endline "AAA";
    let key = Host_function.input_string ~index:0 plugin in
    let value = Host_function.input_string ~index:1 plugin in
    Printf.printf "Write value=%s to key=%s\n" value key;
    Hashtbl.replace kv_store key value
  in

  (* Create a plugin from the manifest with the kv host functions *)
  Plugin.of_manifest_exn ~functions:[ kv_read; kv_write ] ~wasi:true manifest
(* $MDX part-end *)

let () =
  let plugin = make_kv_plugin () in
  let () =
    Plugin.call_string_exn plugin ~name:"count_vowels" "Hello, world"
    |> print_endline
  in
  Plugin.call_string_exn plugin ~name:"count_vowels" "Hello, world"
  |> print_endline
