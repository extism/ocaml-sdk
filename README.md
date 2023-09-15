# Extism

Extism Host SDK for OCaml

## Documentation

Documentation is available at [https://extism.github.io/ocaml-sdk](https://extism.github.io/ocaml-sdk)

## Installation

First, [install the Extism runtime](https://extism.org/docs/install/).

Then add `extism` to your `dune` depdendencies:

```
  (libraries extism)
```

## Example

```ocaml
open Extism

(* Define a host function *)
let hello_world =
  let open Val.Type in
  Function.create "hello_world" ~params:[ I64 ] ~results:[ I64 ]
    ~user_data:"Hello again!"
  @@ fun plugin user_data ->

  (* Get the input *)
  let s = Host_function.input plugin (module Type.String) in
  print_endline ("Original input: " ^ s);

  (* Print the userdata *)
  let () = print_endline user_data in

  (* Return a JSON value *)
  Host_function.return plugin (module Type.Json) (`Assoc [ ("count", `Int 999) ])

let () =
  (* Create a manifest using a file from disk *)
  let manifest = Manifest.(create [ Wasm.file "test/code-functions.wasm" ]) in

  (* Create a plugin from the manifest with the [hello_world] host function *)
  let plugin = Plugin.of_manifest manifest ~functions:[hello_world] ~wasi:true |> Error.unwrap in

  (* Call [count_vowels] *)
  let output = Plugin.call_string plugin ~name:"count_vowels" "this is a test" in

  (* Print the result *)
  print_endline output
```
