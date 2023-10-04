# Extism OCaml Host SDK

This repo contains the OCaml package for integrating with the [Extism](https://extism.org/) runtime.

> **Note**: If you're unsure what Extism is or what an SDK is see our homepage: [https://extism.org](https://extism.org).

> **Note**: This repo is 1.0 alpha version of the OCaml SDK and is a work in progress. We'd love any feedback you have on it, but consider using the supported OCaml SDK in [extism/extism](https://github.com/extism/extism/tree/main/ocaml) until we hit 1.0.

## Documentation

Documentation is available at [https://extism.github.io/ocaml-sdk](https://extism.github.io/ocaml-sdk)

## Installation

### Install the Extism Runtime Dependency

For this library, you first need to install the Extism Runtime. You can [download the shared object directly from a release](https://github.com/extism/extism/releases) or use the [Extism CLI](https://github.com/extism/cli) to install it.

> **Note**: This library has breaking changes and targets 1.0 of the runtime. For the time being, install the runtime from our nightly development builds on git: `sudo PATH="$PATH" env extism lib install --version git`.

### Add the library to dune

Then add `extism` to your [dune](https://dune.build) depdendencies:

```
  (libraries extism)
```

If you're generating an opam file using dune then add `extism` to your `dune-project` package `depends` section:

```
(package
 (depends
  (extism (>= 1.0.0))))
```

Installing the `extism` package on opam will also install the `extism-call` executable, which can be used
to execute Extism plugins.

## Example

```ocaml
open Extism

(* Define a host function *)
let hello_world =
  let open Val_type in
  Function.create "hello_world" ~params:[ I64 ] ~results:[ I64 ]
    ~user_data:"Hello again!"
  @@ fun plugin user_data ->

  (* Get the input *)
  let s = Host_function.input Type.string plugin in
  print_endline ("Original input: " ^ Error.unwrap s);

  (* Print the userdata *)
  let () = print_endline user_data in

  (* Return a JSON value *)
  Host_function.output Type.json plugin (`Assoc [ ("count", `Int 999) ])

let () =
  (* Create a manifest using a file from disk *)
  let manifest = Manifest.(create [ Wasm.file "wasm/code-functions.wasm" ]) in

  (* Create a plugin from the manifest with the [hello_world] host function *)
  let plugin = Plugin.of_manifest manifest ~functions:[hello_world] ~wasi:true |> Error.unwrap in

  (* Call [count_vowels] *)
  let output = Plugin.call_string plugin ~name:"count_vowels" "this is a test" |> Error.unwrap in

  (* Print the result *)
  print_endline output
```
