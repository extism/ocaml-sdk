# Extism

Extism Host SDK for OCaml

## Installation

First, [install the Extism runtime](https://extism.org/docs/install/).

Then add `extism` to your `dune` depdendencies:

```
  (libraries extism)
```

## Example

```ocaml
open Extism

let () =
  let manifest = Manifest.create [Wasm.file "wasm/code.wasm"]
  let plugin = Plugin.of_manifest manifest |> Error.unwrap in
  let output = Plugin.call plugin ~name:"count_vowels" "this is a test" |> Error.unwrap in
  print_endline output
```
