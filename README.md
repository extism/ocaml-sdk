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

## Getting Started

This guide should walk you through some of the concepts in Extism and the OCaml bindings.

### Creating A Plug-in

The primary concept in Extism is the [plug-in](https://extism.org/docs/concepts/plug-in). You can think of a plug-in as a code module stored in a `.wasm` file.

Since you may not have an Extism plug-in on hand to test, let's load a demo plug-in from the web:

```ocaml
open Extism

let wasm = Manifest.Wasm.url "https://github.com/extism/plugins/releases/latest/download/count_vowels.wasm"
let manifest = Manifest.create [wasm]
let plugin = Plugin.of_manifest_exn manifest
```

> **Note**: See [the Manifest docs](https://extism.github.io/ocaml-sdk/extism-manifest/Extism_manifest/index.html) as it has a rich schema and a lot of options.

### Calling A Plug-in's Exports

This plug-in was written in Rust and it does one thing, it counts vowels in a string. As such, it exposes one "export" function: `count_vowels`. We can call exports using [Extism.Plugin.call](https://extism.github.io/ocaml-sdk/extism/Extism/Plugin/index.html#val-call):

```ocaml
# Plugin.call_string_exn plugin ~name:"count_vowels" "Hello, world!";;
- : string = "{\"count\":3,\"total\":3,\"vowels\":\"aeiouAEIOU\"}"
```

All exports have a simple interface of bytes-in and bytes-out. This plug-in happens to take a string and return a JSON encoded string with a report of results.

This library also allows for calls to be typed, when the input and output types are not strings. Instead of getting the output as a JSON encoded string, we can
convert it directly to `Yojson.Safe.t`:

```ocaml
# Plugin.call_exn Type.string Type.json plugin ~name:"count_vowels" "Hello, world!";;
- : Yojson.Safe.t =
`Assoc
  [("count", `Int 3); ("total", `Int 6); ("vowels", `String "aeiouAEIOU")]
```

See [Extism.Type.S](https://extism.github.io/ocaml-sdk/extism/Extism/Type/module-type-S/index.html) to define your own input/output types.

### Plug-in State

Plug-ins may be stateful or stateless. Plug-ins can maintain state b/w calls by the use of variables. Our count vowels plug-in remembers the total number of vowels it's ever counted in the "total" key in the result. You can see this by making subsequent calls to the export:

```ocaml
# Plugin.call_string_exn plugin ~name:"count_vowels" "Hello, world!" |> print_endline;;
{"count":3,"total":9,"vowels":"aeiouAEIOU"}
- : unit = ()
# Plugin.call_string_exn plugin ~name:"count_vowels" "Hello, world!" |> print_endline;;
{"count":3,"total":12,"vowels":"aeiouAEIOU"}
- : unit = ()
```

These variables will persist until this plug-in is freed or you initialize a new one.

### Configuration

Plug-ins may optionally take a configuration object. This is a static way to configure the plug-in. Our count-vowels plugin takes an optional configuration to change out which characters are considered vowels. Example:

```ocaml
# let manifest = Manifest.create [wasm];;
val manifest : Extism_manifest.t =
  {Extism.Manifest.wasm =
    [Extism.Manifest.Wasm.Url
      {Extism.Manifest.Wasm.url =
        "https://github.com/extism/plugins/releases/latest/download/count_vowels.wasm";
       headers = None; meth = None; name = None; hash = None}];
   memory = None; config = None; allowed_hosts = None; allowed_paths = None;
   timeout_ms = None}

# let plugin = Plugin.of_manifest_exn manifest;;
val plugin : Plugin.t = <abstr>
# Plugin.call_string_exn plugin ~name:"count_vowels" "Yellow, world!" |> print_endline;;
{"count":3,"total":3,"vowels":"aeiouAEIOU"}
- : unit = ()

# let plugin = Plugin.of_manifest_exn @@ Manifest.with_config ["vowels", Some "aeiouAEIOUY"] manifest;;
val plugin : Plugin.t = <abstr>
# Plugin.call_string_exn plugin ~name:"count_vowels" "Yellow, world!" |> print_endline;;
{"count":4,"total":4,"vowels":"aeiouAEIOUY"}
- : unit = ()
```

### Host Functions

Let's extend our count-vowels example a little bit: Instead of storing the `total` in an ephemeral plug-in var, let's store it in a persistent key-value store!

Wasm can't use our KV store on it's own. This is where [Host Functions](https://extism.org/docs/concepts/host-functions) come in.

[Host functions](https://extism.org/docs/concepts/host-functions) allow us to grant new capabilities to our plug-ins from our application. They are simply some OCaml functions you write which can be passed down and invoked from any language inside the plug-in.

Let's load the manifest like usual but load up this `count_vowels_kvstore` plug-in:

<!-- $MDX env=host-functions -->
```ocaml
open Extism

let url = "https://github.com/extism/plugins/releases/latest/download/count_vowels_kvstore.wasm"
let wasm = Manifest.Wasm.url url
let manifest = Manifest.create [wasm]
```

> *Note*: The source code for this is [here](https://github.com/extism/plugins/blob/main/count_vowels_kvstore/src/lib.rs) and is written in rust, but it could be written in any of our PDK languages.

Unlike our previous plug-in, this plug-in expects you to provide host functions that satisfy our its import interface for a KV store.

Using [Extism.Function](https://extism.github.io/ocaml-sdk/extism/Extism/Function/index.html) we can define a host function that can be called from the guest plug-in. In this example we will create a function to help us load plugins and setup the host functions.

We want to expose two functions to our plugin (in OCaml types): `val kv_write: string -> string -> unit` which writes a bytes value to a key and `val kv_read: string -> string` which reads the bytes at the given `key`.

<!-- $MDX env=host-functions -->
```ocaml
let make_kv_plugin () =
  (* pretend this is Redis or something :) *)
  let kv_store = Hashtbl.create 8 in

  let kv_read =
    let open Val_type in
    Function.create "kv_read" ~params:[ I64 ] ~results:[ I64 ] ~user_data:kv_store
    @@ fun plugin kv_store ->
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
    Function.create "kv_write" ~params:[ I64; I64 ] ~results:[] ~user_data:kv_store
    @@ fun plugin kv_store ->
    let key = Host_function.input_string ~index:0 plugin in
    let value = Host_function.input_string ~index:1 plugin in
    Printf.printf "Write value=%s to key=%s\n" value key;
    Hashtbl.replace kv_store key value
  in

  (* Create a plugin from the manifest with the kv host functions *)
  Plugin.of_manifest_exn ~functions:[ kv_read; kv_write ] ~wasi:true manifest
```

> *Note*: In order to write host functions you should get familiar with the methods on the [Extism.Host_function](https://extism.github.io/ocaml-sdk/extism/Extism/Host_function/index.html) module.


Now we can invoke the event:

<!-- $MDX env=host-functions -->
```ocaml
# let plugin = make_kv_plugin ();;
val plugin : Plugin.t = <abstr>
# Extism.Plugin.call_string_exn plugin ~name:"count_vowels" "Hello, world" |> print_endline;;
Reading from key=count-vowels
Write value=    to key=count-vowels
{"count":3,"total":3,"vowels":"aeiouAEIOU"}
- : unit = ()
# Extism.Plugin.call_string_exn plugin ~name:"count_vowels" "Hello, world" |> print_endline;;
Reading from key=count-vowels
Write value=    to key=count-vowels
{"count":3,"total":6,"vowels":"aeiouAEIOU"}
- : unit = ()
```

