(** Extism manifest

    This package provides an OCaml interface for the
    {{:https://extism.org/docs/concepts/manifest} Extism manifest} *)

type memory_options = {
  max_pages : int option;
      (** [max_pages] can be used to limit the total number of pages used by the
          runtime *)
}
[@@deriving yojson]
(** Memory options *)

type dict = (string * string) list [@@deriving yojson]
(** Key/value dictionary *)

type config = (string * string option) list [@@deriving yojson]
(** Key/value dictionary with optional values, used for passing values into a
    plugin at runtime *)

module Wasm : sig
  type file = {
    path : string;  (** Path to Wasm module on disk *)
    name : string option; [@yojson.option]
        (** Optional name of module for linking *)
    hash : string option; [@yojson.option]  (** Optional hash for verification *)
  }
  [@@deriving yojson]
  (** WebAssembly file *)

  type data = {
    data : string;  (** A string containing a Wasm module *)
    name : string option; [@yojson.option]
        (** Optional name of module for linking *)
    hash : string option; [@yojson.option]  (** Optional hash for verification *)
  }
  [@@deriving yojson]
  (** WebAssembly module data *)

  type url = {
    url : string;  (** A URL to a Wasm module *)
    headers : dict option; [@yojson.option]  (** Request headers *)
    meth : string option; [@yojson.option] [@key "method"]
        (** Request method *)
    name : string option; [@yojson.option]
        (** Optional name of module for linking *)
    hash : string option; [@yojson.option]  (** Optional hash for verification *)
  }
  [@@deriving yojson]
  (** WebAssembly URL *)

  (** WebAssembly from a file, module data or URL *)
  type t = File of file | Data of data | Url of url [@@deriving yojson]

  val file : ?name:string -> ?hash:string -> string -> t
  (** Create [wasm] from filename *)

  val data : ?name:string -> ?hash:string -> string -> t
  (** Create [wasm] from WebAssembly module data *)

  val url :
    ?headers:(string * string) list ->
    ?name:string ->
    ?meth:string ->
    ?hash:string ->
    string ->
    t
  (** Create [wasm] from URL *)
end

type t = {
  wasm : Wasm.t list;
      (** A list of Wasm modules to be linked, the final module or the module
          named [main] will be used as the main module *)
  memory : memory_options option;  (** Memory options *)
  config : config option;
      (** Config contains immutable configutation parameters from the host to a
          plugin *)
  allowed_hosts : string list option;
      (** A list of allowed hosts when using Extism HTTP functions *)
  allowed_paths : dict option;
      (** A dict containing a mapping from local_path:guest_path *)
  timeout_ms : int option;  (** Plugin timeout in milliseconds *)
}
[@@deriving yojson]
(** Manifest type *)

val create :
  ?config:config ->
  ?memory:memory_options ->
  ?allowed_hosts:string list ->
  ?allowed_paths:dict ->
  ?timeout_ms:int ->
  Wasm.t list ->
  t
(** Create new manifest *)

val to_json : t -> string
(** Convert manifest to JSON *)

val of_json : string -> t
(** Read manifest from JSON string *)

val of_file : string -> t
(** Read manifest from JSON file *)

val with_config : t -> config -> t
(** Returns a new {!t} with the [config] field updated *)

val with_memory_max : t -> int -> t
(** Returns a new {!t} with [memory.max_pages] updates *)
