(** Extism manifest

    This package provides an OCaml interface for the
    {{:https://extism.org/docs/concepts/manifest} Extism manifest}

    {1 Examples}
    Create a manifest from a file on disk:
    {[
      Manifest.create [ Manifest.Wasm.file myPath ]
    ]}

    Or from a URL:
    {[
      Manifest.create [ Manifest.Wasm.url myUrl ]
    ]}

    Create a manifest from a file on disk with a timeout of [1s] set and
    [memory.max_pages] set to 100:
    {[
      Manifest.create ~timeout_ms:1000 [ Manifest.Wasm.file ]
      |> Manifest.with_memory_max 100
    ]}

    {1 API} *)

type memory_options = {
  max_pages : int option;
  (** [max_pages] can be used to limit the total number of pages used by the
      runtime *)
  max_http_response_bytes: int option;
  (** [max_http_response_bytes] can be used to limit the size of the response returned by
      [extism_http_request] *)
  max_var_bytes: int option;
  (** [max_var_bytes] can be used to limit the size of the Extism var store *)
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
  (** Create {!t} from filename *)

  val data : ?name:string -> ?hash:string -> string -> t
  (** Create {!t} from WebAssembly module data *)

  val url :
    ?headers:(string * string) list ->
    ?name:string ->
    ?meth:string ->
    ?hash:string ->
    string ->
    t
  (** Create {!t} from URL *)
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
      (** A dict containing a mapping from [local_path:guest_path] *)
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
(** Create new {!t} *)

val to_json : t -> string
(** Convert manifest to JSON *)

val of_json : string -> t
(** Read manifest from JSON string *)

val of_file : string -> t
(** Read manifest from JSON file *)

val with_config : config -> t -> t
(** Returns a new {!t} with the [config] field updated *)

val with_memory_max : int -> t -> t
(** Returns a new {!t} with [memory.max_pages] updated *)

val with_http_response_max_bytes : int -> t -> t
(** Returns a new {!t} with [memory.max_http_response_bytes] updated *)

val with_var_max_bytes : int -> t -> t
(** Returns a new {!t} with [memory.max_var_bytes] updated *)
