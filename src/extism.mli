(** Extism bindings for OCaml

    {1 Introduction}

    Extism is a framework for executing WebAssembly plugins. The OCaml bindings
    require libextism, installation information is available on
    {{:https://extism.org/docs/install} the Extism website}

    - The {!Plugin} and {!Manifest} modules are the main modules to look at.
    - {!Type} provides different types that can be encoded as input/output to
      {!Plugin.call}
    - {!Function} is used to define host functions and {!Host_function} is used
      from inside host functions to access the plugin memory

    {1 Basic example}

    The following loads a {!Plugin} from a file on disk using {!Manifest} then
    calls a function with a string and prints the string output:
    {@ocaml[
      open Extism

      let () =
        let plugin =
          Plugin.of_manifest_exn
          @@ Manifest.create [ Manifest.Wasm.file "test/code.wasm" ]
        in
        let res =
          Plugin.call_string_exn plugin ~name:"count_vowels" "input data"
        in
        print_endline res
    ]}

    {1 API} *)

module Manifest = Extism_manifest
(** The [Manifest] module is a reference to the [Extism_manifest] package, it
    allows you to programatically construct Extism manifests.

    For example, create a manifest from a file on disk:
    {@ocaml[
      open Extism

      let manifest = Manifest.create [ Manifest.Wasm.file "test/code.wasm" ]
    ]}
    Or from a URL:
    {@ocaml[
      open Extism

      let url =
        "https://github.com/extism/plugins/releases/latest/download/count_vowels.wasm"

      let manifest = Manifest.create [ Manifest.Wasm.url url ]
    ]}*)

(** [Val_type] enumerates the available Wasm types, this should only be used
    when implementing host functions *)
module Val_type : sig
  type t =
    | I32
    | I64
    | F32
    | F64
    | V128
    | FuncRef
    | ExternRef  (** Value type *)

  val ptr : t
  (** An alias for [I64] to signify an Extism pointer value *)

  val of_int : int -> t
  (** Convert from [int] to {!t},
      @raise Invalid_argument if the integer isn't valid *)

  val to_int : t -> int
  (** Convert from {!t} to [int] *)
end

(** [Val] represents low-level WebAssembly values *)
module Val : sig
  type t
  (** Val *)

  (** [Val.Array] is used for input/output parameters for host functions *)
  module Array : sig
    type t = Val.t Ctypes.CArray.t
    (** Array of {!Val.t} *)

    val get : t -> int -> Val.t
    (** Get {!Val.t} at index index *)

    val set : t -> int -> Val.t -> unit
    (** Set set the {!Val.t} at an index *)

    val length : t -> int
    (** Get the number of items in a {! Val.Array.t}*)

    val ( .$[] ) : t -> int -> Val.t
    (** Syntax for {!Val.Array.get} *)

    val ( .$[]<- ) : t -> int -> Val.t -> unit
    (** Syntax for {!Val.Array.set} *)
  end

  val ty : t -> Val_type.t
  (** [ty v] returns the {! Val_type.t} for the value [v] *)

  val of_i32 : int32 -> t
  (** Create an i32 {!Val.t} *)

  val of_i64 : int64 -> t
  (** Create an i64 {!Val} *)

  val of_f32 : float -> t
  (** Create an f32 {!Val} *)

  val of_f64 : float -> t
  (** Create an f64 {!Val} *)

  val to_i32 : t -> int32 option
  (** Get an int32 from {!Val} if the type matches *)

  val to_i64 : t -> int64 option
  (** Get an int64 from {!Val} if the type matches *)

  val to_f32 : t -> float option
  (** Get a f32 from {!Val} if the type matches *)

  val to_f64 : t -> float option
  (** Get an f64 from {!Val} if the type matches *)

  val to_i32_exn : t -> int32
  (** Same as {!to_i32} but raises [Invalid_argument] if the types don't match*)

  val to_i64_exn : t -> int64
  (** Same as {!to_i64} but raises [Invalid_argument] if the types don't match*)

  val to_f32_exn : t -> float
  (** Same as {!to_f32} but raises [Invalid_argument] if the types don't match*)

  val to_f64_exn : t -> float
  (** Same as {!to_f64} but raises [Invalid_argument] if the types don't match*)
end

(** [Type] defines conversions from OCaml values in and out of Extism memory *)
module Type : sig
  (** The interface for all types that can be converted between OCaml and Extism
      host memory *)
  module type S = sig
    type t

    val encode : t -> string
    (** Encode a value to a string *)

    val decode : Bigstringaf.t -> (t, Error.t) result
    (** Decode a value from a bigstring. The bigstring argument uses a direct
        pointer into the plugin memory, this means it shouldn't be stored
        outside of this function without being copied first *)
  end

  module String : S with type t = string
  (** String type *)

  module Bytes : S with type t = bytes
  (** Bytes type *)

  module Bigstring : S with type t = Bigstringaf.t
  (** Bigstring type *)

  module Json : S with type t = Yojson.Safe.t
  (** Json type *)

  module Unit : S with type t = unit
  (** Unit type, noop *)

  module Int64 : S with type t = int64
  (** Int64 type *)

  module Int32 : S with type t = int32
  (** Int32 type *)

  module Int : S with type t = int
  (** OCaml [int] type, encoded as an int64 *)

  module Float32 : S with type t = float
  (** 32-bit float type *)

  module Float64 : S with type t = float
  (** 64-bit float type *)

  (** {2 Packed modules}
      The following are packed modules that can be passed directly to a function
      expecting a [(module S: Type)] **)

  val string : (module S with type t = string)
  (** {!String} *)

  val bytes : (module S with type t = bytes)
  (** {!Bytes} *)

  val bigstring : (module S with type t = Bigstringaf.t)
  (** {!Bigstring} *)

  val json : (module S with type t = Yojson.Safe.t)
  (** {!Json} *)

  val unit : (module S with type t = unit)
  (** {!Unit} *)

  val int : (module S with type t = int)
  (** {!Int} *)

  val int64 : (module S with type t = Int64.t)
  (** {!Int64} *)

  val int32 : (module S with type t = Int32.t)
  (** {!Int32} *)

  val float32 : (module S with type t = Float32.t)
  (** {!Float32} *)

  val float64 : (module S with type t = Float64.t)
  (** {!Float64} *)
end

(** [Host_function] represents the plugin that is currently running from inside
    a host function definition *)
module Host_function : sig
  type t
  (** Opaque type, wraps [ExtismCurrentPlugin], this value should never be
      stored, it is only valid from inside the host function definition *)

  val params : t -> Val.Array.t
  (** Get host function parameters array *)

  val param : t -> int -> Val.t
  (** [param plugin n] returns the [n]th param from [plugin] *)

  val results : t -> Val.Array.t
  (** Get host function results array *)

  val result : t -> int -> Val.t
  (** [result plugin n] returns the [n]th result from [plugin] *)

  val set_result : t -> int -> Val.t -> unit
  (** [set_result plugin n v] updates the [n]th result to [v] *)

  type memory_handle = { offs : Unsigned.UInt64.t; len : Unsigned.UInt64.t }
  (** Represents a block of guest memory *)

  val output_string : t -> ?index:int -> string -> unit
  (** Return a string from a host function, this copies the string into memory
      and sets the results array at [index] with the pointer to the allocated
      value *)

  val output_bigstring : t -> ?index:int -> Bigstringaf.t -> unit
  (** Return a bigstring from a host function, this copies the bigstring into
      memory and sets the results array at [index] with the pointer to the
      allocated value *)

  val input_string : ?index:int -> t -> string
  (** Get a string argument, the parameter at [index] should be an int64 value
      that points to the string in linear memory *)

  val input_bigstring : ?index:int -> t -> Bigstringaf.t
  (** Load a parameter directly from memory *)

  val input :
    (module Type.S with type t = 'a) -> ?index:int -> t -> ('a, Error.t) result
  (** Get parameter from params array at [index] and return the converted result *)

  val input_exn : (module Type.S with type t = 'a) -> ?index:int -> t -> 'a
  (** Similar to {!input_exn} but raises an exception *)

  val output : (module Type.S with type t = 'a) -> ?index:int -> t -> 'a -> unit
  (** Convert a value, allocate it and update the results array at [index] *)

  (** Some helpter functions for reading/writing memory *)
  module Memory_handle : sig
    val memory : ?offs:Unsigned.UInt64.t -> t -> Unsigned.uint8 Ctypes.ptr
    (** Get pointer to entire plugin memory *)

    val find : t -> Unsigned.UInt64.t -> memory_handle option
    (** Convert an offset into a {!memory_handle} *)

    val alloc : t -> int -> memory_handle
    (** Allocate a new block of memory *)

    val free : t -> memory_handle -> unit
    (** Free allocated memory *)

    val to_val : memory_handle -> Val.t
    (** Convert memory block to [Val] *)

    val of_val : t -> Val.t -> memory_handle option
    (** Convert [Val] to memory block *)

    val of_val_exn : t -> Val.t -> memory_handle
    (** Convert [Val] to memory block, raises [Invalid_argument] if the value is
        not a pointer to a valid memory block *)

    val get_string : t -> memory_handle -> string
    (** Get a string from memory stored at the provided offset *)

    val get_bigstring : t -> memory_handle -> Bigstringaf.t
    (** Get a bigstring from memory stored at the provided offset *)

    val set_string : t -> memory_handle -> string -> unit
    (** Store a string into memory at the provided offset *)

    val set_bigstring : t -> memory_handle -> Bigstringaf.t -> unit
    (** Store a bigstring into memory at the provided offset *)
  end
end

(** [Function] is used to create new a new function, which can be called from a
    WebAssembly plugin *)
module Function : sig
  type t
  (** Function type *)

  val create :
    string ->
    ?namespace:string ->
    params:Val_type.t list ->
    results:Val_type.t list ->
    user_data:'a ->
    (Host_function.t -> 'a -> unit) ->
    t
  (** Create a new function, [Function.v name ~params ~results ~user_data f]
      creates a new {!Function.t} with the given [name], [params] specifies the
      argument types, [results] specifies the return types, [user_data] is used
      to pass arbitrary OCaml values into the function and [f] is the OCaml
      function that will be called. *)

  val with_namespace : t -> string -> t
  (** Update a function's namespace *)
end

(** [Plugins] contain functions that can be called *)
module Plugin : sig
  type t

  val create :
    ?config:Manifest.config ->
    ?wasi:bool ->
    ?functions:Function.t list ->
    string ->
    (t, Error.t) result
  (** Make a new plugin from raw WebAssembly or JSON encoded manifest *)

  val create_exn :
    ?config:Manifest.config ->
    ?wasi:bool ->
    ?functions:Function.t list ->
    string ->
    t
  (** Make a new plugin from raw WebAssembly or JSON encoded manifest *)

  val of_manifest :
    ?wasi:bool ->
    ?functions:Function.t list ->
    Manifest.t ->
    (t, Error.t) result
  (** Make a new plugin from a {!Manifest} *)

  val of_manifest_exn :
    ?wasi:bool -> ?functions:Function.t list -> Manifest.t -> t
  (** Make a new plugin from a {!Manifest} *)

  val call_bigstring :
    t -> name:string -> Bigstringaf.t -> (Bigstringaf.t, Error.t) result
  (** Call a function, uses [Bigstringaf.t] for input/output *)

  val call_bigstring_exn : t -> name:string -> Bigstringaf.t -> Bigstringaf.t
  (** Similar to {!call_bigstring} but raises an exception using {!Error.unwrap} *)

  val call_string : t -> name:string -> string -> (string, Error.t) result
  (** Call a function, uses [string] for input/output *)

  val call_string_exn : t -> name:string -> string -> string
  (** Similar to {!call_string} but raises an exception using {!Error.unwrap} *)

  val call :
    (module Type.S with type t = 'a) ->
    (module Type.S with type t = 'b) ->
    t ->
    name:string ->
    'a ->
    ('b, Error.t) result
  (** [call input_type output_type t ~name input] executes a function with input
      and output types defined in {!Type} *)

  val call_exn :
    (module Type.S with type t = 'a) ->
    (module Type.S with type t = 'b) ->
    t ->
    name:string ->
    'a ->
    'b
  (** Similar to {!call} but raises an exception using {!Error.unwrap} *)

  val free : t -> unit
  (** Free a plugin immediately, this isn't normally required unless there are a
      lot of plugins open at once *)

  val reset : t -> bool
  (** Reset the Extism runtime, this will invalidate all allocated memory *)

  val function_exists : t -> string -> bool
  (** Check if a function is exported by a plugin *)

  module Cancel_handle : sig
    type t

    val cancel : t -> bool
    (** Cancel a running plugin *)
  end

  val cancel_handle : t -> Cancel_handle.t
  (** Get the {!Cancel_handle.t} *)

  val id : t -> Uuidm.t
  (** Get the plugin UUID *)
end

val set_log_file :
  ?level:[ `Error | `Warn | `Info | `Debug | `Trace | `Filter of string ] ->
  string ->
  bool
(** Set the log file and level for all Extism plugins, the names [stdout] or
    [stderr] can be used to write to the terminal *)

type drain_logs = unit -> unit

val set_log_custom :
  ?level:[ `Error | `Warn | `Info | `Debug | `Trace | `Filter of string ] ->
  (string -> unit) ->
  drain_logs
(** Set the log level and enable buffered logging. Returns a function that can
    be used to drain the logs *)

(** Extism error type *)
module Error : sig
  type t = [ `Msg of string ]
  (** Error type *)

  exception Error of t
  (** Exception type *)

  val unwrap : ('a, t) result -> 'a
  (** Like [Result.get_ok] for {!t} *)

  val throw : t -> 'a
  (** Raise {!t} as an exception *)
end

val with_plugin : (Plugin.t -> 'a) -> Plugin.t -> 'a
(** [with_plugin f plugin] uses [Fun.protect] to ensure that a plugin is freed
    when [f] finished executing *)

val extism_version : unit -> string
(** Returns the libextism version, not the version of the OCaml library *)
