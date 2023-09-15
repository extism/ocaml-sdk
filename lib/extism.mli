(** Extism bindings for OCaml *)

val extism_version : unit -> string
(** Returns the libextism version, not the version of the OCaml library *)

module Manifest = Extism_manifest

module Error : sig
  type t = [ `Msg of string ]

  exception Error of t

  val unwrap : ('a, t) result -> 'a
  val throw : t -> 'a
end


(** [Val] represents low-level WebAssembly values *)
module Val : sig
  type t
  (** Val *)

  (** [Val.Type] enumerates every possible argument/result type *)
  module Type : sig
    type t =
      | I32
      | I64
      | F32
      | F64
      | V128
      | FuncRef
      | ExternRef  (** Value type *)

    val of_int : int -> t
    val to_int : t -> int
  end

  (** [Val.Array] is used for input/output parameters for host functions *)
  module Array : sig
    type t = Val.t Ctypes.CArray.t
    (** [Val_array] type *)

    val get : t -> int -> Val.t
    (** Get an index *)

    val set : t -> int -> Val.t -> unit
    (** Set an index *)

    val length : t -> int
    (** Get the number of items in a [Val_array]*)

    val ( .$[] ) : t -> int -> Val.t
    (** Syntax for [get] *)

    val ( .$[]<- ) : t -> int -> Val.t -> unit
    (** Syntax for [set] *)
  end

  val ty : t -> Type.t
  (** [ty v] returns the [Val.Type.t] for the value [v] *)

  val of_i32 : int32 -> t
  (** Create an i32 [Val] *)

  val of_i64 : int64 -> t
  (** Create an i64 [Val] *)

  val of_f32 : float -> t
  (** Create an f32 [Val] *)

  val of_f64 : float -> t
  (** Create an f64 [Val] *)

  val to_i32 : t -> int32 option
  (** Get an int32 from [Val] if the type matches *)

  val to_i64 : t -> int64 option
  (** Get an int64 from [Val] if the type matches *)

  val to_f32 : t -> float option
  (** Get a f32 from [Val] if the type matches *)

  val to_f64 : t -> float option
  (** Get an f64 from [Val] if the type matches *)

  val to_i32_exn : t -> int32
  (** Same as [to_i32] but raises an exception if the types don't match*)

  val to_i64_exn : t -> int64
  (** Same as [to_i64] but raises an exception if the types don't match*)

  val to_f32_exn : t -> float
  (** Same as [to_f32] but raises an exception if the types don't match*)

  val to_f64_exn : t -> float
  (** Same as [to_f64] but raises an exception if the types don't match*)
end


(** [Type] defines conversions from OCaml values in and out of Extism memory *)
module Type: sig
  module type S = sig
    type t

    val encode : t -> string
    val decode : Bigstringaf.t -> (t, Error.t) result
  end

  module String : S with type t = string
  module Bytes : S with type t = bytes
  module Bigstring : S with type t = Bigstringaf.t
  module Json : S with type t = Yojson.Safe.t
end

(** [Host_function] represents the plugin that is currently running, it should
    it should only be used from a host function *)
module Host_function: sig
  type t
  (** Opaque type, wraps [ExtismCurrentPlugin] *)

  val params: t -> Val.Array.t
  (** Get host function parameters array *)

  val results: t -> Val.Array.t
  (** Get host function results array *)

  type memory_handle = { offs : Unsigned.UInt64.t; len : Unsigned.UInt64.t }
  (** Represents a block of guest memory *)

  val return_string : t -> ?index:int -> string -> unit
  (** Return a string from a host function, this copies the string into memory and
      sets the results array at [index] with the pointer to the allocated value *)

  val return_bigstring : t -> ?index:int -> Bigstringaf.t -> unit
  (** Return a bigstring from a host function, this copies the bigstring into memory and
      sets the results array at [index] with the pointer to the allocated value *)

  val input_string : ?index:int -> t -> string
  (** Get a string argument, the parameter at [index] should be an int64 value that points to the
      string in linear memory *)

  val input_bigstring : ?index:int -> t -> Bigstringaf.t
  (** Load a parameter directly from memory *)

  val input : t -> ?index:int -> (module Type.S with type t = 'a) -> ('a, Error.t) result
  (** Get parameter from params array at [index] and return the converted result *)

  val return : t -> ?index:int -> (module Type.S with type t = 'a) -> 'a -> unit
  (** Convert a value, allocate it and update the results array at [index] *)

  (** Some helpter functions for reading/writing memory *)
  module Memory_handle : sig
    val memory : ?offs:Unsigned.UInt64.t -> t -> Unsigned.uint8 Ctypes.ptr
    (** Get pointer to entire plugin memory *)

    val find : t -> Unsigned.UInt64.t -> memory_handle option
    (** Convert an offset into a {memory_handle} *)

    val alloc : t -> int -> memory_handle
    (** Allocate a new block of memory *)

    val free : t -> memory_handle -> unit
    (** Free allocated memory *)

    val to_val : memory_handle -> Val.t
    (** Convert memory block to [Val] *)

    val of_val : t -> Val.t -> memory_handle option
    (** Convert [Val] to memory block *)

    val of_val_exn : t -> Val.t -> memory_handle
    (** Convert [Val] to memory block, raises [Invalid_argument] if the value is not a pointer
        to a valid memory block *)

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


(** [Function] is used to create new a new function, which can be called
    from a WebAssembly plugin *)
module Function : sig
  type t
  (** Function type *)

  val create :
    string ->
    ?namespace:string ->
    params:Val.Type.t list ->
    results:Val.Type.t list ->
    user_data:'a ->
    (Host_function.t -> 'a -> unit) ->
    t
  (** Create a new function, [Function.v name ~params ~results ~user_data f] creates
      a new [Function] with the given [name], [params] specifies the argument types,
      [results] specifies the return types, [user_data] is used to pass arbitrary
      OCaml values into the function and [f] is the OCaml function that will be
      called.
  *)

  val with_namespace : t -> string -> t
  (** Update a function's namespace *)
end

val set_log_file :
  ?level:[ `Error | `Warn | `Info | `Debug | `Trace ] -> string -> bool

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

  val of_manifest :
    ?wasi:bool ->
    ?functions:Function.t list ->
    Manifest.t ->
    (t, Error.t) result
  (** Make a new plugin from a [Manifest] *)

  val call_bigstring :
    t -> name:string -> Bigstringaf.t -> (Bigstringaf.t, Error.t) result
  (** Call a function, uses [Bigstringaf.t] for input/output *)

  val call_string : t -> name:string -> string -> (string, Error.t) result
  (** Call a function, uses [string] for input/output *)

  val call :
    (module Type.S with type t = 'a) ->
    (module Type.S with type t = 'b) ->
    t ->
    name:string ->
    'a ->
    ('b, Error.t) result
  (** [call t ~name input_type output_type input] executes a function with typed input and output *)

  val free : t -> unit
  (** Free a plugin immediately, this isn't normally required unless there are a lot of plugins ope
      at once *)

  val function_exists : t -> string -> bool
  (** Check if a function is exported by a plugin *)

  module Cancel_handle : sig
    type t

    val cancel : t -> bool
    (** Cancel a running plugin *)
  end

  val cancel_handle : t -> Cancel_handle.t
  (** Get the {Cancel_handle.t} *)

  val id : t -> Uuidm.t
  (** Get the plugin UUID *)
end

val with_plugin : (Plugin.t -> 'a) -> Plugin.t -> 'a
(** Create a Plugin that can be used within the scope of the provided callback, it will
    automatically be freed when the function returns *)
