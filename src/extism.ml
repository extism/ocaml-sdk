module Manifest = Extism_manifest
module Error = Error
module Plugin = Plugin
module Function = Function
module Host_function = Host_function
module Type = Type
module Val = Val
module Val_type = Val_type

let extism_version = Bindings.extism_version

let with_plugin f p =
  Fun.protect ~finally:(fun () -> Plugin.free p) (fun () -> f p)

let%test "with_plugin" =
  let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
  let plugin = Plugin.of_manifest manifest |> Error.unwrap in
  let b =
    with_plugin
      (fun plugin ->
        Plugin.call Type.string Type.string plugin ~name:"count_vowels"
          "this is a test"
        |> Error.unwrap = "{\"count\":4,\"total\":4,\"vowels\":\"aeiouAEIOU\"}")
      plugin
  in
  Plugin.free plugin;
  Gc.minor ();
  Gc.full_major ();
  b

let%test _ = String.length (extism_version ()) > 0

let parse_level =
  Option.map (function
    | `Error -> "error"
    | `Warn -> "warn"
    | `Info -> "info"
    | `Debug -> "debug"
    | `Trace -> "trace"
    | `Filter f -> f)

let set_log_file ?level filename =
  Bindings.extism_log_file filename (parse_level level)

type drain_logs = unit -> unit

let mk_drain_logs f () =
  let fx s length =
    let s = Ctypes.string_from_ptr s ~length:(Ctypes.Uintptr.to_int length) in
    f s
  in
  Bindings.extism_log_drain fx

let set_log_custom ?level f =
  if Bindings.extism_log_custom (parse_level level) then
    let x = mk_drain_logs f in
    let () = Gc.finalise_last (mk_drain_logs f) x in
    x
  else Error.throw (`Msg "Unable to set custom logging")

let%test _ =
  let log_file =
    try String.length @@ Unix.getenv "TEST_LOG_FILE" > 0
    with Not_found -> false
  in
  if log_file then set_log_file ~level:`Trace "stderr"
  else
    let count = ref 0 in
    let _drain_logs =
      set_log_custom ~level:`Trace (fun s ->
          incr count;
          print_string s)
    in
    let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
    let plugin = Plugin.of_manifest manifest |> Error.unwrap in
    let _ =
      Plugin.call Type.string Type.string plugin ~name:"count_vowels"
        "this is a test"
    in
    (* Make sure logs are drained when drain_logs is garbage collected *)
    Gc.minor ();
    Gc.full_major ();
    !count > 0

module Pool = struct
  exception Timeout

  type queue = {
    mutable count : int Atomic.t;
    q : Plugin.t Queue.t;
    lock : Mutex.t;
  }

  type 'a t = {
    lock : Mutex.t;
    max_instances : int;
    plugins : ('a, unit -> Plugin.t) Hashtbl.t;
    instances : ('a, queue) Hashtbl.t;
  }

  module Instance = struct
    type t = { mutable plugin : Plugin.t option; q : queue }

    let plugin { plugin; _ } =
      match plugin with
      | Some p -> p
      | None -> invalid_arg "plugin instance has already been freed"

    let free x =
      match x.plugin with
      | Some plugin ->
          x.plugin <- None;
          Mutex.protect x.q.lock @@ fun () -> Queue.push plugin x.q.q
      | None -> ()

    let use t f =
      Fun.protect ~finally:(fun () -> free t) (fun () -> f (plugin t))
  end

  let create n =
    {
      lock = Mutex.create ();
      max_instances = n;
      plugins = Hashtbl.create 8;
      instances = Hashtbl.create 8;
    }

  let add t name f =
    Mutex.protect t.lock @@ fun () ->
    Hashtbl.replace t.plugins name f;
    Hashtbl.replace t.instances name
    @@ { q = Queue.create (); count = Atomic.make 0; lock = Mutex.create () }

  let count t name =
    let instance = Hashtbl.find t.instances name in
    Atomic.get instance.count

  let get_opt t name =
    let q = Hashtbl.find t.instances name in
    Mutex.protect q.lock @@ fun () ->
    let mk plugin =
      let x = { Instance.plugin = Some plugin; q } in
      Gc.finalise
        (fun x ->
          match x.Instance.plugin with
          | Some plugin ->
              x.plugin <- None;
              Mutex.protect q.lock @@ fun () -> Queue.push plugin q.q
          | None -> ())
        x;
      x
    in
    match Queue.take_opt q.q with
    | Some x -> Some (mk x)
    | None ->
        let f = Hashtbl.find t.plugins name in
        if Atomic.get q.count < t.max_instances then
          let () = Atomic.incr q.count in
          Some (mk @@ f ())
        else None

  let get ?timeout t name =
    let rec aux start =
      match get_opt t name with
      | Some x -> x
      | None when Option.is_some timeout ->
          let curr = Unix.gettimeofday () in
          if curr -. start >= Option.get timeout then raise Timeout
          else aux start
      | None -> aux start
    in
    aux @@ Unix.gettimeofday ()

  let%test "pool timeout" =
    let pool = create 3 in
    add pool "test" (fun () ->
        let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
        Plugin.of_manifest manifest |> Error.unwrap);
    let _a = get pool "test" in
    let _b = get pool "test" in
    let _c = get pool "test" in
    try
      let _ = get ~timeout:1.0 pool "test" in
      false
    with Timeout -> count pool "test" = 3

  let%test "pool timeout" =
    let pool = create 2 in
    add pool "test" (fun () ->
        let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
        Plugin.of_manifest manifest |> Error.unwrap);
    let a = get pool "test" in
    let () = Instance.free a in
    let b = get pool "test" in
    let () = Instance.free b in
    let _c = get pool "test" in
    try
      let _ = get ~timeout:0.0 pool "test" in
      count pool "test" = 2
    with Timeout -> false

  let%test "pool threads" =
    let pool = create 2 in
    add pool "test" (fun () ->
        let manifest = Manifest.(create [ Wasm.file "test/code.wasm" ]) in
        Plugin.of_manifest manifest |> Error.unwrap);
    let total = Atomic.make 0 in
    let run n =
      Thread.create
        (fun () ->
          Thread.delay n;
          let a = get pool "test" in
          Instance.use a @@ fun plugin ->
          ignore (Plugin.call_string_exn plugin ~name:"count_vowels" "input");
          Atomic.incr total)
        ()
    in
    let all = [ run 1.0; run 1.0; run 0.5; run 0.5; run 0.0 ] in
    let () = List.iter Thread.join all in
    Atomic.get total = List.length all && count pool "test" <= 2
end
