open Ctypes

type t = {
  free_lock : Mutex.t;
  mutable pointer : unit ptr;
  mutable user_data : unit ptr;
  name : string;
  closure :
    unit ptr ->
    Val.t ptr ->
    Unsigned.uint64 ->
    Val.t ptr ->
    Unsigned.uint64 ->
    unit ptr ->
    unit;
}

let free t =
  let () = Mutex.lock t.free_lock in
  Fun.protect
    ~finally:(fun () -> Mutex.unlock t.free_lock)
    (fun () ->
      let () =
        if not (is_null t.user_data) then
          let () = Root.release t.user_data in
          t.user_data <- null
      in
      if not (is_null t.pointer) then
        let () = Bindings.extism_function_free t.pointer in
        t.pointer <- null)

let create name ?namespace ~params ~results ~user_data f =
  let inputs = CArray.of_list Bindings.Extism_val_type.t params in
  let n_inputs = Unsigned.UInt64.of_int (CArray.length inputs) in
  let outputs = CArray.of_list Bindings.Extism_val_type.t results in
  let n_outputs = Unsigned.UInt64.of_int (CArray.length outputs) in
  let free' = Some Root.release in
  let user_data = Root.create user_data in
  let f pointer inputs n_inputs outputs n_outputs user_data =
    let user_data = Root.get user_data in
    let params = CArray.from_ptr inputs (Unsigned.UInt64.to_int n_inputs) in
    let results = CArray.from_ptr outputs (Unsigned.UInt64.to_int n_outputs) in
    f { Host_function.pointer; params; results } user_data
  in
  let pointer =
    Bindings.extism_function_new name (CArray.start inputs) n_inputs
      (CArray.start outputs) n_outputs f user_data free'
  in
  let () =
    Option.iter (Bindings.extism_function_set_namespace pointer) namespace
  in
  let t =
    { pointer; user_data; name; free_lock = Mutex.create (); closure = f }
  in
  Gc.finalise_last (fun () -> free t) t;
  t

let with_namespace f ns =
  Bindings.extism_function_set_namespace f.pointer ns;
  f
