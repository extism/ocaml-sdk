open Extism
open Cmdliner

let read_stdin () = In_channel.input_all stdin

let split_allowed_paths =
  List.filter_map (fun path ->
      let s = String.split_on_char ':' path in
      match s with
      | [] -> None
      | [ p ] -> Some (p, p)
      | p :: tl -> Some (p, String.concat ":" tl))

let split_config =
  List.filter_map (fun path ->
      let s = String.split_on_char '=' path in
      match s with
      | [] -> None
      | [ p ] -> Some (p, None)
      | p :: tl -> Some (p, Some (String.concat "=" tl)))

let print_timing ~time name f =
  if not time then f ()
  else
    let start = Unix.gettimeofday () in
    let out = f () in
    let finish = Unix.gettimeofday () in
    Printf.printf "TIME %s: %fs\n%!" name (finish -. start);
    out

let main file func_name input loop timeout_ms allowed_paths allowed_hosts config
    memory_max http_max var_max log_level log_file wasi stdin time _manifest =
  let input = if stdin then read_stdin () else input in
  let allowed_paths = split_allowed_paths allowed_paths in
  let config = split_config config in
  let memory =
    Manifest.
      {
        max_pages = memory_max;
        max_http_response_bytes = http_max;
        max_var_bytes = var_max;
      }
  in
  let manifest =
    print_timing ~time "loaded manifest" @@ fun () ->
    try
      let m = Manifest.of_file file in
      {
        m with
        timeout_ms = Some timeout_ms;
        allowed_hosts = Some allowed_hosts;
        allowed_paths = Some allowed_paths;
        config = Some config;
        memory = Some memory;
      }
    with _ ->
      Manifest.create ~timeout_ms ~allowed_hosts ~allowed_paths ~config ~memory
        [ Manifest.(Wasm.File { path = file; hash = None; name = None }) ]
  in
  let () =
    match (log_level, log_file) with
    | None, _ -> ()
    | Some level, Some file -> assert (set_log_file ~level file)
    | Some level, None -> assert (set_log_file ~level "stderr")
  in
  let plugin =
    match Plugin.of_manifest manifest ~wasi with
    | Ok x -> x
    | Error (`Msg e) ->
        Printf.eprintf "ERROR Unable to load plugin: %s" e;
        exit 1
  in
  for _ = 0 to loop do
    print_timing ~time ("call to " ^ func_name) @@ fun () ->
    match Plugin.call_string plugin ~name:func_name input with
    | Ok res -> if String.length res > 0 then print_endline res
    | Error (`Msg e) ->
        Printf.eprintf "ERROR call encountered an error: %s" e;
        exit 2
  done

let file =
  let doc = "The Wasm module or Extism manifest path." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let func_name =
  let doc = "The function to run." in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"NAME" ~doc)

let input =
  let doc = "Input data." in
  Arg.(value & opt string "" & info [ "input"; "i" ] ~docv:"INPUT" ~doc)

let loop =
  let doc = "Number of times to call the plugin." in
  Arg.(value & opt int 0 & info [ "loop" ] ~docv:"TIMES" ~doc)

let memory_max =
  let doc = "Max number of memory pages." in
  Arg.(value & opt (some int) None & info [ "memory-max" ] ~docv:"PAGES" ~doc)

let http_max =
  let doc = "Max number of bytes allowed in an HTTP response." in
  Arg.(
    value
    & opt (some int) None
    & info [ "http-response-max" ] ~docv:"BYTES" ~doc)

let var_max =
  let doc = "Max number of bytes allowed in the Extism var store" in
  Arg.(value & opt (some int) None & info [ "var-max" ] ~docv:"BYTES" ~doc)

let timeout =
  let doc = "Plugin timeout in milliseconds." in
  Arg.(value & opt int 30000 & info [ "timeout"; "t" ] ~docv:"MILLIS" ~doc)

let allowed_paths =
  let doc = "Allowed paths." in
  Arg.(
    value & opt_all string []
    & info [ "allow-path" ] ~docv:"LOCAL_PATH[:PLUGIN_PATH]" ~doc)

let allowed_hosts =
  let doc = "Allowed hosts for HTTP requests." in
  Arg.(value & opt_all string [] & info [ "allow-host" ] ~docv:"HOST" ~doc)

let config =
  let doc = "Plugin config." in
  Arg.(value & opt_all string [] & info [ "config" ] ~docv:"KEY=VALUE" ~doc)

let log_file =
  let doc = "File to write logs to." in
  Arg.(
    value & opt (some string) None & info [ "log-file" ] ~docv:"FILENAME" ~doc)

let log_level_enum =
  Arg.enum
    [
      ("warn", `Warn);
      ("info", `Info);
      ("debug", `Debug);
      ("error", `Error);
      ("trace", `Trace);
    ]

let log_level =
  let doc = "Log level." in
  Arg.(
    value
    & opt (some log_level_enum) None
    & info [ "log-level" ] ~docv:"LEVEL" ~doc)

let wasi =
  let doc = "Enable WASI." in
  Arg.(value & flag & info [ "wasi" ] ~doc)

let manifest =
  let doc = "Unused, exists for compatibility with Go CLI" in
  Arg.(value & flag & info [ "manifest" ] ~doc)

let stdin =
  let doc = "Read function input from stdin." in
  Arg.(value & flag & info [ "stdin" ] ~doc)

let time =
  let doc = "Print timing information." in
  Arg.(value & flag & info [ "time" ] ~doc)

let main_t =
  Term.(
    const main $ file $ func_name $ input $ loop $ timeout $ allowed_paths
    $ allowed_hosts $ config $ memory_max $ http_max $ var_max $ log_level
    $ log_file $ wasi $ stdin $ time $ manifest)

let cmd = Cmd.v (Cmd.info "extism-call") main_t
let () = exit (Cmd.eval cmd)
