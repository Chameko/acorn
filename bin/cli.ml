open Cmdliner

(** Features for acorn *)
type features =
  | Highlight
  | All

(** Daemonizes the process *)
let daemonize paths =
  let fork_no_parent () =
    match Unix.handle_unix_error Unix.fork () with
    | 0 -> ()
    | _ -> Stdlib.exit 0
  in
  (* Fork into background, parent exits and child continues *)
  fork_no_parent ();
  (* Become session leader *)
  ignore (Unix.setsid ());
  (* Fork again to ensure that we will never regain a controlling terminal. *)
  fork_no_parent ();
  (* Redirect stdout and stderr *)
  let redirect src dst =
    Unix.dup2 src dst;
    Unix.close src
  in
  redirect
    (Unix.openfile
       (Resources.stdout_file paths)
       [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ]
       0o666)
    Unix.stdout;
  redirect
    (Unix.openfile
       (Resources.stderr_file paths)
       [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ]
       0o666)
    Unix.stderr
;;

(** Function called on start *)
let start init do_daemonize server request features =
  Logs.set_reporter @@ K_error.lwt_reporter ();
  Logs.set_level (Some Logs.Debug);
  match Resources.paths () with
  | Error e -> Printf.printf "%s" @@ K_error.output e
  | Ok paths ->
    Lwt_main.run
    @@
    if server
    then
      if init == 0
      then
        if do_daemonize
        then (
          let () = daemonize paths in
          Server.start_server { paths; sessions = ref [||] })
        else Server.start_server { paths; sessions = ref [||] }
      else (
        match%lwt Kak.kak_session init with
        | Ok session when do_daemonize == true ->
          daemonize paths;
          Server.start_server { paths; sessions = ref [| session |] }
        | Ok session -> Server.start_server { paths; sessions = ref [| session |] }
        | Error err -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output err))
    else Client.start_client ?req:request paths
;;

let init =
  let doc = "Initialise the server from kakoune." in
  Arg.(value & opt int 0 & info [ "i"; "init" ] ~docv:"SESSION ID" ~doc)
;;

let daemonize =
  let doc = "Daemonize the process." in
  Arg.(value & flag & info [ "d"; "daemonize" ] ~doc)
;;

let server =
  let doc = "Initialise the kakorn server." in
  Arg.(value & flag & info [ "s"; "server" ] ~doc)
;;

let request =
  let doc = "Request an action from the server written in JSON." in
  Arg.(value & opt (some string) None & info [ "r"; "request" ] ~doc)
;;

let features =
  let doc = "Features of kakcorn to enable" in
  Arg.(
    value & opt (list string) [ "all" ] & info [ "f"; "features" ] ~docv:"FEATURES" ~doc)
;;

let cmd =
  let doc = "Acorn interface for kakoune" in
  let man =
    [ `S Manpage.s_description
    ; `P
        "$(tname) enables advanced functionality for acorn buffers in kakoune. To use\n\
        \          put <insert rc here> into your kakrc. Available features are"
    ; `I ("$(b, highlight)", "Syntax Highlighting")
    ; `I ("$(b, all)", "All features")
    ]
  in
  let info = Cmd.info "kakorn" ~version:"v0.1.0" ~doc ~man in
  Cmd.v info Term.(const start $ init $ daemonize $ server $ request $ features)
;;

(** Run the CLI *)
let run () = exit (Cmd.eval cmd)
