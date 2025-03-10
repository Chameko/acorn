open Cmdliner
open Paths
open Server
open Client

(** Features for acorn *)
type features =
  | Highlight
  | All

(** Function called on start *)
let start init kakoune server request features =
  Printf.printf "CLI ARGS: INIT: %d\nKAK: %B\nSERVER: %B\n" init kakoune server;
  match request with
  | Some request -> Printf.printf "REQ: %s\n" request
  | None ->
    Printf.printf "REQ: NONE\n";
    Logs.set_reporter @@ K_error.lwt_reporter ();
    Logs.set_level (Some Logs.Debug);
    Printf.printf "FEATURES: ";
    List.iter (fun f -> Printf.printf "%s" f) features;
    Printf.printf "\n";
    (match paths () with
     | Error e -> Printf.printf "%s" @@ K_error.output e
     | Ok paths ->
       if server then start_server paths else Lwt_main.run (start_client paths))
;;

let init =
  let doc = "Initialise the server from kakoune." in
  Arg.(value & opt int 0 & info [ "i"; "init" ] ~docv:"SESSION ID" ~doc)
;;

let kakoune =
  let doc = "Redirect debug output when run in kakoune." in
  Arg.(value & flag & info [ "k"; "kakoune" ] ~doc)
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
  Cmd.v info Term.(const start $ init $ kakoune $ server $ request $ features)
;;

(** Run the CLI *)
let run () = exit (Cmd.eval cmd)
