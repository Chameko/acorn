(** Various error types for the server *)
type error =
  NoRuntimeDir
| IOError of string * string
| ServerAlreadyRunning
| FailedConnection of string

let output = function
  | NoRuntimeDir -> "Cannot find runtime directory"
  | ServerAlreadyRunning -> "Server is already running"
  | IOError (s1, s2) -> "I/O Error for " ^ s1 ^ " . Reason: " ^ s2
  | FailedConnection s -> "Failed connection: " ^ s

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
      | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
      | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }
