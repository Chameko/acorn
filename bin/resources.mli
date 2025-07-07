(** Helper functions for various resources *)

(** Stores the runtime dir and is used to construct the various paths *)
type paths = { runtime_dir : string }

(** Get the runtime dir and create paths *)
val paths : unit -> (paths, K_error.error) result

(** Get the PID file *)
val pid_file : paths -> string

(** Get the kakorn runtime dir *)
val kakorn_dir : paths -> string

(** Get the kakorn buffer dir *)
val buff_dir : paths -> string

(** Get a kakorn buffer FIFO file path. Takes in the buffer name and session id *)
val buff_file : paths -> int -> string -> string

(** Get the redirected stderr file *)
val stderr_file : paths -> string

(** Get the redirected stdout file *)
val stdout_file : paths -> string

(** Get the socket file *)
val socket_file : paths -> string

(** Get the kakoune dir *)
val kak_dir : paths -> string

(** Get the kakoune socket *)
val kak_socket : paths -> int -> string

(** Create the PID file. Creates an error if the PID matches an running process *)
val create_pid : paths -> (unit, K_error.error) result

(** Create the kakorn runtime directory if it doesn't exist *)
val create_dir : paths -> (unit, K_error.error) result

(** Create a socket *)
val create_socket : unit -> (Lwt_unix.file_descr, K_error.error) result

(** Create a kakorn server socket *)
val create_server_socket
  :  paths
  -> unit
  -> (Lwt_unix.file_descr, K_error.error) result Lwt.t
