(** Error type for the server *)
type error =
  | CommandIOFailure of string (** Error for running external commands *)
  | FileIOFailure of string (** Error for File IO *)
  | ServerAlreadyRunning (** Error for if the server is already running *)
  | SocketCreationFailure of string (** Error in creating the socket *)
  | SocketIOFailure of string (** Error in performing socket IO *)
  | BufferNotifFailure (** Failure to find a registered buffer *)

(** Convert the error message to a string *)
val output : error -> string

(** The logs reporter that functions with Lwt, ensuring messages come out in the correct order*)
val lwt_reporter : unit -> Logs.reporter

(** Log an error and ignore the returned unit *)
val log_and_ignore : (unit, error) result Lwt.t -> string -> unit Lwt.t
