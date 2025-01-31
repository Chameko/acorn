(** Various error types for the server *)
type error =
  NoRuntimeDir
| IOError of string * string
| ServerAlreadyRunning

let output = function
  | NoRuntimeDir -> "Cannot find runtime directory"
  | ServerAlreadyRunning -> "Server is already running"
  | IOError (s1, s2) -> "I/O Error for " ^ s1 ^ " . Reason: " ^ s2
