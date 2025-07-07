(** The CLI for kakorn *)

(** Features for kakorn *)
type features =
  | Highlight
  | All

(** Run the CLI *)
val run : unit -> 'a
