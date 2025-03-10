(* The parser state *)
type t = { tokens : Token.t array; cursor : int }

let check_number string =
  let regex = Re.Emacs.compile_pat "^[0-9]+$" in
  Re.execp regex string

(** Get the current token *)
let current p_state =
  if p_state.cursor < Array.length p_state.tokens then
    Some p_state.tokens.(p_state.cursor)
  else None

(** Advance the p_state *)
let advance p_state = { p_state with cursor = p_state.cursor + 1 }

(** Returns a function that attempts to parse using the first and then second
    parser. Returns either the first correctly parsed answer or None. Parser
    state only advances on success *)
let attempt p1 p2 =
 fun p_state ->
  match p1 p_state with Some p_state -> Some p_state | None -> p2 p_state

let ( |? ) p1 p2 = attempt p1 p2

(** Returns a function that parses first p1 then p2. Exits if it encounters
    None. *)
let sequence p1 p2 =
 fun p_state ->
  match p1 p_state with Some p_state -> p2 p_state | None -> None

let ( >> ) p1 p2 = sequence p1 p2
