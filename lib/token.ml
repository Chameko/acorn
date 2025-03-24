(** Where a token is located. [start] and [stop] are both relative to the line *)
type range =
  { start : int
  ; stop : int
  ; line : int
  }
[@@deriving show, eq]

let add_range r1 r2 =
  if r1.line == r2.line
  then [ { start = r1.start; stop = r2.stop; line = r1.line } ]
  else [ r1; r2 ]
;;

let comp_range r1 r2 =
  if r1.line < r2.line
  then 1
  else if r1.line > r2.line
  then -1
  else if r1.start < r2.start
  then 1
  else if r1.start > r2.start
  then -1
  else if r1.stop < r2.stop
  then -1
  else if r2.stop > r2.stop
  then 1
  else 0
;;

let extract_range source r1 =
  let lines = String.split_on_char '\n' source in
  String.sub (List.nth lines (r1.line - 1)) (r1.start - 1) (r1.stop - r1.start + 1)
;;

(** The type for tokens *)
type tt =
  | Text of string (** General text *)
  | Dash (** - *)
  | Star (** * *)
  | Slash (** / *)
  | Underscore (** _ *)
  | Backtick (** ` *)
  | Tilde (** ~ *)
  | Carret (** ^ *)
  | LSquareB (** \[ *)
  | RSquareB (** \] *)
  | LCurly (** \{ *)
  | RCurly (** \}*)
  | LParen (** \( *)
  | RParen (** \) *)
  | Dot (** . *)
  | AtAt (** @@ *)
  | AtPlus (** @+ *)
  | AtEquals (** @= *)
  | Equals (** = *)
  | Newline (** \n *)
  | Backslash (** \\ *)
  | Comma (** , *)
  | Whitespace of string (** ' ' *)
[@@deriving show, eq]

type t =
  { ty : tt
  ; location : range
  }
[@@deriving show, eq]

let to_string tk =
  match tk with
  | Text str | Whitespace str -> str
  | Dash -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Underscore -> "_"
  | Backtick -> "`"
  | Tilde -> "~"
  | Carret -> "^"
  | LSquareB -> "["
  | RSquareB -> "]"
  | LParen -> "("
  | RParen -> ")"
  | Equals -> "="
  | Comma -> ","
  | Newline -> "\n"
  | Backslash -> "\\"
  | LCurly -> "{"
  | RCurly -> "}"
  | Dot -> "."
  | AtAt -> "@@"
  | AtEquals -> "@="
  | AtPlus -> "@+"
;;

let length tk =
  match tk with
  | Text str | Whitespace str -> String.length str
  | Dash
  | Star
  | Slash
  | Underscore
  | Backtick
  | Tilde
  | Carret
  | LSquareB
  | RSquareB
  | LParen
  | RParen
  | Equals
  | Comma
  | Newline
  | Backslash
  | LCurly
  | RCurly
  | Dot -> 1
  | AtAt | AtPlus | AtEquals -> 2
;;
