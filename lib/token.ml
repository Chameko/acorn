(** Where a token is located. [start] and [stop] are both relative to the line *)
type range =
  { start : int
  ; stop : int
  ; line : int
  }
[@@deriving show, eq]

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
