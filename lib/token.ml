(** Where a token is located *)
type loc =
  { offset : int (** Offset from the start of the file *)
  ; length : int (** The length of the token *)
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
  ; location : loc
  }
[@@deriving show, eq]

let to_string tk =
  match tk with
  | Text t -> t
  | Whitespace w -> w
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
