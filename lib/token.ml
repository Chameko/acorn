(** The type for tokens *)
type t =
  | Text of string  (** General text *)
  | Dash  (** - *)
  | Star  (** * *)
  | Slash  (** / *)
  | Underscore  (** _ *)
  | Backtick  (** ` *)
  | Tilde  (** ~ *)
  | Carret  (** ^ *)
  | LSquareB  (** \[ *)
  | RSquareB  (** \] *)
  | LCurly  (** \{ *)
  | RCurly  (** \}*)
  | LParen  (** \( *)
  | RParen  (** \) *)
  | Dot  (** . *)
  | AtAt  (** @@ *)
  | AtPlus  (** @+ *)
  | AtEquals  (** @= *)
  | Equals  (** = *)
  | Newline  (** \n *)
  | Backslash  (** \\ *)
  | Comma  (** , *)
  | Whitespace of string  (** ' ' *)
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

let length tk =
  match tk with
  | Text str | Whitespace str -> String.length str
  | Dash | Star | Slash | Underscore | Backtick | Tilde | Carret | LSquareB
  | RSquareB | LParen | RParen | Equals | Comma | Newline | Backslash | LCurly
  | RCurly | Dot ->
      1
  | AtAt | AtPlus | AtEquals -> 2
