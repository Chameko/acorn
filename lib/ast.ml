open Token

(** Containts types that make up the AST *)

(** Various formatting effects applied to text *)
type formatting =
  | Bold
  | Italics
  | StrikeThrough
  | Underline
  | Monospace
  | Super
  | Sub
[@@deriving show, eq]

(** Content with delimiters *)
type delimited_content =
  { open_delim : loc
  ; inner : loc
  ; close_delim : loc
  }
[@@deriving show, eq]

(** Lowest level container in acorn *)
type contents =
  | Text of loc (** Text *)
  | Fmt of
      { open_fmt : loc (** Opening format tag *)
      ; content : contents array (** The inner text to be formatted *)
      ; end_fmt : loc (** Ending format tag *)
      ; fmt_ty : formatting (** The formatting type *)
      ; location : loc
      } (** Formatted text *)
  | Link of
      { open_brace : loc (** Opening brace of link *)
      ; address : delimited_content
        (** Contents and opening/closing braces of address section *)
      ; name : delimited_content option
        (** Contents and opening/closing braces of name section *)
      ; close_brace : loc (** Closing brace *)
      ; location : loc
      } (** Link *)
  | FMacro of
      { symbol : loc (** @@ symbol of the macro *)
      ; name : loc (** Name of macro*)
      ; open_bracket : loc (** opening bracket of params *)
      ; param : (loc * loc) list (** Parameters of macro with their separating commas *)
      ; close_bracket : loc (** closing bracket of params *)
      ; location : loc
      } (** Function macro *)
[@@deriving show, eq]

let content_location = function
  | Text t -> t
  | Fmt { location; _ } | Link { location; _ } | FMacro { location; _ } -> location
;;

(** Blocks that make up an acorn file *)
type block =
  | UList of
      { elements : (loc * block) list
        (** Elements in an unordered list including the "- " prefix *)
      ; location : loc
      } (** Unordered list *)
  | OList of
      { elements : (loc * block) list
        (** Elements in an ordered list including the "1. " prefix*)
      ; location : loc
      } (** Ordered list *)
  | Section of
      { elements : contents list (** Contentes that make up a section *)
      ; location : loc
      } (** Multiple contents *)
  | BMacro of
      { opening_symbol : loc (** @+ symbol of the macro*)
      ; name : loc (** Name of the macro *)
      ; open_bracket : loc (** Opening bracket of macro *)
      ; param : (loc * loc) list (** Parameters of macro with their separating commas *)
      ; close_bracket : loc (** Closing brace of macro *)
      ; text : loc (** Contents of block macro *)
      ; ending_symbol : loc (** Ending symbol of block macro *)
      } (** Block macro *)
  | AMacro of
      { opening_symbol : loc (** @= of attribute macro *)
      ; name : loc (** Name of macro*)
      ; open_bracket : loc (** Opening bracket of macro *)
      ; param : (loc * loc) list (** Parameters of macro with their separating commas *)
      ; close_bracket : loc (** Closing bracket of macro *)
      ; block : block (** Block attribute macro is attached to *)
      } (** Attribute macro *)
  | Heading of
      { level : loc (** Heading "=" characters *)
      ; name : contents list (** Name of heading *)
      ; block : block list (** Blocks the heading contains *)
      } (** Heading *)
[@@deriving show, eq]

type t = { file : block list } [@@deriving show, eq]

let token_to_fmt = function
  | Token.Star -> Some Bold
  | Token.Slash -> Some Italics
  | Token.Tilde -> Some StrikeThrough
  | Token.Underscore -> Some Underline
  | Token.Backtick -> Some Monospace
  | _ -> None
;;

let fmt_to_token = function
  | Bold -> Token.Star
  | Italics -> Token.Slash
  | StrikeThrough -> Token.Tilde
  | Underline -> Token.Underscore
  | Monospace -> Token.Backtick
  | Super -> Token.Carret
  | Sub -> Token.Underscore
;;

let fmt_of_char = function
  | '*' -> Some Bold
  | '/' -> Some Italics
  | '~' -> Some StrikeThrough
  | '_' -> Some Underline
  | '`' -> Some Monospace
  | _ -> None
;;
