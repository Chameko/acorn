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
  { open_delim : Location.t
  ; inner : Location.t
  ; close_delim : Location.t
  }
[@@deriving show, eq]

(** Lowest level container in acorn *)
type contents =
  | Text of Location.t (** Text *)
  | Fmt of
      { open_fmt : Location.t (** Opening format tag *)
      ; content : contents array (** The inner text to be formatted *)
      ; end_fmt : Location.t (** Ending format tag *)
      ; fmt_ty : formatting (** The formatting type *)
      ; location : Location.t
      } (** Formatted text *)
  | Link of
      { open_brace : Location.t (** Opening brace of link *)
      ; address : delimited_content
        (** Contents and opening/closing braces of address section *)
      ; name : delimited_content option
        (** Contents and opening/closing braces of name section *)
      ; close_brace : Location.t (** Closing brace *)
      ; location : Location.t
      } (** Link *)
  | FMacro of
      { symbol : Location.t (** @@ symbol of the macro *)
      ; name : Location.t (** Name of macro*)
      ; open_bracket : Location.t (** opening bracket of params *)
      ; param : (Location.t * Location.t) array
        (** Parameters of macro with their separating commas *)
      ; close_bracket : Location.t (** closing bracket of params *)
      ; location : Location.t
      } (** Function macro *)
[@@deriving show, eq]

let content_location = function
  | Text t -> t
  | Fmt { location; _ } | Link { location; _ } | FMacro { location; _ } -> location
;;

(** Blocks that make up an acorn file *)
type block =
  | UList of
      { elements : (Location.t * block) array
        (** Elements in an unordered list including the "- " prefix *)
      ; location : Location.t
      } (** Unordered list *)
  | OList of
      { elements : (Location.t * block) array
        (** Elements in an ordered list including the "1. " prefix*)
      ; location : Location.t
      } (** Ordered list *)
  | Section of
      { elements : contents list (** Contentes that make up a section *)
      ; location : Location.t
      } (** Multiple contents *)
  | BMacro of
      { opening_symbol : Location.t (** @+ symbol of the macro*)
      ; name : Location.t (** Name of the macro *)
      ; open_bracket : Location.t (** Opening bracket of macro *)
      ; param : (Location.t * Location.t) array
        (** Parameters of macro with their separating commas *)
      ; close_bracket : Location.t (** Closing brace of macro *)
      ; text : Location.t (** Contents of block macro *)
      ; ending_symbol : Location.t (** Ending symbol of block macro *)
      } (** Block macro *)
  | AMacro of
      { opening_symbol : Location.t (** @= of attribute macro *)
      ; name : Location.t (** Name of macro*)
      ; open_bracket : Location.t (** Opening bracket of macro *)
      ; param : (Location.t * Location.t) array
        (** Parameters of macro with their separating commas *)
      ; close_bracket : Location.t (** Closing bracket of macro *)
      ; block : block (** Block attribute macro is attached to *)
      } (** Attribute macro *)
  | Heading of
      { level : Location.t (** Heading "=" characters *)
      ; name : contents array (** Name of heading *)
      ; block : block array (** Blocks the heading contains *)
      } (** Heading *)
[@@deriving show, eq]

type t = { file : block array } [@@deriving show, eq]

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

let extract_location source loc =
  let open Location in
  String.sub source loc.offset loc.length
;;
