(** Containts types that make up the AST *)

type range = { start : int; stop : int; line : int } [@@deriving show, eq]
(** Where a token is located. [start] and [stop] are both relative to the line
*)

type area = range array [@@deriving show, eq]
(** Multiple [range] *)

type raw = string * area [@@deriving show, eq]
(** Used when we need to know what the [arera] contains *)

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

(** Lowest level container in acorn *)
type contents =
  | Text of area  (** Text *)
  | Fmt of {
      content : contents;  (** The inner text to be formatted*)
      t_fmt : formatting;  (** The formatting type *)
      location : area;
    }  (** Formatted text *)
  | Link of {
      open_brace : range;  (** Opening brace of link *)
      address : range * contents * range;
          (** Contents and opening/closing braces of address section *)
      name : (range * contents * range) option;
          (** Contente and opening/closing braces of name section *)
      close_brace : range;  (** Closing brace *)
      location : area;
    }  (** Link *)
  | FMacro of {
      symbol : range;  (** @@ symbol of the macro *)
      name : raw;  (** Name of macro*)
      open_bracket : range;  (** opening bracket of params *)
      param : (raw * raw) list;
          (** Parameters of macro with their separating commas *)
      close_bracket : range;  (** closing bracket of params *)
      location : area;
    }  (** Function macro *)
[@@deriving show, eq]

(** Blocks that make up an acorn file *)
type block =
  | UList of {
      elements : (range * block) list;
          (** Elements in an unordered list including the "- " prefix *)
      location : area;
    }  (** Unordered list *)
  | OList of {
      elements : (range * block) list;
          (** Elements in an ordered list including the "1. " prefix*)
      location : area;
    }  (** Ordered list *)
  | Section of {
      elements : contents list;  (** Contentes that make up a section *)
      location : area;
    }  (** Multiple contents *)
  | BMacro of {
      opening_symbol : range;  (** @+ symbol of the macro*)
      name : raw;  (** Name of the macro *)
      open_bracket : range;  (** Opening bracket of macro *)
      param : (raw * raw) list;
          (** Parameters of macro with their separating commas *)
      close_bracket : range;  (** Closing brace of macro *)
      text : raw;  (** Contents of block macro *)
      ending_symbol : range;  (** Ending symbol of block macro *)
    }  (** Block macro *)
  | AMacro of {
      opening_symbol : range;  (** @= of attribute macro *)
      name : raw;  (** Name of macro*)
      open_bracket : range;  (** Opening bracket of macro *)
      param : (raw * raw) list;
          (** Parameters of macro with their separating commas *)
      close_bracket : range;  (** Closing bracket of macro *)
      block : block;  (** Block attribute macro is attached to *)
    }  (** Attribute macro *)
  | Heading of {
      level : range;  (** Heading "=" characters *)
      name : contents list;  (** Name of heading *)
      block : block list;  (** Blocks the heading contains *)
    }  (** Heading *)
[@@deriving show, eq]

type t = { file : block list } [@@deriving show, eq]

let token_to_emph = function
  | Token.Star -> Some Bold
  | Token.Slash -> Some Italics
  | Token.Tilde -> Some StrikeThrough
  | Token.Underscore -> Some Underline
  | Token.Backtick -> Some Monospace
  | _ -> None

let emph_to_token = function
  | Bold -> Token.Star
  | Italics -> Token.Slash
  | StrikeThrough -> Token.Tilde
  | Underline -> Token.Underscore
  | Monospace -> Token.Backtick
  | Super -> Token.Carret
  | Sub -> Token.Underscore
