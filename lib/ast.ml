open Token

(** Containts types that make up the AST *)

(** Multiple [range] *)
type area = range list [@@deriving show, eq]

(** Used when we need to know what the [arera] contains *)
type raw = string * area [@@deriving show, eq]

let flatten_ranges ranges =
  let rec driver prev list =
    match list with
    | [] -> []
    | hd :: tl ->
      let combination = add_range prev hd in
      (match combination with
       | [ combination ] | [ _; combination ] -> driver combination tl
       | _ -> failwith "unreachable")
  in
  let ranges = List.sort comp_range ranges in
  driver (List.hd ranges) (List.tl ranges)
;;

let flatten_areas areas =
  let ranges = List.flatten areas in
  flatten_ranges ranges
;;

let rec extract_area source area =
  match area with
  | [] -> ""
  | hd :: tl -> extract_range source hd ^ extract_area source tl
;;

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

type delimited_content =
  { open_delim: range; inner: area; close_delim: range}
[@@deriving show, eq]

(** Lowest level container in acorn *)
type contents =
  | Text of area (** Text *)
  | Fmt of
      { open_fmt : range (** Opening format tag *)
      ; content : contents list (** The inner text to be formatted *)
      ; end_fmt : range (** Ending format tag *)
      ; fmt_ty : formatting (** The formatting type *)
      ; location : area
      } (** Formatted text *)
  | Link of
      { open_brace : range (** Opening brace of link *)
        (** Contents and opening/closing braces of address section *)
      ; address : delimited_content
        (** Contents and opening/closing braces of name section *)
      ; name : delimited_content option
      ; close_brace : range (** Closing brace *)
      ; location : area
      } (** Link *)
  | FMacro of
      { symbol : range (** @@ symbol of the macro *)
      ; name : range (** Name of macro*)
      ; open_bracket : range (** opening bracket of params *)
      ; param : (raw * raw) list (** Parameters of macro with their separating commas *)
      ; close_bracket : range (** closing bracket of params *)
      ; location : area
      } (** Function macro *)
[@@deriving show, eq]

let content_location = function
  | Text t -> t
  | Fmt { location; _ } | Link { location; _ } | FMacro { location; _ } -> location
;;

(** Blocks that make up an acorn file *)
type block =
  | UList of
      { elements : (range * block) list
        (** Elements in an unordered list including the "- " prefix *)
      ; location : area
      } (** Unordered list *)
  | OList of
      { elements : (range * block) list
        (** Elements in an ordered list including the "1. " prefix*)
      ; location : area
      } (** Ordered list *)
  | Section of
      { elements : contents list (** Contentes that make up a section *)
      ; location : area
      } (** Multiple contents *)
  | BMacro of
      { opening_symbol : range (** @+ symbol of the macro*)
      ; name : raw (** Name of the macro *)
      ; open_bracket : range (** Opening bracket of macro *)
      ; param : (raw * raw) list (** Parameters of macro with their separating commas *)
      ; close_bracket : range (** Closing brace of macro *)
      ; text : raw (** Contents of block macro *)
      ; ending_symbol : range (** Ending symbol of block macro *)
      } (** Block macro *)
  | AMacro of
      { opening_symbol : range (** @= of attribute macro *)
      ; name : raw (** Name of macro*)
      ; open_bracket : range (** Opening bracket of macro *)
      ; param : (raw * raw) list (** Parameters of macro with their separating commas *)
      ; close_bracket : range (** Closing bracket of macro *)
      ; block : block (** Block attribute macro is attached to *)
      } (** Attribute macro *)
  | Heading of
      { level : range (** Heading "=" characters *)
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
