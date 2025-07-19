(** Various kakoune faces *)
type kak_face =
  | Base
  | Macro
  | MarkupHeading
  | MarkupHeadingMarker
  | MarkupHeading1
  | MarkupHeading2
  | MarkupHeading3
  | MarkupHeading4
  | MarkupHeading5
  | MarkupHeading6
  | MarkupList
  | MarkupUnderline
  | MarkupBold
  | MarkupBoldItalic
  | MarkupItalic
  | MarkupStrikethrough
  | MarkupLink
  | MarkupLinkUrl
  | MarkupQuote
  | MarkupRaw

let kak_face_from_fmt = function
  | Ast.Bold -> MarkupBold
  | Ast.Italics -> MarkupItalic
  | Ast.StrikeThrough -> MarkupStrikethrough
  | Ast.Monospace -> MarkupRaw
  | Ast.Underline -> MarkupUnderline
  | Ast.Sub | Ast.Super -> Base
;;

(** Wrap the AST in a more agnostic way *)
type t =
| Node of (t * kak_face) array (** A Node of the AST *)
| Leaf of Token.Location.t (** A leaf of the AST *)

(** Node behaviour *)
module type Nodelike = sig
  (** Type of the node *)
  type node

  (** Get the children of a node*)
  val children : node -> (t * kak_face) array

  (** Get the location of a node *)
  val location : node -> Token.Location.t
end

type kak_range =
  { offset : int
  ; line : int
  ; length : int
  }

module DelimContentNode : Nodelike with type node := Ast.delimited_content = struct
  open Ast

  let children dc =
    let { open_delim; inner; close_delim } = dc in
    [| Leaf open_delim, Base; Leaf inner, Base; Leaf close_delim, Base |]
  ;;

  let location dc =
    let open Token.Location in
    { offset = dc.open_delim.offset
    ; length = dc.open_delim.length + dc.inner.length + dc.close_delim.length
    }
  ;;
end

module ParamNode : Nodelike with type node := Token.Location.t * Token.Location.t = struct
  let children (param1, param2) = [| Leaf param1, Base; Leaf param2, Base |]

  let location (param1, param2) =
    let open Token.Location in
    { offset = param1.offset; length = param1.length + param2.length }
  ;;
end

module ContentNode : Nodelike with type node := Ast.contents = struct
  open Ast

  let rec children = function
    | Text t -> [| Leaf t, Base |]
    | Fmt { open_fmt; content; end_fmt; fmt_ty; _ } ->
      let array =
        Array.make (Array.length content + 2) (Leaf open_fmt, kak_face_from_fmt fmt_ty)
      in
      Array.iteri
        (fun i c -> array.(1 + i) <- Node (children c), kak_face_from_fmt fmt_ty)
        content;
      array.(Array.length array) <- Leaf end_fmt, kak_face_from_fmt fmt_ty;
      array
    | Link { open_brace; close_brace; address; name; _ } ->
      (match name with
       | Some name ->
         let array = Array.make 4 (Leaf open_brace, MarkupLink) in
         array.(1) <- Node (DelimContentNode.children address), MarkupLinkUrl;
         array.(2) <- Node (DelimContentNode.children name), MarkupLink;
         array.(3) <- Leaf close_brace, MarkupLink;
         array
       | None ->
         let array = Array.make 3 (Leaf open_brace, MarkupLink) in
         array.(1) <- Node (DelimContentNode.children address), MarkupLinkUrl;
         array.(2) <- Leaf close_brace, MarkupLink;
         array)
    | FMacro { symbol; name; open_bracket; param; close_bracket; _ } ->
      let array = Array.make (Array.length param + 3) (Leaf symbol, Macro) in
      array.(1) <- Leaf name, Macro;
      array.(2) <- Leaf open_bracket, Base;
      Array.iteri (fun i p -> array.(3 + i) <- Node (ParamNode.children p), Base) param;
      array.(Array.length array) <- Leaf close_bracket, Macro;
      array
  ;;

  let location = Ast.content_location
end

(** Map from a token location to a line *)
module LocationMap = Map.Make (Token.Location)

(** Create a map from token offset to line *)
let line_map tokens =
let open Token in
Array.fold_left
(fun (line, map) tk ->
let map = LocationMap.add tk.location line map in
if Token.equal_tt Token.Newline tk.ty then line + 1, map else line, map)
(1, LocationMap.empty)
tokens
;;
