type t =
  | Node of t array
  | Leaf of Token.Location.t

module LocationMap = Map.Make (Token.Location)

module type Nodelike = sig
  type node

  val children : node -> t array
  val location : node -> Token.Location.t
end

type kak_range =
  { offset : int
  ; line : int
  ; length : int
  }

let line_map tokens =
  let open Token in
  Array.fold_left
    (fun (line, map) tk ->
       let map = LocationMap.add tk.location line map in
       if Token.equal_tt Token.Newline tk.ty then line + 1, map else line, map)
    (1, LocationMap.empty)
    tokens
;;

module DelimContentNode : Nodelike with type node := Ast.delimited_content = struct
  open Ast

  let children dc =
    let { open_delim; inner; close_delim } = dc in
    [| Leaf open_delim; Leaf inner; Leaf close_delim |]
  ;;

  let location dc =
    let open Token.Location in
    { offset = dc.open_delim.offset
    ; length = dc.open_delim.length + dc.inner.length + dc.close_delim.length
    }
  ;;
end

module ParamNode : Nodelike with type node := Token.Location.t * Token.Location.t = struct
  let children (param1, param2) = [| Leaf param1; Leaf param2 |]

  let location (param1, param2) =
    let open Token.Location in
    { offset = param1.offset; length = param1.length + param2.length }
  ;;
end

module ContentNode : Nodelike with type node := Ast.contents = struct
  open Ast

  let rec children = function
    | Text t -> [| Leaf t |]
    | Fmt { open_fmt; content; end_fmt; _ } ->
      let array = Array.make (Array.length content + 2) (Leaf open_fmt) in
      Array.iteri (fun i c -> array.(1 + i) <- Node (children c)) content;
      array.(Array.length array) <- Leaf end_fmt;
      array
    | Link { open_brace; close_brace; address; name; _ } ->
      (match name with
       | Some name ->
         let array = Array.make 4 (Leaf open_brace) in
         array.(1) <- Node (DelimContentNode.children address);
         array.(2) <- Node (DelimContentNode.children name);
         array.(3) <- Leaf close_brace;
         array
       | None ->
         let array = Array.make 3 (Leaf open_brace) in
         array.(1) <- Node (DelimContentNode.children address);
         array.(2) <- Leaf close_brace;
         array)
    | FMacro { symbol; name; open_bracket; param; close_bracket; _ } ->
      let array = Array.make (Array.length param + 3) (Leaf symbol) in
      array.(1) <- Leaf name;
      array.(2) <- Leaf open_bracket;
      Array.iteri (fun i p -> array.(3 + i) <- Node (ParamNode.children p)) param;
      array.(Array.length array) <- Leaf close_bracket;
      array
  ;;

  let location = Ast.content_location
end
