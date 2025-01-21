
module Green = struct
  type range = { start: int; stop : int; }
  [@@deriving show, eq]

  type node_type =
    | Block
    | Heading
    | UList
    | OList
    | BMacro
    | FMacro
    | AMacro
    | ParamList
    | Bold
    | Italics
    | StrikeThrough
    | Underline
    | Monospace
    | Super
    | Sub
    | Url
    | Link
    | Name
 [@@deriving show, eq]

  type tree_contents =
    | Node of {ty : node_type; start : int; }
    | Item of {ty : Token.t; location : range; }
    | NodeEnd of {stop : int;}
[@@deriving show, eq]
  
  type t = {
      ast : tree_contents list;
      depth : int;
      len : int;
    }
[@@deriving show, eq]

  let create = {ast = []; depth = 0; len = 0}

  (** Start Node *)
  let start_node ast node_type =
    {ast with ast = Node {ty = node_type; start = ast.len} :: ast.ast; depth = ast.depth + 1}
  
  (** Start Node from nothing *)
  let start_with node_type =
    start_node create node_type

  (** Start ast from a certain point *)
  let start_from node_type len =
    {ast = [Node {ty = node_type; start = len}]; depth = 1; len = len}

  (** End node *)
  let end_node ast =
    if ast.depth - 1 >= 0 then
      {ast with ast = NodeEnd {stop = ast.len} :: ast.ast; depth = ast.depth - 1}
    else
      failwith "Attempted to end node with no more nodes t"

  (** Add item *)
  let add_item ast token_type =
    let location = {start = ast.len; stop = ast.len + Token.length token_type} in
    {ast with
      ast = Item {ty = token_type; location = location;} :: ast.ast;
      len = ast.len + (Token.length token_type);
    }

  (** Get the length of the ast in characters *)
  let length ast =
    match List.nth ast (List.length ast - 1) with
    | Node n -> n.start
    | NodeEnd n -> n.stop
    | Item i -> i.location.stop

  (** Update the ast by setting a new start point *)
  let update ast len =
    let update_len = function
      | Node n -> Node { ty = n.ty; start = len + n.start}
      | NodeEnd n -> NodeEnd {stop = n.stop + len}
      | Item i -> Item {
                      ty = i.ty;
                      location = { start = i.location.start + len ; stop = i.location.stop + len}
                    }
    in
    List.map update_len ast
  
  (** Attach a node to an existing green ast *)
  let attach ast1 ast2 =
    if ast2.depth != 0 then
      failwith "Attempted to attach unfinished tree"
    else
      let new_ast = update ast2.ast ast1.len in
      {ast1 with ast = new_ast ; len = length new_ast}

  (** Pretty print the cst *)
  let pp_cst ppf cst =
    let open Format in
    let rec walk_cst ppf = function
      | [] -> ()
      | hd :: tl ->
         match hd with
         | Node n ->
            fprintf ppf "@ %a at %d" pp_node_type n.ty n.start;
            pp_open_vbox ppf 0;
            walk_cst ppf  tl
         | Item i -> fprintf ppf "@ %a at %d..%d" Token.pp i.ty i.location.start i.location.stop;
                     walk_cst ppf tl
         | NodeEnd e ->
            pp_close_box ppf ();
            fprintf ppf "@ end at %d" e.stop;
            walk_cst ppf  tl
  in
  fprintf ppf "@[<v>%a@]" walk_cst cst.ast
end

module Red = struct
  
  type emphasis =
    | Bold
    | Italics
    | StrikeThrough
    | Underline
    | Monospace
    | Super
    | Sub

  type text = { text : string ; location : Green.range ;}

  type contents =
    | Section of contents list
    | Text of string
    | Emph of { content : contents; t_emphasis : emphasis ; }
    | Link of { address : string; name : string option ;}
    | FMacro of { name : string; param : string list; }

  type block =
    | UList of contents list
    | OList of contents list
    | Basic of contents list
    | BMacro of { name : string; param : string list; text : string; }
    | AMacro of { name : string; param : string list; contents : contents list; }
    | Heading of { level : int ; name : contents list ; block : block list; }
    | LineBreak
  
  type t = {
      file : block list
    }

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

  let rec fold_contents contents =
    match contents with
    | [] -> []
    | hd :: tl ->
       match hd with
       | Some content -> content :: fold_contents tl
       | None -> fold_contents tl
end
