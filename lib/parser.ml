open Ast

(* The parser state *)
type t = {
    tokens : Token.t array;
    cursor : int;
    ast : Green.t;
  }

(* A function that parses something. Starts parsing on the [current] token and
   leaves on the last parsed token. As a general rule the parser should only advance
   on succesfull parsing *)
and 'a parser = t -> t option

let add_item token p_state =
  {p_state with ast = Green.add_item p_state.ast token}

let start_node node_type p_state =
  {p_state with ast = Green.start_node p_state.ast node_type}

let end_node p_state =
  {p_state with ast = Green.end_node p_state.ast}
  
(* Get the current token *)
let current p_state =
  if p_state.cursor < (Array.length p_state.tokens) then
    Some p_state.tokens.(p_state.cursor)
  else
    None

(* Advance the p_state *)
let advance p_state =
  {p_state with cursor = p_state.cursor + 1}

(* Attempt to parse using list of parsers in order. Returns either the first correctly parsed
 answer or None. Parser state only advances from parsing on success *)
let rec attempt (parsers : 'a parser list) p_state =
  match parsers with
  | [] -> None
  | h :: t ->
     match h p_state with
     | None -> attempt t p_state
     | success -> success

let whitespace_parser p_state =
  match current p_state with
  | Some Token.Whitespace w -> Some ( add_item (Token.Whitespace w) (advance p_state))
  | _ -> None

let nl_parser p_state =
  match current p_state with
  | Some Token.Newline -> Some (add_item (Token.Newline) (advance p_state))
  | _ -> None

let whitespace_or_nl_parser p_state =
  attempt [whitespace_parser; nl_parser] p_state

(** Parse text. Text is anything that could be seen as a word *)
let text_parser p_state =
  match current p_state with
  | Some Token.Text t -> Some (add_item (Token.Text t) (advance p_state))
  | _ -> None

(** Parse emphasis char *)
let emph_char_parser p_state =
  match current p_state with
  | Some Token.Star -> Some (add_item Token.Star (advance p_state))
  | Some Token.Slash -> Some (add_item Token.Slash (advance p_state))
  | Some Token.Tilde -> Some (add_item Token.Tilde (advance p_state))
  | Some Token.Underscore -> Some (add_item Token.Underscore (advance p_state))
  | Some Token.Backtick -> Some (add_item Token.Backtick (advance p_state))
  | _ -> None

(** Parse start of emphasis. Starts on emphasis and
   ends on char after emphasis. Returns both p_state and emphasis *)
let rec start_emph_parser p_state =
  let p_state, tk = (
      match current p_state with
      | Some Token.Star -> (emph_char_parser (start_node Green.Bold p_state ), Token.Star)
      | Some Token.Slash -> (emph_char_parser (start_node Green.Italics p_state ), Token.Slash)
      | Some Token.Tilde -> (emph_char_parser (start_node Green.StrikeThrough p_state ), Token.Tilde)
      | Some Token.Underscore -> (emph_char_parser (start_node Green.Underline p_state ), Token.Underscore)
      | Some Token.Backtick -> (emph_char_parser (start_node Green.Monospace p_state ), Token.Backtick)
      | _ -> (None, Token.AtAt)
    )
  in
  (* Check if its a emphasis character *)
  match p_state with
  | None -> None
  | Some p_state ->
     (* Check for more emphasis character *)
     match start_emph_parser p_state with
     (* Add our emphasis to the list*)
     | Some (p_state, tkl) -> Some (p_state, tk :: tkl)
     | None ->
        (* We need not whitespace *)
        match whitespace_or_nl_parser p_state with
        | None -> Some (p_state, [tk])
        | Some _ -> None

(* Parse end of emphasis. Starts on emphasis
 and ends on char after emphasis. Verifies that *)
let rec end_emph_parser tk p_state =
  let valid_ending ~p_state =
    (* Check for not text after emphasis character *)
    match whitespace_or_nl_parser p_state with
    | Some _ -> Some p_state
    | None ->
       (* Allow for end of file termination *)
       match current p_state with
       | Some _ -> None
       | None -> Some p_state
  in
  
  let p_state, tk2 = (
      match current p_state with
      | Some Token.Star -> (emph_char_parser p_state, Token.Star)
      | Some Token.Slash -> (emph_char_parser p_state, Token.Slash)
      | Some Token.Tilde -> (emph_char_parser p_state, Token.Tilde)
      | Some Token.Underscore -> (emph_char_parser p_state, Token.Underscore)
      | Some Token.Backtick -> (emph_char_parser p_state, Token.Backtick)
      | _ -> (None, Token.AtAt)
    )
  in
  (* Check if an emphasis character has been parsed*)
  match p_state with
  | None -> None
  | Some p_state ->
     (* If we've found the emphasis we're looking for we keep parsing
        to ensure that it has valid formatting and discard the rest of
        the state *)
     if tk == tk2 then
       match end_emph_parser tk p_state with
       | Some _ -> Some p_state
       | None -> valid_ending ~p_state
     else
       (* If we haven't found the emphasis keep going and update the state *)
       match end_emph_parser tk p_state with
       | Some p_state -> Some p_state
       | None -> valid_ending ~p_state


(** Parse some contents. Handles special cases such as emphasis, then exit cases,
    then everything else in converted into text. Exit param is a function for any
    extra exit conditions you want to add to the contents parser *)
let rec contents_parser p_state ?(exit=fun _ -> None) () =
  let continue p_state =
    match as_text_parser p_state with
    | Some p_state -> contents_parser p_state ~exit  ()
    | None -> p_state
  in
    
  match current p_state with
  | None -> p_state
  (* Handle emphasis *)
  | Some Token.Whitespace w ->
     (match emph_text_parser (add_item (Token.Whitespace w) (advance p_state))
      with
      | Some p_state -> contents_parser p_state ~exit ()
      | None -> continue p_state)
  (* Handle links *)
  | Some Token.LSquareB ->
     (match link_parser p_state with
      | Some p_state -> contents_parser p_state ~exit ()
      | None -> continue p_state)
  | _ ->
     (* Special exit conditions *)
     match exit p_state with
     | Some p_state -> p_state
     | None ->
        (* Newline is a common delimiter and is therefore handled after
           exit *)
        match current p_state with
        | Some Token.Newline ->
           let p_state = advance (add_item Token.Newline p_state) in
           (match current p_state with
            (* Unordered list *)
            | Some Token.Dash ->
               (match ulist_parser (start_node Green.UList p_state) with
               | Some p_state -> contents_parser (end_node p_state) ~exit  ()
               | None -> contents_parser p_state ~exit ())
            | _ -> continue p_state)
        | _ -> continue p_state

(** Parse emphasised text. Exit allows for custom terminating delimiters which
    allow emphasis to end on them *)
and emph_text_parser p_state =
  match start_emph_parser p_state with
  | None -> None
  | Some (p_state, tkl) ->
     Some (emphasis_stack_parser p_state tkl)

(** Parse stacked emphasis. Ensures functions are run in
    the correct order. Exit allows for custom terrminating delimiters which allow
    emphasis to end on them *)
and emphasis_stack_parser p_state = function
  | [] -> p_state
  | hd :: tl ->
     let p_state = emphasis_stack_parser p_state tl in
     (* The only way we exit on a valid emphasis character is if we were parsing a
      stack of them. As such we check if our emphasis is next in the chain *)
     match end_emph_parser hd p_state with
     | Some p_state -> end_node p_state
     | None -> 
        end_node (contents_parser
          p_state
          ~exit:(
            fun p_state ->
            match text_parser p_state with
            | Some p_state -> end_emph_parser hd p_state
            | _ -> None
          )
          ())

(** Parse the token as text *)
and as_text_parser p_state =
  match current p_state with
  | Some tk -> Some (add_item (tk) (advance p_state))
  | None -> None

(** Parses links. Bails on improper formatting of links *)
and link_parser p_state =
  (* Function for parsing the url *)
  let rec link_url_parser p_state =
    match current p_state with
    (* If we run into a RSquareB we exit *)
    | Some Token.RSquareB -> (add_item Token.RSquareB (advance p_state))
    (* If we run into a LSquareB we go down two levels so we have
       to exit twice. This balances the brackets *)
    | Some Token.LSquareB ->
       link_url_parser
         (link_url_parser (add_item Token.LSquareB (advance p_state)))
    | _ ->
       match as_text_parser p_state with
       | Some p_state -> link_url_parser p_state
       | None -> p_state
  in
  
  (* Function for parsing a links name *)
  let rec link_name_parser p_state =
    (* We allow pretty much anything in the title so we
       use the content parser with special exit rules*)
    let exit p_state =
      match current p_state with
      (* If we find a LParen we go down two levels so we have to exit twice.
         This balances the brackets *)
      | Some Token.LParen ->
         Some (link_name_parser (
                   link_name_parser (
                       advance (add_item Token.LParen p_state))))
      (* If we run into the RParen we exit *)
      | Some Token.RParen ->
         Some (add_item Token.RParen p_state |> advance)
      | _ -> None
    in
    contents_parser
      p_state
      ~exit
      ()
  in

  (* Convenience function for closing the link *)
  let close_link p_state =
    match current p_state with
    | Some Token.RSquareB ->
       Some (add_item Token.RSquareB p_state |> advance |> end_node)
    | _ -> None
  in

  (* Start link parsing *)
  let p_state = start_node Green.Link p_state in
  match (current p_state, current (advance p_state)) with
  (* Start with [[]()] format *)
  | (Some Token.LSquareB,
     Some Token.LSquareB) ->
     (* Parse the link url *)
     let p_state =
       p_state
       |> advance |> add_item Token.LSquareB
       |> start_node Green.Url
       |> advance |> add_item Token.LSquareB
       |> link_url_parser
       |> end_node
     in
     (* Check we don't run into anything unexpected *)
     (match current p_state with
      | Some Token.LParen ->
         (* Parse the link title *)
         let p_state =
           p_state |> start_node Green.Name
           |> add_item Token.LParen |> advance
           |> link_name_parser
           |> end_node
         in
         (* Close the link *)
         close_link p_state
      (* Bail if we run into bad formatting *)
      | _ -> None)
  | (Some Token.LSquareB,
     Some Token.LParen) ->
     (* Start with [()[]] reverse format *)
     let p_state =
       (* Parse the link title *)
       p_state |> advance |> add_item Token.LSquareB
       |> start_node Green.Name
       |> advance |> add_item Token.LParen
       |> link_name_parser
       |> end_node
     in
     (* Check we don't run into anything unexpected *)
     (match current p_state with
      | Some Token.LSquareB ->
         (* Parse link url *)
         let p_state =
           p_state |> start_node Green.Url
           |> add_item Token.LSquareB |> advance
           |> link_url_parser
           |> end_node
         in
         (* Close the link *)
         close_link p_state
      | _ -> None)
  | _ -> None

(** Unordered list parser *)
and ulist_parser p_state =
  let rec ulist_level_parser p_state =
    match current p_state with
    | Some Token.Dash ->
       ulist_level_parser (advance (add_item Token.Dash p_state))
    | Some Token.Whitespace w ->
       Some (advance (add_item (Token.Whitespace w) p_state))
    | Some Token.Newline ->
       Some p_state
    | _ -> None
  in

  (* Parse the list *)
  match ulist_level_parser p_state with
  (* We have list markers *)
  | Some p_state ->
     (* Parse normal content until Newline *)
     let p_state = contents_parser
       p_state
       ~exit:(fun p_state ->
         match current p_state with
         | Some Token.Newline ->
            Some (advance (add_item Token.Newline p_state))
         | _ -> None)
       ()
     in
     (match ulist_parser p_state with
     | Some p_state -> Some p_state
     | None -> Some p_state)
  | None -> None


(** Parse the initial contents. Special due to emphasis not requiring
 whitespace before the emphasis character *)
let init_content_parser p_state =
  match current p_state with
  | None -> p_state
  | Some Token.Star
    | Some Token.Slash
    | Some Token.Tilde
    | Some Token.Underscore
    | Some Token.Backtick ->
     (match emph_text_parser p_state with
     | Some p_state -> contents_parser p_state ()
     | None ->
        (match as_text_parser p_state with
        | Some p_state -> contents_parser p_state ()
        | None -> p_state))
  | Some Token.LSquareB ->
     (match link_parser p_state with
      | Some p_state -> contents_parser p_state ()
      | None ->
         (match as_text_parser p_state with
          | Some p_state -> contents_parser p_state ()
          | None -> p_state))
  | Some Token.Dash ->
     (match ulist_parser (start_node Green.UList p_state) with
      | Some p_state -> contents_parser (end_node p_state) ()
      | None -> contents_parser p_state ())
  | _ ->
     match as_text_parser p_state with
     | Some p_state -> contents_parser p_state ()
     | None -> p_state

let parse tokens =
  let p_state = init_content_parser {tokens = tokens; cursor = 0; ast = Green.create }
  in
  {p_state with ast = ({p_state.ast with ast = List.rev p_state.ast.ast})}
