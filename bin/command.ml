open Sexplib.Std

module Client = struct
  (** Replies to send to the client *)
  type reply =
    | Debug of int
    | Output of int
    | End
  [@@deriving sexp]

  let client_debug msg oc =
    let%lwt () =
      Lwt_io.write_line
        oc
        (Debug (String.length msg) |> sexp_of_reply |> Sexplib.Sexp.to_string)
    in
    Lwt_io.write oc msg
  ;;

  let client_output msg oc =
    let%lwt () =
      Lwt_io.write_line
        oc
        (Output (String.length msg) |> sexp_of_reply |> Sexplib.Sexp.to_string)
    in
    Lwt_io.write oc msg
  ;;
end

module Server = struct
  (** Commands to send to the server *)
  type command =
    | Term (** Terminate the server *)
    | OpenBuffer of
        { session : int
        ; name : string
        } (** Report the buffer name *)
    | CloseBuffer of
        { session : int
        ; name : string
        }
    | Init of int (** Initialise a kak session *)
  [@@deriving sexp]
end
