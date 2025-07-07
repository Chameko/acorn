(** The kakorn server*)
type server =
  { paths : Resources.paths (** Contains the runtime dir and is used to construct paths *)
  ; sessions : Kak.session list ref (** The kak sessions the server is responsible for *)
  }

(** Start the kakorn server *)
val start_server : server -> unit Lwt.t
