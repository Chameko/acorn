(** The kakorn client *)

(** Start the client *)
val start_client : ?req:string -> Resources.paths -> unit Lwt.t
