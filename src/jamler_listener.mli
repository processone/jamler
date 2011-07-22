module type ListenModule =
sig
  val name : string
  val listen_parser :
    (Lwt_unix.file_descr -> Process.empty Process.pid) Jamler_config.p
end

val register_mod : (module ListenModule) -> unit

val start_listeners : unit -> unit
val sockaddr_to_string: Unix.sockaddr -> string
