module type ListenModule =
sig
  val name : string
  val listen_parser : (Lwt_unix.file_descr -> unit) Jamler_config.p
end

val register_mod : (module ListenModule) -> unit

val start_listeners : unit -> unit
