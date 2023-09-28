module type ListenModule =
sig
  val name : string
  val listen_parser :
    (Eio_unix.Net.stream_socket_ty Eio.Std.r ->
     Process.pid) Jamler_config.p
end

val register_mod : (module ListenModule) -> unit

val start_listeners : unit -> unit
val sockaddr_to_string: Unix.sockaddr -> string
