(*
type s2s_out_msg = [ `Send_element of Xml.element ]

module S2S :
sig
  type s2s = string * msg Process.pid
  type fromto = Jlib.namepreped * Jlib.namepreped
  type s2s_table = (fromto, s2s list) Hashtbl.t

  val route : Jlib.jid -> Jlib.jid -> Xml.element -> unit
  val have_connection : fromto -> bool
  val has_key : fromto -> string -> bool
  val get_connections_pids : fromto -> msg Process.pid list
  val try_register : fromto -> s2s_out_msg Process.pid -> string option
  val remove_connection : fromto -> s2s_out_msg Process.pid -> string -> unit
  val find_connection : Jlib.jid -> Jlib.jid -> msg Process.pid option
  val dirty_get_connections: unit -> fromto list
  val allow_host : Jlib.namepreped -> Jlib.namepreped -> bool
  val is_service : Jlib.jid -> Jlib.jid -> bool

  val s2s_send_packet : (Jlib.jid * Jlib.jid * Xml.element) Jamler_hooks.hook

end

val allow_host : Jlib.namepreped -> Jlib.namepreped -> bool
val has_key : (Jlib.namepreped * Jlib.namepreped) -> string -> bool
*)
