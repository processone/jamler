open Process

type t = Jlib.jid -> Jlib.jid -> Xml.element -> unit

type msg = [ `Route of Jlib.jid * Jlib.jid * Xml.element ]

val route : t

val register_route :
  ?local_hint : t option -> Jlib.namepreped -> msg pid -> unit
val unregister_route : Jlib.namepreped -> msg pid -> unit
