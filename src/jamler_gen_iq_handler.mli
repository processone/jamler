type component = [ `SM ]
type response = [ `IQ of Jlib.iq_response Jlib.iq | `Ignore ]
val add_iq_handler :
  component -> Jlib.namepreped -> string ->
  (Jlib.jid -> Jlib.jid -> Jlib.iq_query Jlib.iq -> response) -> unit -> unit
val remove_iq_handler : component -> Jlib.namepreped -> string -> unit
val handle :
  component -> Jlib.namepreped -> string ->
  Jlib.jid -> Jlib.jid -> Jlib.iq_query Jlib.iq -> bool