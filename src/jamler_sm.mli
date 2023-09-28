open Process

type broadcast =
    [ `RosterItem of Jlib.LJID.t * [ `None | `From | `To | `Both | `Remove ] ]
type msg +=
   | Broadcast of broadcast
   | Replaced
type info = [ `TODO ] list
type external_owner =
  | ExternalPid of Erlang.pid
  | ExternalNode of string * Erlang.erl_term
type owner =
  | Local of pid
  | External of external_owner
type sid = float * owner

val route : Jamler_router.t
val open_session :
  sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
  int -> info -> string list -> unit
val close_session :
  sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
  info -> string list -> unit
val close_session_unset_presence :
  sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
  string -> info -> string list -> unit
val set_presence :
  sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
  int -> Xml.element -> info -> string list -> unit
val unset_presence :
  sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
  string -> info -> string list -> unit

val get_user_resources :
  Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped list

val roster_in_subscription :
  (Jlib.nodepreped * Jlib.namepreped * Jlib.jid *
     [ `Subscribe | `Subscribed | `Unsubscribe | `Unsubscribed ] *
     string, bool)
  Jamler_hooks.fold_hook

val broadcast : Jlib.jid -> broadcast -> unit

val start : unit -> unit
