open Process

type broadcast =
    [ `RosterItem of Jlib.LJID.t * [ `None | `From | `To | `Both | `Remove ] ]
type msg =
    [ Jamler_router.msg
    | `Broadcast of broadcast
    | `Replaced
    | `Node_up of string
    | `Node_down of string ]
type info = [ `TODO ] list
type owner =
  | Local of msg pid
  | External of Erlang.pid
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

val cluster_store :
  (string list -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
     float -> int -> owner -> unit) ref
val cluster_remove :
  (string list -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
     float -> owner -> unit) ref

val node_up_hook : string Jamler_hooks.plain_hook
val node_down_hook : string Jamler_hooks.plain_hook
