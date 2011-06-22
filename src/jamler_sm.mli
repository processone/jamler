open Process

type msg = Jamler_router.msg
type info = [ `TODO ] list
type sid = float * msg pid

val route : Jamler_router.t
val open_session :
  sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
  int -> info -> unit
val close_session :
  sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped -> unit
val close_session_unset_presence :
  sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
  string -> unit
val set_presence :
  sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
  int -> Xml.element -> info -> unit
val unset_presence :
  sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
  string -> info -> unit

val get_user_resources :
  Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped list

val roster_in_subscription :
  (Jlib.nodepreped * Jlib.namepreped * Jlib.jid *
     [ `Subscribe | `Subscribed | `Unsubscribe | `Unsubscribed ] *
     string, bool)
  Jamler_hooks.fold_hook
