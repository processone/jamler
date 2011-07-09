type items_t = | Items of (Xml.element list)
	       | IError of Xml.element
	       | IEmpty

type features_t = | Features of (string list)
		  | FError of Xml.element
		  | FEmpty

val register_feature : Jlib.namepreped -> string -> unit
val unregister_feature : Jlib.namepreped -> string -> unit
val register_extra_domain : Jlib.namepreped -> Jlib.namepreped -> unit
val unregister_extra_domain : Jlib.namepreped -> Jlib.namepreped -> unit

val disco_local_items :
  (Jlib.jid * Jlib.jid * string * string, items_t)
  Jamler_hooks.fold_hook
val disco_local_identity :
  (Jlib.jid * Jlib.jid * string * string, Xml.element list)
  Jamler_hooks.fold_hook
val disco_info :
  (Jlib.namepreped * string option * string * string, Xml.element list)
  Jamler_hooks.fold_hook
val disco_local_features :
  (Jlib.jid * Jlib.jid * string * string, features_t)
  Jamler_hooks.fold_hook
val disco_sm_items :
  (Jlib.jid * Jlib.jid * string * string, items_t)
  Jamler_hooks.fold_hook
val disco_sm_identity :
  (Jlib.jid * Jlib.jid * string * string, Xml.element list)
  Jamler_hooks.fold_hook
val disco_sm_features :
  (Jlib.jid * Jlib.jid * string * string, features_t)
  Jamler_hooks.fold_hook
