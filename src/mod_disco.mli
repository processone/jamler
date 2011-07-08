
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
