type 'a access_rule

val get_rule : string -> 'a Jamler_config.p -> 'a access_rule
val match_rule : Jlib.namepreped -> 'a access_rule -> Jlib.jid -> 'a -> 'a
val match_global_rule : 'a access_rule -> Jlib.jid -> 'a -> 'a

val access : bool Jamler_config.p
val all : bool access_rule
val none : bool access_rule
val none_string : string access_rule

