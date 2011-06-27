type rule = string				(* TODO *)
type acl = string				(* TODO *)

val match_rule : Jlib.namepreped -> rule -> Jlib.jid -> bool
val match_global_rule : rule -> Jlib.jid -> bool
val match_acl : acl -> Jlib.jid -> Jlib.namepreped -> bool
