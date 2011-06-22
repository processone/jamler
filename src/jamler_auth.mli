val check_password_with_authmodule :
  Jlib.nodepreped -> Jlib.namepreped -> string -> string option
val check_password_digest_with_authmodule :
  Jlib.nodepreped -> Jlib.namepreped ->
  string -> string -> (string -> string) -> string option
val get_password_with_authmodule :
  Jlib.nodepreped -> Jlib.namepreped -> (string * string) option

val does_user_exist : Jlib.nodepreped -> Jlib.namepreped -> bool
