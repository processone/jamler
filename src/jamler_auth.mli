type result = | OK
	      | Empty_password
	      | Not_allowed
	      | Invalid_jid
	      | Exists
	      | Server_error

module type Auth =
sig
  val name : string

  val check_password :
    Jlib.nodepreped -> Jlib.namepreped -> string -> bool
  val check_password_digest :
    Jlib.nodepreped -> Jlib.namepreped ->
    string -> string -> (string -> string) -> bool
  val get_password :
    Jlib.nodepreped -> Jlib.namepreped -> string option
  val plain_password_required : bool

  val does_user_exist : Jlib.nodepreped -> Jlib.namepreped -> bool
  val remove : Jlib.nodepreped -> Jlib.namepreped -> unit
  val remove' : Jlib.nodepreped -> Jlib.namepreped -> string -> unit
  val try_register : Jlib.nodepreped -> Jlib.namepreped -> string -> result
  val set_password : Jlib.nodepreped -> Jlib.namepreped -> string -> result

end

val register_mod : (module Auth) -> unit

val check_password_with_authmodule :
  Jlib.nodepreped -> Jlib.namepreped -> string -> string option
val check_password_digest_with_authmodule :
  Jlib.nodepreped -> Jlib.namepreped ->
  string -> string -> (string -> string) -> string option
val get_password_with_authmodule :
  Jlib.nodepreped -> Jlib.namepreped -> (string * string) option
val plain_password_required : Jlib.namepreped -> bool

val does_user_exist : Jlib.nodepreped -> Jlib.namepreped -> bool
val remove : Jlib.nodepreped -> Jlib.namepreped -> unit
val remove' : Jlib.nodepreped -> Jlib.namepreped -> string -> unit
val entropy : string -> float
val try_register : Jlib.nodepreped -> Jlib.namepreped -> string -> result
val set_password : Jlib.nodepreped -> Jlib.namepreped -> string -> result

val remove_user : (Jlib.nodepreped * Jlib.namepreped) Jamler_hooks.hook
val register_user : (Jlib.nodepreped * Jlib.namepreped) Jamler_hooks.hook
