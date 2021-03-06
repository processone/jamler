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
    Jlib.nodepreped -> Jlib.namepreped -> string -> bool Lwt.t
  val check_password_digest :
    Jlib.nodepreped -> Jlib.namepreped ->
    string -> string -> (string -> string) -> bool Lwt.t
  val get_password :
    Jlib.nodepreped -> Jlib.namepreped -> string option Lwt.t
  val plain_password_required : bool

  val does_user_exist : Jlib.nodepreped -> Jlib.namepreped -> bool Lwt.t
  val remove : Jlib.nodepreped -> Jlib.namepreped -> unit Lwt.t
  val remove' : Jlib.nodepreped -> Jlib.namepreped -> string -> unit Lwt.t
  val try_register : Jlib.nodepreped -> Jlib.namepreped -> string -> result Lwt.t
  val set_password : Jlib.nodepreped -> Jlib.namepreped -> string -> result Lwt.t

end

val register_mod : (module Auth) -> unit

val check_password_with_authmodule :
  Jlib.nodepreped -> Jlib.namepreped -> string -> string option Lwt.t
val check_password_digest_with_authmodule :
  Jlib.nodepreped -> Jlib.namepreped ->
  string -> string -> (string -> string) -> string option Lwt.t
val get_password_with_authmodule :
  Jlib.nodepreped -> Jlib.namepreped -> (string * string) option Lwt.t
val plain_password_required : Jlib.namepreped -> bool

val does_user_exist : Jlib.nodepreped -> Jlib.namepreped -> bool Lwt.t
val remove : Jlib.nodepreped -> Jlib.namepreped -> unit Lwt.t
val remove' : Jlib.nodepreped -> Jlib.namepreped -> string -> unit Lwt.t
val entropy : string -> float
val try_register : Jlib.nodepreped -> Jlib.namepreped -> string -> result Lwt.t
val set_password : Jlib.nodepreped -> Jlib.namepreped -> string -> result Lwt.t

val remove_user : (Jlib.nodepreped * Jlib.namepreped) Jamler_hooks.hook
val register_user : (Jlib.nodepreped * Jlib.namepreped) Jamler_hooks.hook
