type json = Yojson.Safe.json
type 'a p = P of (json -> 'a)

exception Error of string

val int : int p
val bool : bool p
val string : string p
val namepreped : Jlib.namepreped p
val list : 'a p -> 'a list p
val enum : (string * 'a) list -> 'a p
val keys : string list p

type path = string list

val get_global_opt : path -> 'a p -> unit -> 'a option
val get_global_opt_with_default : path -> 'a p -> 'a -> unit -> 'a
val get_opt : path -> 'a p -> Jlib.namepreped -> 'a option
val get_opt_with_default : path -> 'a p -> 'a -> Jlib.namepreped -> 'a
val get_module_opt :
  string -> path -> 'a p -> Jlib.namepreped -> 'a option
val get_module_opt_with_default :
  string -> path -> 'a p -> 'a -> Jlib.namepreped -> 'a


val myhosts : unit -> Jlib.namepreped list
val is_my_host : Jlib.namepreped -> bool
val auth_modules : Jlib.namepreped -> string list
val modules : Jlib.namepreped -> string list
val read_config : string -> unit Lwt.t
