type 'a p

val int : int p
val bool : bool p
val string : string p
val namepreped : Jlib.namepreped p
val list : 'a p -> 'a list p

type path = string list

val get_global_opt : path -> 'a p -> unit -> 'a option
val get_global_opt_with_default : path -> 'a p -> 'a -> unit -> 'a
val get_opt : Jlib.namepreped -> path -> 'a p -> unit -> 'a option
val get_opt_with_default : Jlib.namepreped -> path -> 'a p -> 'a -> unit -> 'a


val myhosts : unit -> Jlib.namepreped list
val auth_modules : Jlib.namepreped -> string list
val modules : Jlib.namepreped -> (string * 'a list) list
val read_config : string -> unit Lwt.t
