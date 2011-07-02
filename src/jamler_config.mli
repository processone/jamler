val myhosts : unit -> Jlib.namepreped list
val auth_modules : Jlib.namepreped -> string list
val modules : Jlib.namepreped -> (string * 'a list) list
val read_config : string -> unit Lwt.t
