module type Module =
sig
  val name : string
  val start : Jlib.namepreped -> 'a list -> unit Lwt.t
  val stop : Jlib.namepreped -> unit Lwt.t
end

val register_mod : (module Module) -> unit
val start_module : Jlib.namepreped -> string -> 'a list -> unit Lwt.t
