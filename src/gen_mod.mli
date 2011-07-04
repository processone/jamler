type mod_info

module type Module =
sig
  val name : string
  val start : Jlib.namepreped -> mod_info list Lwt.t
  val stop : Jlib.namepreped -> unit Lwt.t
end

val register_mod : (module Module) -> unit
val start_module : Jlib.namepreped -> string -> unit Lwt.t

val hook :
  'a Jamler_hooks.hook -> Jlib.namepreped ->
  ('a -> Jamler_hooks.result Lwt.t) -> int -> mod_info
val fold_hook :
  ('a, 'b) Jamler_hooks.fold_hook -> Jlib.namepreped ->
  ('b -> 'a -> (Jamler_hooks.result * 'b) Lwt.t) -> int -> mod_info
val iq_handler :
  Jamler_gen_iq_handler.component -> Jlib.namepreped -> string ->
  (Jlib.jid -> Jlib.jid -> Jlib.iq_query Jlib.iq ->
     Jamler_gen_iq_handler.response Lwt.t) ->
  unit -> mod_info

