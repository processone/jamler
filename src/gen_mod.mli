type mod_info

module type Module =
sig
  val name : string
  val start : Jlib.namepreped -> mod_info list
  val stop : Jlib.namepreped -> unit
end

val register_mod : (module Module) -> unit
val start_module : Jlib.namepreped -> string -> unit

val hook :
  'a Jamler_hooks.hook -> Jlib.namepreped ->
  ('a -> Jamler_hooks.result) -> int -> mod_info
val fold_hook :
  ('a, 'b) Jamler_hooks.fold_hook -> Jlib.namepreped ->
  ('b -> 'a -> Jamler_hooks.result * 'b) -> int -> mod_info
val iq_handler :
  Jamler_gen_iq_handler.component -> Jlib.namepreped -> string ->
  (Jlib.jid -> Jlib.jid -> Jlib.iq_query Jlib.iq ->
     Jamler_gen_iq_handler.response) ->
  unit -> mod_info

