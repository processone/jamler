let section = Jamler_log.new_section "gen_mod"

module type Module =
sig
  val name : string

  val start : Jlib.namepreped -> unit Lwt.t
  val stop : Jlib.namepreped -> unit Lwt.t
end

let mods : (string, (module Module)) Hashtbl.t =
  Hashtbl.create 10

let register_mod mod' =
  let module M = (val mod' : Module) in
    Hashtbl.replace mods M.name mod'

let start_module host mod_name =
  (* TODO *)
  try
    let m = Hashtbl.find mods mod_name in
    let module M = (val m : Module) in
      M.start host
  with
    | exn ->
	Lwt_log.error_f ~exn ~section
	  "Problem starting the module %s for host %s"
	  mod_name (host :> string)




