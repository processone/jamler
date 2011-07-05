let section = Jamler_log.new_section "gen_mod"

type mod_info = (unit -> unit) * (unit -> unit)

module type Module =
sig
  val name : string

  val start : Jlib.namepreped -> mod_info list Lwt.t
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
    lwt mod_info = M.start host in
      (* TODO: store mod_info *)
      List.iter (fun (f, _) -> f ()) mod_info;
      Lwt_log.notice_f ~section
	"started module %s on %s" mod_name (host :> string)
  with
    | exn ->
	Lwt_log.error_f ~exn ~section
	  "Problem starting the module %s for host %s"
	  mod_name (host :> string)

let hook hook host f seq =
  ((fun () -> Jamler_hooks.add hook host f seq),
   (fun () -> Jamler_hooks.delete hook host f seq)
  )

let fold_hook hook host f seq =
  ((fun () -> Jamler_hooks.add_fold hook host f seq),
   (fun () -> Jamler_hooks.delete_fold hook host f seq)
  )

let iq_handler component host ns f type' =
  ((fun () -> Jamler_gen_iq_handler.add_iq_handler component host ns f type'),
   (fun () -> Jamler_gen_iq_handler.remove_iq_handler component host ns)
  )


