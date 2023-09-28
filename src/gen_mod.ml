let src = Jamler_log.new_src "gen_mod"

type mod_info = (unit -> unit) * (unit -> unit)

module type Module =
sig
  val name : string

  val start : Jlib.namepreped -> mod_info list
  val stop : Jlib.namepreped -> unit
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
    let mod_info = M.start host in
    (* TODO: store mod_info *)
    List.iter (fun (f, _) -> f ()) mod_info;
    Logs.info ~src
      (fun m -> m "started module %s for \"%s\"" mod_name (host :> string));
    ()
  with
  | exn ->
     Logs.err ~src
       (fun m -> m "problem starting the module %s for host %s: %a"
	           mod_name (host :> string) Jamler_log.pp_exn exn);
     ()

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


