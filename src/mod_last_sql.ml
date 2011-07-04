module GenIQHandler = Jamler_gen_iq_handler

module ModLastSQL :
sig
  include Gen_mod.Module
end
  =
struct
  let name = "mod_last_sql"
  let section = Jamler_log.new_section name

  let process_local_iq _from to' = function
    | {Jlib.iq_type = `Get _subel; iq_xmlns = <:ns<LAST>>; _} as iq ->
	let secs = (int_of_float (Jlib.uptime ())) in
	  Lwt.return
	    (`IQ {iq with
		    Jlib.iq_type =
		 `Result
		   (Some (`XmlElement
			    ("query",
			     [("xmlns", <:ns<LAST>>);
			      ("seconds", string_of_int secs)],
			     [])))})
    | {Jlib.iq_type = `Get subel; _} as iq ->
        Lwt.return (`IQ {iq with
                           Jlib.iq_type =
                        `Error (Jlib.err_service_unavailable, Some subel)})
    | {Jlib.iq_type = `Set subel; _} as iq ->
        Lwt.return (`IQ {iq with
                           Jlib.iq_type =
                        `Error (Jlib.err_not_allowed, Some subel)})

  let start host _opts =
    GenIQHandler.add_iq_handler `Local host <:ns<LAST>> process_local_iq ();
    lwt () = Lwt_log.notice ~section "started" in
      Lwt.return ()

  let stop host =
    GenIQHandler.remove_iq_handler `Local host <:ns<LAST>>;
    lwt() = Lwt_log.notice ~section "stopped" in
      Lwt.return ()

end

let () = Gen_mod.register_mod (module ModLastSQL : Gen_mod.Module)
