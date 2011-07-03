module GenIQHandler = Jamler_gen_iq_handler

module ModVersion :
sig
  include Gen_mod.Module
end
  =
struct
  let name = "mod_version"

  let get_os =
    let os_str = Printf.sprintf
      "%s/%s/%s (OCaml %s)" Cfg.os Cfg.system Cfg.arch Cfg.ocaml in
      `XmlElement ("os", [], [`XmlCdata os_str])
      
  let process_local_iq _from _to'
      ({Jlib.iq_id = _ID;
	Jlib.iq_type = iq_type;
	Jlib.iq_xmlns = _xmlns; _} as iq) =
    let iq_res =
      match iq_type with
	| `Set subel ->
	    {iq with
	       Jlib.iq_type = `Error (Jlib.err_not_allowed, Some subel)};
	| `Get _subel ->
	    (*let host = to'.Jlib.server in
	      OS = case gen_mod:get_module_opt(Host, ?MODULE, show_os, true) of
	      true -> [get_os()];
	      false -> []
	      end,*)
	    let os = [get_os] in
	      {iq with
		 Jlib.iq_type =
		  `Result
		    (Some (`XmlElement
			     ("query",
			      [("xmlns", <:ns<VERSION>>)],
			      [`XmlElement ("name", [],
					    [`XmlCdata Cfg.name]);
			       `XmlElement ("version", [],
					    [`XmlCdata Cfg.version])
			      ] @ os
			     )))}
    in
      Lwt.return (`IQ iq_res)

  let start host _opts =
    GenIQHandler.add_iq_handler `Local host <:ns<VERSION>> process_local_iq ();
    Lwt.return ()

  let stop host =
    GenIQHandler.remove_iq_handler `Local host <:ns<VERSION>>;
    Lwt.return ()

end

let () = Gen_mod.register_mod (module ModVersion : Gen_mod.Module)
