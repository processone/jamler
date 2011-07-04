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

  let show_os = Jamler_config.(get_module_opt_with_default
				 name ["show_os"] bool true)

  let process_local_iq _from to'
      ({Jlib.iq_id = _ID;
	Jlib.iq_type = iq_type;
	Jlib.iq_xmlns = _xmlns; _} as iq) =
    let iq_res =
      match iq_type with
	| `Set subel ->
	    {iq with
	       Jlib.iq_type = `Error (Jlib.err_not_allowed, Some subel)};
	| `Get _subel ->
	    let host = to'.Jlib.lserver in
	    let os =
	      if show_os host
	      then [get_os]
	      else []
	    in
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

  let start host =
    Lwt.return (
      [Gen_mod.iq_handler `Local host <:ns<VERSION>> process_local_iq ();
      ]
    )

  let stop host =
    Lwt.return ()

end

let () = Gen_mod.register_mod (module ModVersion : Gen_mod.Module)
