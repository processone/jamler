module GenIQHandler = Jamler_gen_iq_handler

module ModTime :
sig
  include Gen_mod.Module
end
  =
struct
  let name = "mod_time"

  let process_local_iq _from _to = function
    | {Jlib.iq_type = `Set subel; _} as iq ->
	Lwt.return (`IQ {iq with
			   Jlib.iq_type =
			`Error (Jlib.err_not_allowed, Some subel)})
    | {Jlib.iq_type = `Get _subel; iq_xmlns = <:ns<TIME90>>; _} as iq ->
	let utc = Jlib.timestamp_to_iso (Unix.gmtime (Unix.time ())) in
	  Lwt.return (
	    `IQ {iq with
		   Jlib.iq_type =
		`Result
		  (Some (`XmlElement
			   ("query",
			    [("xmlns", <:ns<TIME90>>)],
			    [`XmlElement ("utc", [],
					  [`XmlCdata utc])])))})
    | {Jlib.iq_type = `Get _subel; iq_xmlns = <:ns<TIME>>; _} as iq ->
	let utc, tzo = Jlib.timestamp_to_iso' (Unix.time ()) in
	  Lwt.return (
	    `IQ {iq with
		   Jlib.iq_type =
		`Result
		  (Some (`XmlElement
			   ("query",
			    [("xmlns", <:ns<TIME>>)],
			    [`XmlElement ("time", [],
					  [`XmlElement ("tzo", [],
							[`XmlCdata tzo]);
					   `XmlElement ("utc", [],
							[`XmlCdata utc])])])))})
    | {Jlib.iq_type = `Get subel; _} as iq ->
	Lwt.return (`IQ {iq with
			   Jlib.iq_type =
			`Error (Jlib.err_service_unavailable, Some subel)})

  let start host _opts =
    GenIQHandler.add_iq_handler `Local host <:ns<TIME>> process_local_iq ();
    GenIQHandler.add_iq_handler `Local host <:ns<TIME90>> process_local_iq ();
    Lwt.return ()

  let stop host =
    GenIQHandler.remove_iq_handler `Local host <:ns<TIME90>>;
    GenIQHandler.remove_iq_handler `Local host <:ns<TIME>>;
    Lwt.return ()

end

let () = Gen_mod.register_mod (module ModTime : Gen_mod.Module)
