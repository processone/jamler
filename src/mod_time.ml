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
	let time_f = Unix.time () in
	let tm_local = Unix.localtime time_f in
	let tm_utc = Unix.gmtime time_f in
	let utc = Printf.sprintf
	  "%04d-%02d-%02dT%02d:%02d:%02dZ"
	  (tm_utc.Unix.tm_year + 1900) (tm_utc.Unix.tm_mon + 1)
	  tm_utc.Unix.tm_mday tm_utc.Unix.tm_hour
	  tm_utc.Unix.tm_min tm_utc.Unix.tm_sec in
	let time_f_utc, _ = Unix.mktime tm_utc in
	let time_f_local, _ = Unix.mktime tm_local in
	let sec_diff = int_of_float (time_f_local -. time_f_utc) in
	let div = abs(sec_diff) / 3600 in
	let rem = abs(sec_diff) mod 3600 in
	let sign = if sec_diff >= 0 then "" else "-" in
	let tzo = sign ^ (Printf.sprintf "%02d:%02d" div rem) in
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
