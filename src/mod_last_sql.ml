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

  let start host =
    Lwt.return (
      [Gen_mod.iq_handler `Local host <:ns<LAST>> process_local_iq ();
      ]
    )

  let stop host =
    Lwt.return ()

end

let () = Gen_mod.register_mod (module ModLastSQL : Gen_mod.Module)

let rec get_data_rec luser lserver els' res =
    match els' with
      | [] ->
	  Lwt.return (List.rev res)
      | (`XmlElement (_name, attrs, _)) as el :: els -> (
	  let lxmlns = Xml.get_attr_s "xmlns" attrs in
	  let username = (luser : Jlib.nodepreped :> string) in
	  let query =
	    <:sql<
	      select @(data)s from private_storage
              where username=%(username)s and
              namespace=%(lxmlns)s
	    >>
	  in
	  lwt private_data = Sql.query lserver query in (
	      match private_data with
		| [data] -> (
		    try
		      let newel = Xml.parse_element data in
			get_data_rec luser lserver els (newel :: res)
		    with
		      | _ ->
			  get_data_rec luser lserver els res
		  )
		| _ ->
		    (* MREMOND: I wonder when the query could return a vcard ?1
		       {selected, ["vcard"], []} ->
		       EKHRAMTSOV: WTF??? *)
		    get_data_rec luser lserver els (el :: res)
	    ))
      | _ :: els ->
	  get_data_rec luser lserver els res

  let get_data luser lserver els =
    get_data_rec luser lserver els []
