module GenIQHandler = Jamler_gen_iq_handler
module SM = Jamler_sm
module Hooks = Jamler_hooks
module Config = Jamler_config

module ModPrivateSQL :
sig
  include Gen_mod.Module
end
  =
struct
  let name = "mod_private_sql"
  let section = Jamler_log.new_section name
  let remove_user = Hooks.create ()

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
			get_data_rec luser lserver els
			  ((newel :> Xml.element_cdata) :: res)
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

  (* set_data(LUser, LServer, El) ->
    case El of
	{xmlelement, _Name, Attrs, _Els} ->
	    XMLNS = xml:get_attr_s("xmlns", Attrs),
	    case XMLNS of
		"" ->
		    ignore;
		_ ->
		    Username = ejabberd_odbc:escape(LUser),
		    LXMLNS = ejabberd_odbc:escape(XMLNS),
		    SData = ejabberd_odbc:escape(
			      xml:element_to_binary(El)),
			odbc_queries:set_private_data(LServer, Username, LXMLNS, SData)
	    end;
	_ ->
	    ignore
    end. *)

  let get_data luser lserver els =
    get_data_rec luser lserver els []

  let process_sm_iq from to' iq = 
    let luser = from.Jlib.luser in
    let lserver = from.Jlib.lserver in
      match List.mem lserver (Config.myhosts ()) with
	| true -> (
	    match iq.Jlib.iq_type with
	      | `Set (`XmlElement (name, attrs, els)) ->
		  (* F = fun() ->
				lists:foreach(
				  fun(El) ->
					  set_data(LUser, LServer, El)
				  end, Els)
			end,
		    odbc_queries:sql_transaction(LServer, F),
		  *)
		  Lwt.return
		    (`IQ {iq with
			    Jlib.iq_type =
			 `Result
			   (Some (`XmlElement (name, attrs, [])))})
	      | `Get subel ->
		  let `XmlElement (name, attrs, els) = subel in
		    try_lwt
		      lwt res_els = get_data luser lserver els in
			Lwt.return
			  (`IQ {iq with
				  Jlib.iq_type =
			       `Result
				 (Some (`XmlElement (name, attrs, res_els)))})
		    with
		      | _ ->
			  Lwt.return
			    (`IQ {iq with
				    Jlib.iq_type =
				 `Error (Jlib.err_internal_server_error,
					 Some subel)}))
	| false -> (
	    match iq.Jlib.iq_type with
	      | `Set subel
	      | `Get subel ->
		  Lwt.return (`IQ {iq with
				     Jlib.iq_type =
				  `Error (Jlib.err_not_allowed, Some subel)}))

  let remove_user_h (luser, lserver) =
    let username = (luser : Jlib.nodepreped :> string) in
    let delete_private_storage =
      <:sql<
	delete from private_storage
	where username=%(username)s
      >>
    in
    lwt _ = Sql.query lserver delete_private_storage in
      Lwt.return (Hooks.OK)

  let start host =
    Lwt.return (
      [Gen_mod.hook remove_user host remove_user_h 50;
       Gen_mod.iq_handler `SM host <:ns<PRIVATE>> process_sm_iq ();
      ]
    )

  let stop _host =
    Lwt.return ()

end

let () = Gen_mod.register_mod (module ModPrivateSQL : Gen_mod.Module)
