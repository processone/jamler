module GenIQHandler = Jamler_gen_iq_handler

type li_type =
  | No
  | JID of Jlib.LJID.t
  | Group of string
  | Subscription of [ `None | `From | `To | `Both ]

type p_type =
  | Message
  | IQ
  | Presence_in
  | Presence_out
  | Other

type action = bool

type listitem =
    {li_type : li_type;
     action : action;
     order : int;
     match_all : bool;
     match_iq : bool;
     match_message : bool;
     match_presence_in : bool;
     match_presence_out : bool;
    }

type userlist =
    {name : string option;
     list : listitem list;
    }

let new_userlist () =
  {name = None; list = []}

let privacy_check_packet :
    (Jlib.nodepreped * Jlib.namepreped * userlist *
       (Jlib.jid * Jlib.jid * Xml.element) *
       [ `In | `Out ], action)
    Jamler_hooks.fold_hook
    = Jamler_hooks.create_fold ()

let privacy_iq_get :
    (Jlib.jid * Jlib.jid * [ `Get of Xml.element ] Jlib.iq * userlist,
     [ `Error of Xml.element
     | `Result of Xml.element option ])
    Jamler_hooks.fold_hook
    = Jamler_hooks.create_fold ()

let privacy_iq_set :
    (Jlib.jid * Jlib.jid * [ `Set of Xml.element ] Jlib.iq,
     [ `Error of Xml.element
     | `Result of Xml.element option
     | `ResultList of Xml.element option * userlist ])
    Jamler_hooks.fold_hook
    = Jamler_hooks.create_fold ()

module ModPrivacySql :
sig
  include Gen_mod.Module
end
  =
struct
  let name = "mod_privacy_sql"

  let sql_get_default_privacy_list luser lserver =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      <:sql<
	select @(name)s from privacy_default_list
	where username=%(username)s
      >>
    in
      Sql.query lserver query

  let sql_get_default_privacy_list_t luser =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      <:sql<
	select @(name)s from privacy_default_list
	where username=%(username)s
      >>
    in
      Sql.query_t query

  let sql_get_privacy_list_names luser lserver =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      <:sql<
	select @(name)s from privacy_list
	where username=%(username)s
      >>
    in
      Sql.query lserver query

  let sql_get_privacy_list_names_t luser =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      <:sql<
	select @(name)s from privacy_list
	where username=%(username)s
      >>
    in
      Sql.query_t query

  let sql_get_privacy_list_id luser lserver name =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      <:sql<
	select @(id)d from privacy_list
	where username=%(username)s and name=%(name)s
      >>
    in
      Sql.query lserver query

  let sql_get_privacy_list_id_t luser name =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      <:sql<
	select @(id)d from privacy_list
	where username=%(username)s and name=%(name)s
      >>
    in
      Sql.query_t query

(*
sql_get_privacy_list_data(LUser, LServer, Name) ->
    Username = ejabberd_odbc:escape(LUser),
    SName = ejabberd_odbc:escape(Name),
    odbc_queries:get_privacy_list_data(LServer, Username, SName).
*)
  let sql_get_privacy_list_data_by_id id lserver =
    let query =
      <:sql<
	select @(t)s, @(value)s, @(action)s, @(ord)d,
               @(match_all)b, @(match_iq)b,
               @(match_message)b, @(match_presence_in)b, @(match_presence_out)b
        from privacy_list_data 
        where id=%(id)d order by ord
      >>
    in
      Sql.query lserver query

  let sql_set_default_privacy_list luser name =
    let username = (luser : Jlib.nodepreped :> string) in
    let insert_query =
      <:sql<
	insert into privacy_default_list(username, name)
	values (%(username)s, %(name)s)
      >>
    in
    let update_query =
      <:sql<
	update privacy_default_list
	set name = %(name)s
        where username = %(username)s
      >>
    in
      Sql.update_t insert_query update_query

  let sql_unset_default_privacy_list luser lserver =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      <:sql<
	delete from privacy_default_list
        where username=%(username)s
      >>
    in
      Sql.query lserver query

  let sql_remove_privacy_list luser name =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      <:sql<
	delete from privacy_list
	where username=%(username)s and name=%(name)s
      >>
    in
      Sql.query_t query

  let sql_add_privacy_list luser name =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      <:sql<
	insert into privacy_list(username, name)
	values (%(username)s, %(name)s)
      >>
    in
      Sql.query_t query

  let sql_set_privacy_list id ritems =
    let query =
      <:sql<
	delete from privacy_list_data
	where id=%(id)d
      >>
    in
    lwt _ = Sql.query_t query in
      Lwt_list.iter_s
	(fun (stype, svalue, saction, order, match_all, match_iq,
	      match_message, match_presence_in, match_presence_out) ->
	   let query =
	     <:sql<
	       insert into privacy_list_data(
		 id, t, value, action, ord, match_all, match_iq,
		 match_message, match_presence_in,
		 match_presence_out
	       )
	       values (
		 %(id)d, %(stype)s, %(svalue)s, %(saction)s,
		 %(order)d, %(match_all)b, %(match_iq)b,
		 %(match_message)b, %(match_presence_in)b,
		 %(match_presence_out)b
	       )
	     >>
	   in
	   lwt _ = Sql.query_t query in
	     Lwt.return ()
	) ritems

(*
sql_del_privacy_lists(LUser, LServer) ->
    Username = ejabberd_odbc:escape(LUser),
    Server = ejabberd_odbc:escape(LServer),
    odbc_queries:del_privacy_lists(LServer, Server, Username).
*)

  let raw_to_item (stype, svalue, saction, order, match_all, match_iq,
		   match_message, match_presence_in, match_presence_out) =
    let li_type =
      match stype with
	| "n" -> No
	| "j" ->
	    let jid = Jlib.string_to_jid_exn svalue in
	      JID (Jlib.jid_tolower jid)
	| "g" ->
	    Group svalue
	| "s" -> (
	    match svalue with
	      | "none" -> Subscription `None
	      | "both" -> Subscription `Both
	      | "from" -> Subscription `From
	      | "to" ->   Subscription `To
	      | _ -> assert false
	  )
	| _ -> assert false
    in
    let action =
      match saction with
	| "a" -> true
	| "d" -> false
	| _ -> assert false
    in
      {li_type;
       action;
       order;
       match_all;
       match_iq;
       match_message;
       match_presence_in;
       match_presence_out;
      }

  let item_to_raw {li_type;
		   action;
		   order;
		   match_all;
		   match_iq;
		   match_message;
		   match_presence_in;
		   match_presence_out;
		  } =
    let stype, svalue =
      match li_type with
	| No -> "n", ""
	| JID jid ->
	    "j", Jlib.LJID.to_string jid
	| Group g ->
	    "g", g
	| Subscription s -> (
	    match s with
	      | `None -> "s", "none"
	      | `Both -> "s", "both"
	      | `From -> "s", "from"
	      | `To ->   "s", "to"
	  )
    in
    let saction =
	match action with
	  | true -> "a"
	  | false -> "d"
    in
      (stype, svalue, saction, order, match_all, match_iq,
       match_message, match_presence_in, match_presence_out)

  let action_to_list action =
    match action with
      | true -> "allow"
      | false -> "deny"

  let order_to_list order =
    string_of_int order

  let type_to_list li_type =
    match li_type with
      | No -> assert false
      | JID _ -> "jid"
      | Group _ -> "group"
      | Subscription _ -> "subscription"

  let value_to_list li_type =
    match li_type with
      | No -> assert false
      | JID jid -> Jlib.LJID.to_string jid
      | Group g -> g
      | Subscription s -> (
	  match s with
	    | `Both -> "both"
	    | `To -> "to"
	    | `From -> "from"
	    | `None -> "none"
	)



  let list_to_action s =
    match s with
      | "allow" -> Some true
      | "deny" -> Some false
      | _ -> None



  let item_to_xml item =
    let attrs = [("action", action_to_list item.action);
		 ("order", order_to_list item.order)]
    in
    let attrs =
      match item.li_type with
	| No -> attrs
	| li_type ->
	    ("type", type_to_list li_type) ::
	      ("value", value_to_list li_type) ::
	      attrs
    in
    let subels =
      match item.match_all with
	| true -> []
	| false ->
	    let se =
	      match item.match_iq with
		| true -> [`XmlElement ("iq", [], [])]
		| false -> []
	    in
	    let se =
	      match item.match_message with
		| true -> `XmlElement ("message", [], []) :: se
		| false -> []
	    in
	    let se =
	      match item.match_presence_in with
		| true -> `XmlElement ("presence-in", [], []) :: se
		| false -> []
	    in
	    let se =
	      match item.match_presence_out with
		| true -> `XmlElement ("presence-out", [], []) :: se
		| false -> []
	    in
	      se
    in
      `XmlElement ("item", attrs, subels)



  let is_ptype_match item ptype =
    if item.match_all
    then true
    else
      match ptype with
	| Message -> item.match_message
	| IQ -> item.match_iq
	| Presence_in -> item.match_presence_in
	| Presence_out -> item.match_presence_out
	| Other -> false

  let is_type_match li_type jid sg =
    match li_type with
      | No -> Lwt.return true
      | JID vjid -> (
	  let (luser, lserver, lresource) = vjid in
	    match (luser :> string), (lresource :> string) with
	      | "", "" -> (
		  match jid with
		    | (_, lserver', _) when lserver = lserver' ->
			Lwt.return true
		    | _ ->
			Lwt.return false
		)
	      | _, "" -> (
		  match jid with
		    | (luser', lserver', _) when
			luser = luser' &&
			lserver = lserver' ->
			Lwt.return true
		    | _ ->
			Lwt.return false
		)
	      | _, _ ->
		  Lwt.return (vjid = jid)
	)
      | Subscription value ->
	  lwt (subscription, _groups) = Lazy.force sg in
	    Lwt.return (value = subscription)
      | Group value ->
	  lwt (_subscription, groups) = Lazy.force sg in
	    Lwt.return (List.mem value groups)


  let rec check_packet_aux list ptype jid sg =
    match list with
      | [] -> Lwt.return true
      | item :: list -> (
	  let {li_type = li_type; action; _} = item in
	    if is_ptype_match item ptype then (
	      lwt itm = is_type_match li_type jid sg in
		if itm
		then Lwt.return action
		else check_packet_aux list ptype jid sg
	    ) else check_packet_aux list ptype jid sg
	)


(* From is the sender, To is the destination.
   If Dir = out, User@Server is the sender account (From).
   If Dir = in, User@Server is the destination account (To). *)
  let check_packet _ (user, server, {list; _},
		      (from, to', `XmlElement (pname, attrs, _)), dir) =
    match list with
      | [] ->
	  Lwt.return (Jamler_hooks.OK, true)
      | _ ->
	  let ptype =
	    match pname with
	      | "message" -> `Message;
	      | "iq" -> `IQ;
	      | "presence" -> (
		  match Xml.get_attr_s "type" attrs with
		      (* notification *)
		    | ""
		    | "unavailable" -> `Presence;
			(* subscribe, subscribed, unsubscribe,
			   unsubscribed, error, probe, or other *)
		    | _ -> `Other
		)
	      | _ -> assert false
	  in
	  let ptype =
	    match ptype, dir with
	      | `Message, `In -> Message;
	      | `Iq, `In -> IQ;
	      | `Presence, `In -> Presence_in;
	      | `Presence, `Out -> Presence_out;
	      | _, _ -> Other
	  in
	  let jid =
	    match dir with
	      | `In -> from
	      | `Out -> to'
	  in
	  let ljid = Jlib.jid_tolower jid in
	  let sg =
	    lazy (Jamler_hooks.run_fold
		    Gen_roster.roster_get_jid_info
		    server
		    (`None, [])
		    (user, server, jid)
		 )
	  in
	  lwt res = check_packet_aux list ptype ljid sg in
	    Lwt.return (Jamler_hooks.OK, res)


  let process_local_iq _from _to = function
    | {Jlib.iq_type = `Set subel; _} as iq ->
	Lwt.return (`IQ {iq with
			   Jlib.iq_type =
			`Error (Jlib.err_not_allowed, Some subel)})
    | {Jlib.iq_type = `Get _subel; iq_xmlns = <:ns<TIME90>>; _} as iq ->
	let utc = Jlib.timestamp_to_iso' (Unix.gmtime (Unix.time ())) in
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
	let utc, tzo = Jlib.timestamp_to_iso (Unix.time ()) (Jlib.get_tzo ()) in
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

  let process_iq _from _to iq =
    match iq.Jlib.iq_type with
      | `Set subel
      | `Get subel ->
	  Lwt.return
	    (`IQ {iq with Jlib.iq_type = `Error (Jlib.err_not_allowed,
						 Some subel)})


  let process_lists_get luser lserver active =
    lwt default =
      match_lwt sql_get_default_privacy_list luser lserver with
	| [] -> Lwt.return None
	| [default] -> Lwt.return (Some default)
	| _ -> Lwt.return None
    in
      match_lwt sql_get_privacy_list_names luser lserver with
	| [] ->
	    Lwt.return (
	      `Result
		(Some
		   (`XmlElement ("query", [("xmlns", <:ns<PRIVACY>>)], []))))
	| names ->
	    let items =
	      List.map
		(fun name -> `XmlElement ("list", [("name", name)], []))
		names
	    in
	    let items =
	      match default with
		| None -> items
		| Some default ->
		    `XmlElement ("default", [("name", default)], []) :: items
	    in
	    let items =
		match active with
		  | None -> items
		  | Some active ->
		      `XmlElement ("active", [("name", active)], []) :: items
	    in
	      Lwt.return (
		`Result (Some
			   (`XmlElement ("query", [("xmlns", <:ns<PRIVACY>>)],
					 items))))


  let process_list_get luser lserver =
    function
      | Some name -> (
	  match_lwt sql_get_privacy_list_id luser lserver name with
	    | [] ->
		Lwt.return (`Error Jlib.err_item_not_found)
	    | [id] -> (
		lwt ritems = sql_get_privacy_list_data_by_id id lserver in
		let items = List.map raw_to_item ritems in
		let litems = List.map item_to_xml items in
		  Lwt.return (
		    `Result
		      (Some
			 (`XmlElement
			    ("query", [("xmlns", <:ns<PRIVACY>>)],
			     [`XmlElement ("list",
					   [("name", name)], litems)]))))
	      )
	    | _ ->
		Lwt.return (`Error Jlib.err_internal_server_error)
	)
      | None ->
	  Lwt.return (`Error Jlib.err_bad_request)

  let process_iq_get _ (from, _to, {Jlib.iq_type = `Get subel},
			{name = active; _}) =
    let {Jlib.luser = luser; Jlib.lserver = lserver; _} = from in
    let `XmlElement (_, _, els) = subel in
    lwt res =
      match Xml.remove_cdata els with
	| [] ->
	    process_lists_get luser lserver active
	| [`XmlElement (name, attrs, _subels)] -> (
	    match name with
	      | "list" ->
		  let list_name = Xml.get_attr "name" attrs in
		    process_list_get luser lserver list_name
	      | _ ->
		  Lwt.return (`Error Jlib.err_bad_request)
	  )
	| _ ->
	    Lwt.return (`Error Jlib.err_bad_request)
    in
      Lwt.return (Jamler_hooks.OK, res)

  type parse_res =
    | PBad
    | PRemove
    | PList of listitem list


  let rec parse_matches1 item =
    function
      | [] ->
	  Some item
      | `XmlElement ("message", _, _) :: els ->
	  parse_matches1 {item with match_message = true} els
      | `XmlElement ("iq", _, _) :: els ->
	  parse_matches1 {item with match_iq = true} els
      | `XmlElement ("presence-in", _, _) :: els ->
	  parse_matches1 {item with match_presence_in = true} els
      | `XmlElement ("presence-out", _, _) :: els ->
	  parse_matches1 {item with match_presence_out = true} els
      | `XmlElement _ :: _els ->
	  None

  let parse_matches item =
    function
      | [] ->
	  Some {item with match_all = true}
      | els ->
	  parse_matches1 item els


  let rec parse_items_aux els res =
    match els with
      | [] ->
	  (* Sort the items by their 'order' attribute *)
	  PList (List.sort (fun x y -> compare x.order y.order) res)
      | `XmlElement ("item", attrs, subels) :: els -> (
	  let type' = Xml.get_attr "type" attrs in
	  let value = Xml.get_attr "value" attrs in
	  let saction = Xml.get_attr "action" attrs in
	  let sorder = Xml.get_attr_s "order" attrs in
	  let action =
	    match saction with
	      | None -> None
	      | Some a -> list_to_action a
	  in
	  let order =
	    try
	      let v = int_of_string sorder in
		if v >= 0
		then Some v
		else None
	    with
	      | _ -> None
	  in
	    match action, order with
	      | Some action, Some order -> (
		  let i = {action;
			   order;
			   li_type = No;
			   match_all = false;
			   match_iq = false;
			   match_message = false;
			   match_presence_in = false;
			   match_presence_out = false;
			  }
		  in
		  let i =
		    match type', value with
		      | Some t, Some v -> (
			  match t with
			    | "jid" -> (
				match Jlib.string_to_jid v with
				  | None ->
				      None
				  | Some jid ->
				      Some {i with
					      li_type = JID (
						Jlib.jid_tolower jid)}
			      )
			    | "group" ->
				Some {i with li_type = Group v}
			    | "subscription" -> (
				 match v with
				   | "none" ->
				       Some {i with
					       li_type = Subscription `None}
				   | "both" ->
				       Some {i with
					       li_type = Subscription `Both}
				   | "from" ->
				       Some {i with
					       li_type = Subscription `From}
				   | "to" ->
				       Some {i with
					       li_type = Subscription `To}
				   | _ ->
				       None
			      )
			    | _ -> None
			)
		      | Some _, None ->
			  None
		      | _ ->
			  Some i
		  in
		    match i with
		      | None ->
			  PBad
		      | Some i -> (
			  match parse_matches i (Xml.remove_cdata subels) with
			    | None ->
				PBad
			    | Some i ->
				parse_items_aux els (i :: res)
			)
		)
	      | _ -> PBad
	)
      | _ ->
	  PBad

let parse_items =
  function
    | [] ->
	PRemove
    | els ->
	parse_items_aux els []


  let process_list_set luser lserver list_name els =
    match list_name with
      | Some name -> (
	  match parse_items els with
	    | PBad ->
		Lwt.return (`Error Jlib.err_bad_request)
	    | PRemove -> (
		let f () =
		  match_lwt sql_get_default_privacy_list_t luser with
		    | [] ->
			lwt _ = sql_remove_privacy_list luser name in
			  Lwt.return (`Result None)
		    | [default] ->
			(* TODO: check active *)
			if name = default
			then Lwt.return (`Error Jlib.err_conflict)
			else (
			  lwt _ = sql_remove_privacy_list luser name in
			    Lwt.return (`Result None)
			)
		    | _ -> assert false
		in
		  match_lwt Sql.transaction lserver f with
		    | `Error _ as error ->
			Lwt.return error
		    | `Result _ as res ->
			(* TODO *)
		    (*ejabberd_router:route(
		      jlib:make_jid(LUser, LServer, ""),
		      jlib:make_jid(LUser, LServer, ""),
		      {xmlelement, "broadcast", [],
		       [{privacy_list,
			 #userlist{name = Name, list = []},
			 Name}]}),
		    *)
			Lwt.return res;
	      )
	    | PList list -> (
		let ritems = List.map item_to_raw list in
		let f () =
		  lwt id =
		    match_lwt sql_get_privacy_list_id_t luser name with
		      | [] -> (
			  lwt _ = sql_add_privacy_list luser name in
			    match_lwt sql_get_privacy_list_id_t luser name with
			      | [i] -> Lwt.return i
			      | _ -> assert false
			)
		      | [i] -> Lwt.return i
		      | _ -> assert false
		  in
		  lwt _ = sql_set_privacy_list id ritems in
		    Lwt.return (`Result None)
		in
		  match_lwt Sql.transaction lserver f with
		    | `Error _ as error ->
			Lwt.return error
		    | `Result _ as res ->
			(* TODO *)
		    (*NeedDb = is_list_needdb(List),
		    ejabberd_router:route(
		      jlib:make_jid(LUser, LServer, ""),
		      jlib:make_jid(LUser, LServer, ""),
		      {xmlelement, "broadcast", [],
		       [{privacy_list,
			 #userlist{name = Name, list = List, needdb = NeedDb},
			 Name}]}),*)
			Lwt.return res
	      )
	)
      | None ->
	  Lwt.return (`Error Jlib.err_bad_request)

  let process_active_set luser lserver =
    function
      | Some name -> (
	  match_lwt sql_get_privacy_list_id luser lserver name with
	    | [] ->
		Lwt.return (`Error Jlib.err_item_not_found)
	    | [id] -> (
		lwt ritems = sql_get_privacy_list_data_by_id id lserver in
		let items = List.map raw_to_item ritems in
		  Lwt.return (
		    `ResultList (None, {name = Some name; list = items}))
	      )
	    | _ -> assert false
	)
      | None ->
	  Lwt.return (`ResultList (None, {name = None; list = []}))

  let process_default_set luser lserver =
    function
      | Some name -> (
	  let f () =
	    match_lwt sql_get_privacy_list_names_t luser with
	      | [] ->
		  Lwt.return (`Error Jlib.err_item_not_found)
	      | names -> (
		  if List.mem name names then (
		    lwt _ = sql_set_default_privacy_list luser name in
		      Lwt.return (`Result None)
		  ) else
		    Lwt.return (`Error Jlib.err_item_not_found)
		)
	  in
	    Sql.transaction lserver f
	)
      | None -> (
	  lwt _ = sql_unset_default_privacy_list luser lserver in
	    Lwt.return (`Result None)
	)

  let process_iq_set _ (from, _to', {Jlib.iq_type = `Set subel}) =
    let {Jlib.luser = luser; Jlib.lserver = lserver} = from in
    let `XmlElement (_, _, els) = subel in
    lwt res =
      match Xml.remove_cdata els with
	| [`XmlElement (name, attrs, subels)] -> (
	    let list_name = Xml.get_attr "name" attrs in
	      match name with
		| "list" ->
		    process_list_set luser lserver list_name
		      (Xml.remove_cdata subels)
		| "active" ->
		    process_active_set luser lserver list_name
		| "default" ->
		    process_default_set luser lserver list_name
		| _ ->
		    Lwt.return (`Error Jlib.err_bad_request)
	  )
	| _ ->
	    Lwt.return (`Error Jlib.err_bad_request)
    in
      Lwt.return (Jamler_hooks.OK, res)


  let start host =
    Lwt.return (
      [Gen_mod.fold_hook privacy_iq_get host process_iq_get 50;
       Gen_mod.fold_hook privacy_iq_set host process_iq_set 50;
(*
    ejabberd_hooks:add(privacy_get_user_list, Host,
		       ?MODULE, get_user_list, 50),*)
       Gen_mod.fold_hook privacy_check_packet host check_packet 50;
(*    ejabberd_hooks:add(privacy_updated_list, Host,
		       ?MODULE, updated_list, 50),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
*)
       Gen_mod.iq_handler `SM host <:ns<PRIVACY>> process_iq ();
      ]
    )

  let stop _host =
    Lwt.return ()

(*
-record(privacy, {us,
		  default = none,
		  lists = []}).















get_user_list(_, User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),

    case catch sql_get_default_privacy_list(LUser, LServer) of
	{selected, ["name"], []} ->
	    #userlist{};
	{selected, ["name"], [{Default}]} ->
	    case catch sql_get_privacy_list_data(LUser, LServer, Default) of
		{selected, ["t", "value", "action", "ord", "match_all",
			    "match_iq", "match_message",
			    "match_presence_in", "match_presence_out"],
		 RItems} ->
		    Items = lists:map(fun raw_to_item/1, RItems),
		    NeedDb = is_list_needdb(Items),
		    #userlist{name = Default, list = Items, needdb = NeedDb};
		_ ->
		    #userlist{}
	    end;
	_ ->
	    #userlist{}
    end.



remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    sql_del_privacy_lists(LUser, LServer).


updated_list(_,
	     #userlist{name = OldName} = Old,
	     #userlist{name = NewName} = New) ->
    if
	OldName == NewName ->
	    New;
	true ->
	    Old
    end.


*)

end

let () = Gen_mod.register_mod (module ModPrivacySql : Gen_mod.Module)

