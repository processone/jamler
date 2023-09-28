module GenIQHandler = Jamler_gen_iq_handler
module Auth = Jamler_auth

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

module LJIDGroup : Set.OrderedType with type t = Jlib.LJID.t * string =
struct
  type t = Jlib.LJID.t * string
  let compare = compare
end

module LJIDGroupSet = Set.Make(LJIDGroup)

module LJIDMap = Map.Make(Jlib.LJID)

type subscription = [ `None | `From | `To | `Both ]

type userlist =
    {name : string option;
     list : listitem list;
     subscriptions : subscription LJIDMap.t;
     groups : LJIDGroupSet.t;
    }

let new_userlist () =
  {name = None;
   list = [];
   subscriptions = LJIDMap.empty;
   groups = LJIDGroupSet.empty}

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

let privacy_get_user_list
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
      [%sql {|
	     select @(name)s from privacy_default_list
	     where username=%(username)s
             |}]
    in
    Sql.query lserver query

  let sql_get_default_privacy_list_t luser =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	select @(name)s from privacy_default_list
	where username=%(username)s
      |}]
    in
    Sql.query_t query

  let sql_get_privacy_list_names luser lserver =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	select @(name)s from privacy_list
	where username=%(username)s
      |}]
    in
    Sql.query lserver query

  let sql_get_privacy_list_names_t luser =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	select @(name)s from privacy_list
	where username=%(username)s
      |}]
    in
      Sql.query_t query

  let sql_get_privacy_list_id luser lserver name =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	select @(id)d from privacy_list
	where username=%(username)s and name=%(name)s
      |}]
    in
    Sql.query lserver query

  let sql_get_privacy_list_id_t luser name =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	select @(id)d from privacy_list
	where username=%(username)s and name=%(name)s
      |}]
    in
    Sql.query_t query

  let sql_get_privacy_list_data luser lserver name =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	select @(t)s, @(value)s, @(action)s, @(ord)d,
               @(match_all)b, @(match_iq)b,
               @(match_message)b, @(match_presence_in)b, @(match_presence_out)b
        from privacy_list_data
        where id = (select id from privacy_list where
                    username=%(username)s and name=%(name)s)
        order by ord
      |}]
    in
    Sql.query lserver query

  let sql_get_privacy_list_data_by_id id lserver =
    let query =
      [%sql {|
	select @(t)s, @(value)s, @(action)s, @(ord)d,
               @(match_all)b, @(match_iq)b,
               @(match_message)b, @(match_presence_in)b, @(match_presence_out)b
        from privacy_list_data 
        where id=%(id)d order by ord
      |}]
    in
    Sql.query lserver query

  let sql_set_default_privacy_list luser name =
    let username = (luser : Jlib.nodepreped :> string) in
    let insert_query =
      [%sql {|
	insert into privacy_default_list(username, name)
	values (%(username)s, %(name)s)
      |}]
    in
    let update_query =
      [%sql {|
	update privacy_default_list
	set name = %(name)s
        where username = %(username)s
      |}]
    in
    Sql.update_t insert_query update_query

  let sql_unset_default_privacy_list luser lserver =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	delete from privacy_default_list
        where username=%(username)s
      |}]
    in
    Sql.query lserver query

  let sql_remove_privacy_list luser name =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	delete from privacy_list
	where username=%(username)s and name=%(name)s
      |}]
    in
    Sql.query_t query

  let sql_add_privacy_list luser name =
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	insert into privacy_list(username, name)
	values (%(username)s, %(name)s)
      |}]
    in
    Sql.query_t query

  let sql_set_privacy_list id ritems =
    let query =
      [%sql {|
	delete from privacy_list_data
	where id=%(id)d
      |}]
    in
    let _ = Sql.query_t query in
      List.iter
	(fun (stype, svalue, saction, order, match_all, match_iq,
	      match_message, match_presence_in, match_presence_out) ->
	   let query =
	     [%sql {|
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
	     |}]
	   in
	   let _ = Sql.query_t query in
	   ()
	) ritems

  let sql_del_privacy_lists luser lserver =
    let user = (luser : Jlib.nodepreped :> string) in
    let server = (lserver : Jlib.namepreped :> string) in
    let us = user ^ "@" ^ server in
    let del_privacy_list_query =
      [%sql {|delete from privacy_list where username=%(user)s|}] in
    let del_privacy_list_data_query =
      [%sql {|delete from privacy_list_data where value=%(us)s|}] in
    let del_privacy_default_list_query =
      [%sql {|delete from privacy_default_list where username=%(user)s|}] in
    let _ = Sql.query lserver del_privacy_list_query in
    let _ = Sql.query lserver del_privacy_list_data_query in
    let _ = Sql.query lserver del_privacy_default_list_query in
    ()

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
	| (JID _ | Subscription _ | Group _) as li_type ->
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

  let is_type_match li_type jid userlist =
    match li_type with
      | No -> true
      | JID vjid -> (
	  let (luser, lserver, lresource) = vjid in
	    match (luser :> string), (lresource :> string) with
	      | "", "" -> (
		  match jid with
		    | (_, lserver', _) when lserver = lserver' ->
			true
		    | _ ->
			false
		)
	      | _, "" -> (
		  match jid with
		    | (luser', lserver', _) when
			luser = luser' &&
			lserver = lserver' ->
			true
		    | _ ->
			false
		)
	      | _, _ ->
		  vjid = jid
	)
      | Subscription value -> (
	  let subscriptions = userlist.subscriptions in
	    try
	      LJIDMap.find jid subscriptions = value
	    with
	      | Not_found ->
		  false
	)
      | Group value ->
	  LJIDGroupSet.mem (jid, value) userlist.groups


  let rec check_packet_aux list ptype jid userlist =
    match list with
      | [] -> true
      | item :: list -> (
	  let {li_type = li_type; action; _} = item in
	    if is_ptype_match item ptype &&
	      is_type_match li_type jid userlist
	    then action
	    else check_packet_aux list ptype jid userlist
	)


(* From is the sender, To is the destination.
   If Dir = out, User@Server is the sender account (From).
   If Dir = in, User@Server is the destination account (To). *)
  let check_packet _ (_user, _server, ({list; _} as userlist),
		      (from, to', `XmlElement (pname, attrs, _)), dir) =
    match list with
      | [] ->
	  (Jamler_hooks.OK, true)
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
	  let res = check_packet_aux list ptype ljid userlist in
	    (Jamler_hooks.OK, res)


  let _process_local_iq _from _to = function
    | {Jlib.iq_type = `Set subel; _} as iq ->
       `IQ {iq with
	   Jlib.iq_type =
	     `Error (Jlib.err_not_allowed, Some subel)}
    | {Jlib.iq_type = `Get _subel; iq_xmlns = [%xmlns "TIME90"]; _} as iq ->
       let utc = Jlib.timestamp_to_iso' (Unix.gmtime (Unix.time ())) in
       `IQ {iq with
	   Jlib.iq_type =
	     `Result
	       (Some (`XmlElement
			("query",
			 [("xmlns", [%xmlns "TIME90"])],
			 [`XmlElement ("utc", [],
				       [`XmlCdata utc])])))}
    | {Jlib.iq_type = `Get _subel; iq_xmlns = [%xmlns "TIME"]; _} as iq ->
       let utc, tzo = Jlib.timestamp_to_iso (Unix.time ()) (Jlib.get_tzo ()) in
       `IQ {iq with
	   Jlib.iq_type =
	     `Result
	       (Some (`XmlElement
			("query",
			 [("xmlns", [%xmlns "TIME"])],
			 [`XmlElement ("time", [],
				       [`XmlElement ("tzo", [],
						     [`XmlCdata tzo]);
					`XmlElement ("utc", [],
						     [`XmlCdata utc])])])))}
    | {Jlib.iq_type = `Get subel; _} as iq ->
       `IQ {iq with
	   Jlib.iq_type =
	     `Error (Jlib.err_service_unavailable, Some subel)}

  let process_iq _from _to iq =
    match iq.Jlib.iq_type with
    | `Set subel
    | `Get subel ->
       `IQ {iq with Jlib.iq_type = `Error (Jlib.err_not_allowed,
					   Some subel)}


  let process_lists_get luser lserver active =
    let default =
      match sql_get_default_privacy_list luser lserver with
      | [] -> None
      | [default] -> Some default
      | _ -> None
    in
    match sql_get_privacy_list_names luser lserver with
    | [] ->
       `Result
	 (Some
	    (`XmlElement ("query", [("xmlns", [%xmlns "PRIVACY"])], [])))
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
       `Result (Some
		  (`XmlElement ("query", [("xmlns", [%xmlns "PRIVACY"])],
				items)))


  let process_list_get luser lserver =
    function
    | Some name -> (
      match sql_get_privacy_list_id luser lserver name with
      | [] ->
	 `Error Jlib.err_item_not_found
      | [id] -> (
	let ritems = sql_get_privacy_list_data_by_id id lserver in
	let items = List.map raw_to_item ritems in
	let litems = List.map item_to_xml items in
	`Result
	  (Some
	     (`XmlElement
		("query", [("xmlns", [%xmlns "PRIVACY"])],
		 [`XmlElement ("list",
			       [("name", name)], litems)])))
      )
      | _ ->
	 `Error Jlib.err_internal_server_error
    )
    | None ->
       `Error Jlib.err_bad_request

  let process_iq_get _ (from, _to, {Jlib.iq_type = `Get subel; _},
			{name = active; _}) =
    let {Jlib.luser = luser; Jlib.lserver = lserver; _} = from in
    let `XmlElement (_, _, els) = subel in
    let res =
      match Xml.remove_cdata els with
      | [] ->
	 process_lists_get luser lserver active
      | [`XmlElement (name, attrs, _subels)] -> (
	match name with
	| "list" ->
	   let list_name = Xml.get_attr "name" attrs in
	   process_list_get luser lserver list_name
	| _ ->
	   `Error Jlib.err_bad_request
      )
      | _ ->
	 `Error Jlib.err_bad_request
    in
    (Jamler_hooks.OK, res)

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
       `Error Jlib.err_bad_request
    | PRemove -> (
      let f () =
	match sql_get_default_privacy_list_t luser with
	| [] ->
	   let _ = sql_remove_privacy_list luser name in
	   (`Result None)
	| [default] ->
	   (* TODO: check active *)
	   if name = default
	   then `Error Jlib.err_conflict
	   else (
	     let _ = sql_remove_privacy_list luser name in
	     `Result None
	   )
	| _ -> assert false
      in
      match Sql.transaction lserver f with
      | `Error _ as error ->
	 error
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
	 res;
    )
    | PList list -> (
      let ritems = List.map item_to_raw list in
      let f () =
	let id =
	  match sql_get_privacy_list_id_t luser name with
	  | [] -> (
	    let _ = sql_add_privacy_list luser name in
	    match sql_get_privacy_list_id_t luser name with
	    | [i] -> i
	    | _ -> assert false
	  )
	  | [i] -> i
	  | _ -> assert false
	in
	sql_set_privacy_list id ritems;
	`Result None
      in
      match Sql.transaction lserver f with
      | `Error _ as error ->
	 error
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
	 res
    )
  )
  | None ->
     `Error Jlib.err_bad_request

  let is_list_needdb items =
    List.exists
      (fun item ->
	 match item.li_type with
	   | Subscription _
	   | Group _ -> true;
	   | JID _ | No -> false
      ) items

  let get_roster_data luser lserver =
    let roster_items =
      Jamler_hooks.run_fold Gen_roster.roster_get lserver [] (luser, lserver)
    in
    let subscriptions, groups =
      List.fold_left
	(fun (s, g) (ljid, item) ->
	  let sub =
	    match item.Gen_roster.subscription with
	    | `None _ -> `None
	    | `From _ -> `From
	    | `To _ -> `To
	    | `Both -> `Both
	  in
	  let s = LJIDMap.add ljid sub s in
	  let g =
	    List.fold_left
	      (fun g gr ->
		LJIDGroupSet.add (ljid, gr) g
	      ) g item.Gen_roster.groups
	  in
	  (s, g)
	) (LJIDMap.empty, LJIDGroupSet.empty) roster_items
    in
    (subscriptions, groups)

  let make_userlist luser lserver name items =
    let subscriptions, groups =
      if is_list_needdb items
      then get_roster_data luser lserver
      else (LJIDMap.empty, LJIDGroupSet.empty)
    in
    {name = Some name;
     list = items;
     subscriptions;
     groups}

  let process_active_set luser lserver =
    function
    | Some name -> (
      match sql_get_privacy_list_id luser lserver name with
      | [] ->
	 `Error Jlib.err_item_not_found
      | [id] -> (
	let ritems = sql_get_privacy_list_data_by_id id lserver in
	let items = List.map raw_to_item ritems in
	let userlist = make_userlist luser lserver name items in
	`ResultList (None, userlist)
      )
      | _ -> assert false
    )
    | None ->
       `ResultList (None, new_userlist ())

  let process_default_set luser lserver =
    function
    | Some name -> (
      let f () =
	match sql_get_privacy_list_names_t luser with
	| [] ->
	   `Error Jlib.err_item_not_found
	| names -> (
	  if List.mem name names then (
	    let _ = sql_set_default_privacy_list luser name in
	    `Result None
	  ) else
	    `Error Jlib.err_item_not_found
	)
      in
      Sql.transaction lserver f
    )
    | None -> (
      let _ = sql_unset_default_privacy_list luser lserver in
      `Result None
    )

  let process_iq_set _ (from, _to', {Jlib.iq_type = `Set subel; _}) =
    let {Jlib.luser = luser; Jlib.lserver = lserver; _} = from in
    let `XmlElement (_, _, els) = subel in
    let res =
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
	   `Error Jlib.err_bad_request
      )
      | _ ->
	 `Error Jlib.err_bad_request
    in
    (Jamler_hooks.OK, res)


  let get_user_list _ (luser, lserver) =
    match sql_get_default_privacy_list luser lserver with
    | [] ->
       (Jamler_hooks.OK, new_userlist ())
    | [default] -> (
      let ritems = sql_get_privacy_list_data luser lserver default in
      let items = List.map raw_to_item ritems in
      let userlist = make_userlist luser lserver default items in
      (Jamler_hooks.OK, userlist)
    )
    | _ ->
       (Jamler_hooks.OK, new_userlist ())

  let remove_user (user, server) =
    sql_del_privacy_lists user server;
    Jamler_hooks.OK

  let start host =
    Mod_disco.register_feature host [%xmlns "PRIVACY"];
    [Gen_mod.fold_hook privacy_iq_get host process_iq_get 50;
     Gen_mod.fold_hook privacy_iq_set host process_iq_set 50;
     Gen_mod.fold_hook privacy_get_user_list host get_user_list 50;
     Gen_mod.fold_hook privacy_check_packet host check_packet 50;
     Gen_mod.hook Auth.remove_user host remove_user 50;
(*    ejabberd_hooks:add(privacy_updated_list, Host,
		       ?MODULE, updated_list, 50),
*)
     Gen_mod.iq_handler `SM host [%xmlns "PRIVACY"] process_iq ();
    ]

  let stop host =
    Mod_disco.register_feature host [%xmlns "PRIVACY"];
    ()

(*

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

