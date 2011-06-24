module LJID = Jlib.LJID
module Hooks = Jamler_hooks
module Router = Jamler_router
module GenIQHandler = Jamler_gen_iq_handler
module SM = Jamler_sm
module C2S = Jamler_c2s.C2S

type subscription =
    [ `None of [ `None | `Out | `In | `Both ]
    | `To of [ `None | `In ]
    | `From of [ `None | `Out ]
    | `Both ]

type subscription_request =
    [ `Subscribe
    | `Subscribed
    | `Unsubscribe
    | `Unsubscribed
    ]

type +'a roster_item =
    {jid : string * string * string;
     name : string;
     subscription : [> subscription ] as 'a;
     groups : string list;
     askmessage : string;
    }

let rec process_item_attrs item =
  function
    | (attr, value) :: attrs -> (
	match attr with
	  | "jid" -> (
	      match Jlib.string_to_jid value with
		| None ->
		    process_item_attrs item attrs
		| Some jid1 ->
		    let jid =
		      (jid1.Jlib.user, jid1.Jlib.server, jid1.Jlib.resource)
		    in
		      process_item_attrs {item with jid} attrs
	    )
	  | "name" ->
	      process_item_attrs {item with name = value} attrs
	  | "subscription" -> (
	      match value with
		| "remove" ->
		    process_item_attrs
		      {item with subscription = `Remove} attrs
		| _ ->
		    process_item_attrs item attrs
	    )
	  | "ask" ->
	      process_item_attrs item attrs
	  | _ ->
	      process_item_attrs item attrs
      )
    | [] ->
	item


let rec process_item_els item =
  function
    | `XmlElement (name, _attrs, sels) :: els -> (
	match name with
	  | "group" ->
	      let groups = Xml.get_cdata sels :: item.groups in
		process_item_els {item with groups} els
	  | _ ->
	      process_item_els item els
      )
    | `XmlCdata _ :: els ->
	process_item_els item els
    | [] ->
	item

let in_state_change subscription t =
  match subscription, t with
    | `None `None, `Subscribe    -> Some (`None `In)
    | `None `None, `Subscribed   -> None
    | `None `None, `Unsubscribe  -> None
    | `None `None, `Unsubscribed -> None
    | `None `Out,  `Subscribe    -> Some (`None `Both)
    | `None `Out,  `Subscribed   -> Some (`To `None)
    | `None `Out,  `Unsubscribe  -> None
    | `None `Out,  `Unsubscribed -> Some (`None `None)
    | `None `In,   `Subscribe    -> None
    | `None `In,   `Subscribed   -> None
    | `None `In,   `Unsubscribe  -> Some (`None `None)
    | `None `In,   `Unsubscribed -> None
    | `None `Both, `Subscribe    -> None
    | `None `Both, `Subscribed   -> Some (`To `In)
    | `None `Both, `Unsubscribe  -> Some (`None `Out)
    | `None `Both, `Unsubscribed -> Some (`None `In)
    | `To   `None, `Subscribe    -> Some (`To `In)
    | `To   `None, `Subscribed   -> None
    | `To   `None, `Unsubscribe  -> None
    | `To   `None, `Unsubscribed -> Some (`None `None)
    | `To   `In,   `Subscribe    -> None
    | `To   `In,   `Subscribed   -> None
    | `To   `In,   `Unsubscribe  -> Some (`To `None)
    | `To   `In,   `Unsubscribed -> Some (`None `In)
    | `From `None, `Subscribe    -> None
    | `From `None, `Subscribed   -> Some `Both
    | `From `None, `Unsubscribe  -> Some (`None `None)
    | `From `None, `Unsubscribed -> None
    | `From `Out,  `Subscribe    -> None
    | `From `Out,  `Subscribed   -> Some `Both
    | `From `Out,  `Unsubscribe  -> Some (`None `Out)
    | `From `Out,  `Unsubscribed -> Some (`From `None)
    | `Both,       `Subscribe    -> None
    | `Both,       `Subscribed   -> None
    | `Both,       `Unsubscribe  -> Some (`To `None)
    | `Both,       `Unsubscribed -> Some (`From `None)

let out_state_change subscription t =
  match subscription, t with
    | `None `None, `Subscribe    -> Some (`None `Out)
    | `None `None, `Subscribed   -> None
    | `None `None, `Unsubscribe  -> None
    | `None `None, `Unsubscribed -> None
    | `None `Out,  `Subscribe    -> Some (`None `Out)
    | `None `Out,  `Subscribed   -> None
    | `None `Out,  `Unsubscribe  -> Some (`None `None)
    | `None `Out,  `Unsubscribed -> None
    | `None `In,   `Subscribe    -> Some (`None `Both)
    | `None `In,   `Subscribed   -> Some (`From `None)
    | `None `In,   `Unsubscribe  -> None
    | `None `In,   `Unsubscribed -> Some (`None `None)
    | `None `Both, `Subscribe    -> None
    | `None `Both, `Subscribed   -> Some (`From `Out)
    | `None `Both, `Unsubscribe  -> Some (`None `In)
    | `None `Both, `Unsubscribed -> Some (`None `Out)
    | `To   `None, `Subscribe    -> None
    | `To   `None, `Subscribed   -> Some `Both
    | `To   `None, `Unsubscribe  -> Some (`None `None)
    | `To   `None, `Unsubscribed -> None
    | `To   `In,   `Subscribe    -> None
    | `To   `In,   `Subscribed   -> Some `Both
    | `To   `In,   `Unsubscribe  -> Some (`None `In)
    | `To   `In,   `Unsubscribed -> Some (`To `None)
    | `From `None, `Subscribe    -> Some (`From `Out)
    | `From `None, `Subscribed   -> None
    | `From `None, `Unsubscribe  -> None
    | `From `None, `Unsubscribed -> Some (`None `None)
    | `From `Out,  `Subscribe    -> None
    | `From `Out,  `Subscribed   -> None
    | `From `Out,  `Unsubscribe  -> Some (`From `None)
    | `From `Out,  `Unsubscribed -> Some (`None `Out)
    | `Both,       `Subscribe    -> None
    | `Both,       `Subscribed   -> None
    | `Both,       `Unsubscribe  -> Some (`From `None)
    | `Both,       `Unsubscribed -> Some (`To `None)

let in_auto_reply subscription t =
  match subscription, t with
    | `From `None, `Subscribe   -> Some `Subscribed
    | `From `Out,  `Subscribe   -> Some `Subscribed
    | `Both,       `Subscribe   -> Some `Subscribed
    | `None `In,   `Unsubscribe -> Some `Unsubscribed
    | `None `Both, `Unsubscribe -> Some `Unsubscribed
    | `To   `In,   `Unsubscribe -> Some `Unsubscribed
    | `From `None, `Unsubscribe -> Some `Unsubscribed
    | `From `Out,  `Unsubscribe -> Some `Unsubscribed
    | `Both,       `Unsubscribe -> Some `Unsubscribed
    | _,           _            -> None



module type RosterStorage =
sig
  val read_roster :
    Jlib.nodepreped ->
    Jlib.namepreped -> (LJID.t * subscription roster_item) list Lwt.t
  val delete_roster : Jlib.nodepreped -> Jlib.namepreped -> unit Lwt.t
  val read_roster_item :
    Jlib.nodepreped ->
    Jlib.namepreped -> LJID.t -> subscription roster_item option Lwt.t
  val write_roster_item :
    Jlib.nodepreped ->
    Jlib.namepreped -> LJID.t -> subscription roster_item -> unit Lwt.t
  val delete_roster_item :
    Jlib.nodepreped -> Jlib.namepreped -> LJID.t -> unit Lwt.t
  val item_set_transaction :
    Jlib.nodepreped -> Jlib.namepreped -> Jlib.jid ->
    Xml.attributes -> Xml.element_cdata list ->
    (subscription roster_item *
       [subscription | `Remove ] roster_item *
       LJID.t) Lwt.t
  val subscription_transaction :
    [ `In | `Out ] ->
    Jlib.nodepreped -> Jlib.namepreped -> Jlib.jid ->
    subscription_request -> string ->
    (subscription roster_item option *
       [ `Subscribed | `Unsubscribed ] option) Lwt.t
end

module Make(RS : RosterStorage) :
sig
end
  =
struct
  let roster_hash items =
    string_of_int
      (Hashtbl.hash
	 (List.sort compare
	    (List.map
	       (fun (jid, item) ->
		  (jid, {item with groups = List.sort compare item.groups})
	       ) items)))
		
  let roster_versioning_enabled host =
    (* TODO *)
    false
    (*gen_mod:get_module_opt(Host, ?MODULE, versioning, false).*)

  let roster_version_on_db host =
    (* TODO *)
    false
    (*gen_mod:get_module_opt(Host, ?MODULE, store_current_id, false).*)

  (* Returns a list that may contain an xmlelement with the XEP-237 feature if it's enabled. *)
  let get_versioning_feature acc host =
    if roster_versioning_enabled host then (
      let feature = `XmlElement ("ver", [("xmlns", <:ns<ROSTER_VER>>)], []) in
	feature :: acc
    ) else []

  let roster_version lserver luser =
    (* TODO *)
    ""
(*
	US = {LUser, LServer},
	case roster_version_on_db(LServer) of
		true ->
			case mnesia:dirty_read(roster_version, US) of
				[#roster_version{version = V}] -> V;
				[] -> not_found
			end;
		false ->
			roster_hash(ejabberd_hooks:run_fold(roster_get, LServer, [], [US]))
	end.
*)

  let item_to_xml item =
    let (u, s, r) = item.jid in
    let attrs = [("jid", Jlib.jid_to_string' u s r)] in
    let attrs =
      match item.name with
	| "" -> attrs
	| name -> ("name", name) :: attrs
    in
    let attrs =
      match item.subscription with
	| `None _ -> ("subscription", "none")   :: attrs
	| `From _ -> ("subscription", "from")   :: attrs
	| `To _ ->   ("subscription", "to")     :: attrs
	| `Both ->   ("subscription", "both")   :: attrs
	| `Remove -> ("subscription", "remove") :: attrs
    in
    let attrs =
      let pending =
	match item.subscription with
	  | `None pending -> (pending :> [ `Both | `In | `None | `Out ])
	  | `From pending -> (pending :> [ `Both | `In | `None | `Out ])
	  | `To pending -> (pending :> [ `Both | `In | `None | `Out ])
	  | `Both
	  | `Remove -> `None
      in
	match pending with
	  | `Out -> ("ask", "subscribe") :: attrs
	  | `Both -> ("ask", "subscribe") :: attrs
	  | _ -> attrs
    in
    let subels =
      List.map
	(fun g -> `XmlElement ("group", [], [(`XmlCdata g)]))
	item.groups
    in
      (`XmlElement ("item", attrs, subels) : Xml.element)

  let item_to_xml' (_jid, item) = item_to_xml item

  let roster_get = Hooks.create_fold ()

(*
 Load roster from DB only if neccesary. 
 It is neccesary if
     - roster versioning is disabled in server OR
     - roster versioning is not used by the client OR
     - roster versioning is used by server and client, BUT the server isn't storing versions on db OR
     - the roster version from client don't match current version.
*)
  let process_iq_get from to' iq =
    let `Get subel = iq.Jlib.iq_type in
    let luser = from.Jlib.luser in
    let lserver = from.Jlib.lserver in
    let us = (luser, lserver) in
      try_lwt
	lwt to_send =
	  match Xml.get_tag_attr "ver" subel, 
	    roster_versioning_enabled lserver,
	    roster_version_on_db lserver with
	      | Some requested_version, true, true ->
		  (* Retrieve version from DB. Only load entire roster
		     when neccesary. *)
			(*case mnesia:dirty_read(roster_version, US) of
				[#roster_version{version = RequestedVersion}] ->
					{false, false};
				[#roster_version{version = NewVersion}] ->
					{lists:map(fun item_to_xml/1, 
						ejabberd_hooks:run_fold(roster_get, To#jid.lserver, [], [US])), NewVersion};
				[] ->*)
		  let roster_version =
		    string_of_int (Hashtbl.hash (Unix.gettimeofday ()))
		  in
		    (*mnesia:dirty_write(#roster_version{us = US, version = RosterVersion}),
					{lists:map(fun item_to_xml/1,
						ejabberd_hooks:run_fold(roster_get, To#jid.lserver, [], [US])), RosterVersion}*) (* TODO *)
		    Lwt.return (Some ([], Some roster_version))

	      | Some requested_version, true, false ->
		  lwt roster_items =
		    Hooks.run_fold roster_get to'.Jlib.lserver [] us
		  in
		  let hash = roster_hash roster_items in
		    Lwt.return (
		      if hash = requested_version
		      then None
		      else Some (List.map item_to_xml' roster_items, Some hash)
		    )

	      | _ ->
		  lwt roster_items =
		    Hooks.run_fold roster_get to'.Jlib.lserver [] us
		  in
		    Lwt.return (Some (List.map item_to_xml' roster_items, None))
	in
	let subel =
	  match to_send with
	    | None -> None
	    | Some (items, None) ->
		Some (`XmlElement ("query", [("xmlns", <:ns<ROSTER>>)],
				   (items :> Xml.element_cdata list)))
	    | Some (items, Some version) ->
		Some (`XmlElement ("query", [("xmlns", <:ns<ROSTER>>);
					     ("ver", version)],
				   (items :> Xml.element_cdata list)))
	in
	  Lwt.return {iq with Jlib.iq_type = `Result subel}
      with
    	| _ ->
	    Lwt.return 
	      {iq with Jlib.iq_type = `Error (Jlib.err_internal_server_error,
					      Some subel)}

  let get_user_roster acc (u, s) =
    lwt items = RS.read_roster u s in
    let items =
      List.filter
	(function
	   | (jid, {subscription = `None `In; _}) -> false
	   | _ -> true) items @ acc
    in
      Lwt.return (Hooks.OK, items)

  let push_item'' user server resource from item roster_version =
    let extra_attrs =
      match roster_version with
	| None -> []
	| Some roster_version -> [("ver", roster_version)]
    in
    let resiq =
      {Jlib.iq_type =
	  `Set (`XmlElement ("query",
			     ("xmlns", <:ns<ROSTER>>) :: extra_attrs,
			     [(item_to_xml item :> Xml.element_cdata)]));
       Jlib.iq_xmlns = <:ns<ROSTER>>;
       Jlib.iq_id = "push" ^ Jlib.get_random_string ();
       Jlib.iq_lang = "";
      }
    in
      Router.route
	from
	(Jlib.make_jid' user server resource)
	(Jlib.iq_to_xml resiq)

  (* TODO: don't push to those who didn't load roster *)
  let push_item' user server resource from item =
    push_item'' user server resource from item None


  let push_item user server from item =
(* TODO *)
    (*SM.route
      (Jlib.make_jid "" "" "")
      (jlib:make_jid' user server (Jlib.resourceprep_exn ""))
      (`XmlElement
	 ("broadcast", [],
	  [{item,
	    Item#roster.jid,
	    Item#roster.subscription}])),*)
(* TODO *)
  (*
    case roster_versioning_enabled(Server) of
	true ->
		push_item_version(Server, User, From, Item, roster_version(Server, User));
	false ->*)
    List.iter
      (fun resource ->
	 push_item' user server resource from item)
      (SM.get_user_resources user server)

(*
%% @doc Roster push, calculate and include the version attribute.
%% TODO: don't push to those who didn't load roster
push_item_version(Server, User, From, Item, RosterVersion)  ->
    lists:foreach(fun(Resource) ->
			  push_item(User, Server, Resource, From, Item, RosterVersion)
		end, ejabberd_sm:get_user_resources(User, Server)).
*)


  let send_presence_type from to' type' =
    Router.route from to'
      (`XmlElement ("presence", [("type", type')], []))

  let send_unsubscribing_presence from jid item =
    let is_to =
      match item.subscription with
	| `Both -> true
	| `To _ -> true
	| _ -> false
    in
    let is_from =
      match item.subscription with
	| `Both -> true
	| `From _ -> true
	| _ -> false
    in
      if is_to then (
	send_presence_type
	  (Jlib.jid_remove_resource from)
	  (Jlib.ljid_to_jid jid)
	  "unsubscribe"
      );
      if is_from then (
	send_presence_type
	  (Jlib.jid_remove_resource from)
	  (Jlib.ljid_to_jid jid)
	  "unsubscribed"
      )


  let process_item_set from to' =
    function
      | `XmlElement (_name, attrs, els) -> (
	  let jid = Jlib.string_to_jid (Xml.get_attr_s "jid" attrs) in
	  let {Jlib.luser = luser; Jlib.lserver = lserver; _} = from in
	    match jid with
	      | None ->
		  Lwt.return ()
	      | Some jid1 -> (
		  match_lwt RS.item_set_transaction luser lserver jid1 attrs els with
		    | (old_item, item, ljid) -> (
			push_item luser lserver to' item;
			(match item.subscription with
			   | `Remove ->
			       send_unsubscribing_presence from ljid old_item
			   | _ ->
			       ()
			);
			Lwt.return ()
		      )
			  (*E ->
			    ?DEBUG("ROSTER: roster item set error: ~p~n", [E]),
			    ok*)
		)
	)
      | `XmlCdata _ -> Lwt.return ()


  let process_iq_set from to' iq =
    let `Set subel = iq.Jlib.iq_type in
    let `XmlElement (_name, _attrs, els) = subel in
    lwt () = Lwt_list.iter_s (fun el -> process_item_set from to' el) els in
      Lwt.return {iq with Jlib.iq_type = `Result None}


  let process_local_iq from to' iq =
    match iq with
      | {Jlib.iq_type = `Set _; _} as iq ->
	  process_iq_set from to' iq
      | {Jlib.iq_type = `Get _; _} as iq ->
	  process_iq_get from to' iq

  let process_iq from to' iq =
    let (`Set sub_el | `Get sub_el) = iq.Jlib.iq_type in
    let lserver = from.Jlib.lserver in
    lwt iq_res =
      if List.mem lserver (Jamler_config.myhosts ())
      then
	process_local_iq from to' iq
      else
	Lwt.return
	  {iq with
	     Jlib.iq_type = `Error (Jlib.err_item_not_found, Some sub_el)}
    in
      Lwt.return (`IQ iq_res)


  let rec fill_subscription_lists items f t =
    match items with
      | (j, i) :: is -> (
	  match i.subscription with
	    | `Both ->
		fill_subscription_lists is (j :: f) (j :: t)
	    | `From _ ->
		fill_subscription_lists is (j :: f) t
	    | `To _ ->
		fill_subscription_lists is f (j :: t)
	    | `None _ ->
		fill_subscription_lists is f t
	)
      | [] ->
	  (f, t)

  let get_subscription_lists _ (user, server) =
    lwt items = RS.read_roster user server in
      Lwt.return (Hooks.OK, fill_subscription_lists items [] [])


  let process_subscription direction luser lserver jid1 type' reason =
    match_lwt (RS.subscription_transaction
		 direction luser lserver jid1 type' reason) with
      | (push, auto_reply) -> (
	  (match auto_reply with
	     | None -> ()
	     | Some auto_reply ->
		 let t =
		   match auto_reply with
		     | `Subscribed -> "subscribed"
		     | `Unsubscribed -> "unsubscribed"
		 in
		   Router.route
		     (Jlib.make_jid'
			luser lserver (Jlib.resourceprep_exn ""))
		     jid1
		     (`XmlElement ("presence", [("type", t)], []))
	  );
	  match push with
	    | Some item -> (
		if item.subscription <> `None `In then (
		  push_item luser lserver
		    (Jlib.make_jid'
		       luser lserver (Jlib.resourceprep_exn ""))
		    item
		);
		Lwt.return true;
	      )
	    | None ->
		Lwt.return false
	)

  let in_subscription _ (user, server, jid, type', reason) =
    lwt res = process_subscription `In user server jid type' reason in
      Lwt.return (Hooks.OK, res)

  let out_subscription (user, server, jid, type') =
    lwt _ = process_subscription `Out user server jid type' "" in
      Lwt.return Hooks.OK


(*
remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    send_unsubscription_to_rosteritems(LUser, LServer),
    F = fun() ->
		lists:foreach(fun(R) ->
				      mnesia:delete_object(R)
			      end,
			      mnesia:index_read(roster, US, #roster.us))
        end,
    mnesia:transaction(F).

%% For each contact with Subscription:
%% Both or From, send a "unsubscribed" presence stanza;
%% Both or To, send a "unsubscribe" presence stanza.
send_unsubscription_to_rosteritems(LUser, LServer) ->
    RosterItems = get_user_roster([], {LUser, LServer}),
    From = jlib:make_jid({LUser, LServer, ""}),
    lists:foreach(fun(RosterItem) ->
			  send_unsubscribing_presence(From, RosterItem)
		  end,
		  RosterItems).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_items(User, Server, SubEl) ->
    {xmlelement, _Name, _Attrs, Els} = SubEl,
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    F = fun() ->
		lists:foreach(fun(El) ->
				      process_item_set_t(LUser, LServer, El)
			      end, Els)
	end,
    mnesia:transaction(F).

process_item_set_t(LUser, LServer, {xmlelement, _Name, Attrs, Els}) ->
    JID1 = jlib:string_to_jid(xml:get_attr_s("jid", Attrs)),
    case JID1 of
	error ->
	    ok;
	_ ->
	    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
	    LJID = {JID1#jid.luser, JID1#jid.lserver, JID1#jid.lresource},
	    Item = #roster{usj = {LUser, LServer, LJID},
			   us = {LUser, LServer},
			   jid = JID},
	    Item1 = process_item_attrs_ws(Item, Attrs),
	    Item2 = process_item_els(Item1, Els),
	    case Item2#roster.subscription of
		remove ->
		    mnesia:delete({roster, {LUser, LServer, LJID}});
		_ ->
		    mnesia:write(Item2)
	    end
    end;
process_item_set_t(_LUser, _LServer, _) ->
    ok.

process_item_attrs_ws(Item, [{Attr, Val} | Attrs]) ->
    case Attr of
	"jid" ->
	    case jlib:string_to_jid(Val) of
		error ->
		    process_item_attrs_ws(Item, Attrs);
		JID1 ->
		    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
		    process_item_attrs_ws(Item#roster{jid = JID}, Attrs)
	    end;
	"name" ->
	    process_item_attrs_ws(Item#roster{name = Val}, Attrs);
	"subscription" ->
	    case Val of
		"remove" ->
		    process_item_attrs_ws(Item#roster{subscription = remove},
					  Attrs);
		"none" ->
		    process_item_attrs_ws(Item#roster{subscription = none},
					  Attrs);
		"both" ->
		    process_item_attrs_ws(Item#roster{subscription = both},
					  Attrs);
		"from" ->
		    process_item_attrs_ws(Item#roster{subscription = from},
					  Attrs);
		"to" ->
		    process_item_attrs_ws(Item#roster{subscription = to},
					  Attrs);
		_ ->
		    process_item_attrs_ws(Item, Attrs)
	    end;
	"ask" ->
	    process_item_attrs_ws(Item, Attrs);
	_ ->
	    process_item_attrs_ws(Item, Attrs)
    end;
process_item_attrs_ws(Item, []) ->
    Item.

get_in_pending_subscriptions(Ls, User, Server) ->
    JID = jlib:make_jid(User, Server, ""),
    US = {JID#jid.luser, JID#jid.lserver},
    case mnesia:dirty_index_read(roster, US, #roster.us) of
	Result when is_list(Result) ->
    	    Ls ++ lists:map(
		    fun(R) ->
			    Message = R#roster.askmessage,
			    Status  = if is_binary(Message) ->
					      binary_to_list(Message);
					 true ->
					      ""
				      end,
			    {xmlelement, "presence",
			     [{"from", jlib:jid_to_string(R#roster.jid)},
			      {"to", jlib:jid_to_string(JID)},
			      {"type", "subscribe"}],
			     [{xmlelement, "status", [],
			       [{xmlcdata, Status}]}]}
		    end,
		    lists:filter(
		      fun(R) ->
			      case R#roster.ask of
				  in   -> true;
				  both -> true;
				  _ -> false
			      end
		      end,
		      Result));
	_ ->
	    Ls
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_jid_info(_, User, Server, JID) ->
    LUser = jlib:nodeprep(User),
    LJID = jlib:jid_tolower(JID),
    LServer = jlib:nameprep(Server),
    case catch mnesia:dirty_read(roster, {LUser, LServer, LJID}) of
	[#roster{subscription = Subscription, groups = Groups}] ->
	    {Subscription, Groups};
	_ ->
	    LRJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
	    if
		LRJID == LJID ->
		    {none, []};
		true ->
		    case catch mnesia:dirty_read(
				 roster, {LUser, LServer, LRJID}) of
			[#roster{subscription = Subscription,
				 groups = Groups}] ->
			    {Subscription, Groups};
			_ ->
			    {none, []}
		    end
	    end
    end.
*)
(*
webadmin_page(_, Host,
	      #request{us = _US,
		       path = ["user", U, "roster"],
		       q = Query,
		       lang = Lang} = _Request) ->
    Res = user_roster(U, Host, Query, Lang),
    {stop, Res};

webadmin_page(Acc, _, _) -> Acc.

user_roster(User, Server, Query, Lang) ->
    US = {jlib:nodeprep(User), jlib:nameprep(Server)},
    Items1 = mnesia:dirty_index_read(roster, US, #roster.us),
    Res = user_roster_parse_query(User, Server, Items1, Query),
    Items = mnesia:dirty_index_read(roster, US, #roster.us),
    SItems = lists:sort(Items),
    FItems =
	case SItems of
	    [] ->
		[?CT("None")];
	    _ ->
		[?XE("table",
		     [?XE("thead",
			  [?XE("tr",
			       [?XCT("td", "Jabber ID"),
				?XCT("td", "Nickname"),
				?XCT("td", "Subscription"),
				?XCT("td", "Pending"),
				?XCT("td", "Groups")
			       ])]),
		      ?XE("tbody",
			  lists:map(
			    fun(R) ->
				    Groups =
					lists:flatmap(
					  fun(Group) ->
						  [?C(Group), ?BR]
					  end, R#roster.groups),
				    Pending = ask_to_pending(R#roster.ask),
				    TDJID = build_contact_jid_td(R#roster.jid),
				    ?XE("tr",
					[TDJID,
					 ?XAC("td", [{"class", "valign"}],
					      R#roster.name),
					 ?XAC("td", [{"class", "valign"}],
					      atom_to_list(R#roster.subscription)),
					 ?XAC("td", [{"class", "valign"}],
					      atom_to_list(Pending)),
					 ?XAE("td", [{"class", "valign"}], Groups),
					 if
					     Pending == in ->
						 ?XAE("td", [{"class", "valign"}],
						      [?INPUTT("submit",
							       "validate" ++
							       ejabberd_web_admin:term_to_id(R#roster.jid),
							       "Validate")]);
					     true ->
						 ?X("td")
					 end,
					 ?XAE("td", [{"class", "valign"}],
					      [?INPUTT("submit",
						       "remove" ++
						       ejabberd_web_admin:term_to_id(R#roster.jid),
						       "Remove")])])
			    end, SItems))])]
	end,
    [?XC("h1", ?T("Roster of ") ++ us_to_list(US))] ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    error -> [?XREST("Bad format")];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      FItems ++
	      [?P,
	       ?INPUT("text", "newjid", ""), ?C(" "),
	       ?INPUTT("submit", "addjid", "Add Jabber ID")
	      ])].

build_contact_jid_td(RosterJID) ->
    %% Convert {U, S, R} into {jid, U, S, R, U, S, R}:
    ContactJID = jlib:make_jid(RosterJID),
    JIDURI = case {ContactJID#jid.luser, ContactJID#jid.lserver} of
		 {"", _} -> "";
		 {CUser, CServer} ->
		     case lists:member(CServer, ?MYHOSTS) of
			 false -> "";
			 true -> "/admin/server/" ++ CServer ++ "/user/" ++ CUser ++ "/"
		     end
	     end,
    case JIDURI of
	[] ->
	    ?XAC("td", [{"class", "valign"}], jlib:jid_to_string(RosterJID));
	URI when is_list(URI) ->
	    ?XAE("td", [{"class", "valign"}], [?AC(JIDURI, jlib:jid_to_string(RosterJID))])
    end.

user_roster_parse_query(User, Server, Items, Query) ->
    case lists:keysearch("addjid", 1, Query) of
	{value, _} ->
	    case lists:keysearch("newjid", 1, Query) of
		{value, {_, undefined}} ->
		    error;
		{value, {_, SJID}} ->
		    case jlib:string_to_jid(SJID) of
			JID when is_record(JID, jid) ->
			    user_roster_subscribe_jid(User, Server, JID),
			    ok;
			error ->
			    error
		    end;
		false ->
		    error
	    end;
	false ->
	    case catch user_roster_item_parse_query(
			 User, Server, Items, Query) of
		submitted ->
		    ok;
		{'EXIT', _Reason} ->
		    error;
		_ ->
		    nothing
	    end
    end.


user_roster_subscribe_jid(User, Server, JID) ->
    out_subscription(User, Server, JID, subscribe),
    UJID = jlib:make_jid(User, Server, ""),
    ejabberd_router:route(
      UJID, JID, {xmlelement, "presence", [{"type", "subscribe"}], []}).

user_roster_item_parse_query(User, Server, Items, Query) ->
    lists:foreach(
      fun(R) ->
	      JID = R#roster.jid,
	      case lists:keysearch(
		     "validate" ++ ejabberd_web_admin:term_to_id(JID), 1, Query) of
		  {value, _} ->
		      JID1 = jlib:make_jid(JID),
		      out_subscription(
			User, Server, JID1, subscribed),
		      UJID = jlib:make_jid(User, Server, ""),
		      ejabberd_router:route(
			UJID, JID1, {xmlelement, "presence",
				     [{"type", "subscribed"}], []}),
		      throw(submitted);
		  false ->
		      case lists:keysearch(
			     "remove" ++ ejabberd_web_admin:term_to_id(JID), 1, Query) of
			  {value, _} ->
			      UJID = jlib:make_jid(User, Server, ""),
			      process_iq(
				UJID, UJID,
				#iq{type = set,
				    sub_el = {xmlelement, "query",
					      [{"xmlns", ?NS_ROSTER}],
					      [{xmlelement, "item",
						[{"jid", jlib:jid_to_string(JID)},
						 {"subscription", "remove"}],
						[]}]}}),
			      throw(submitted);
			  false ->
			      ok
		      end

	      end
      end, Items),
    nothing.

us_to_list({User, Server}) ->
    jlib:jid_to_string({User, Server, ""}).

webadmin_user(Acc, _User, _Server, Lang) ->
    Acc ++ [?XE("h3", [?ACT("roster/", "Roster")])].
*)

  let _ =
    let test = Jlib.string_to_jid_exn "test@e.localhost" in
    let test10 = Jlib.string_to_jid_exn "test10@e.localhost" in
    let i1 = {jid = ("test10", "e.localhost", "");
	      name = "test10__";
	      subscription = `Both;
	      groups = ["asd"; "qwe"];
	      askmessage = "";
	     }
    in
    let i2 = {jid = ("test", "e.localhost", "");
	      name = "test";
	      subscription = `Both;
	      groups = ["test"];
	      askmessage = "";
	     }
    in
    lwt () =
      RS.write_roster_item test.Jlib.luser test.Jlib.lserver
	(test10.Jlib.luser, test10.Jlib.lserver, test10.Jlib.lresource)
	i1
    in
    lwt () =
      RS.write_roster_item test10.Jlib.luser test10.Jlib.lserver
	(test.Jlib.luser, test.Jlib.lserver, test.Jlib.lresource)
	i2
    in
      Lwt.return ()

  let _ =
    let host = Jlib.nameprep_exn "e.localhost" in
      Hooks.add_fold roster_get host get_user_roster 50;
      Hooks.add_fold SM.roster_in_subscription host in_subscription 50;
      Hooks.add C2S.roster_out_subscription host out_subscription 50;
      Hooks.add_fold C2S.roster_get_subscription_lists host
	get_subscription_lists 50;
    (*ejabberd_hooks:add(roster_get_jid_info, Host,
		       ?MODULE, get_jid_info, 50),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(resend_subscription_requests_hook, Host,
		       ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:add(roster_get_versioning_feature, Host,
		       ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:add(webadmin_page_host, Host,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host,
		       ?MODULE, webadmin_user, 50),*)
      GenIQHandler.add_iq_handler `SM host <:ns<ROSTER>> process_iq ()

(*
  start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(roster,[{disc_copies, [node()]},
				{attributes, record_info(fields, roster)}]),
    mnesia:create_table(roster_version, [{disc_copies, [node()]},
    				{attributes, record_info(fields, roster_version)}]),

    update_table(),
    mnesia:add_table_index(roster, us),
    mnesia:add_table_index(roster_version, us),
    ejabberd_hooks:add(roster_get, Host,
		       ?MODULE, get_user_roster, 50),
    ejabberd_hooks:add(roster_in_subscription, Host,
		       ?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription, Host,
		       ?MODULE, out_subscription, 50),
    ejabberd_hooks:add(roster_get_subscription_lists, Host,
		       ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:add(roster_get_jid_info, Host,
		       ?MODULE, get_jid_info, 50),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(resend_subscription_requests_hook, Host,
		       ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:add(roster_get_versioning_feature, Host,
		       ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:add(webadmin_page_host, Host,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host,
		       ?MODULE, webadmin_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ROSTER,
				  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(roster_get, Host,
			  ?MODULE, get_user_roster, 50),
    ejabberd_hooks:delete(roster_in_subscription, Host,
			  ?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription, Host,
			  ?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(roster_get_subscription_lists, Host,
			  ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
			  ?MODULE, get_jid_info, 50),
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(resend_subscription_requests_hook, Host,
			  ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:delete(roster_get_versioning_feature, Host,
		          ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, Host,
			  ?MODULE, webadmin_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ROSTER).
*)



end

