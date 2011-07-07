module GenIQHandler = Jamler_gen_iq_handler
module Hooks = Jamler_hooks
module Router = Jamler_router

module ModOfflineSQL :
sig
  include Gen_mod.Module
  type offline_msg
end
  =
struct
  type offline_msg = {user : Jlib.nodepreped;
		      timestamp : float;
		      expire : float;
		      from : Jlib.jid;
		      to' : Jlib.jid;
		      packet : Xml.element}

  let name = "mod_offline_sql"
  let offline_message_hook = Hooks.create ()
  let resend_offline_messages_hook = Hooks.create_fold ()
  let remove_user = Hooks.create ()
  let anonymous_purge_hook = Hooks.create ()
  let disco_sm_features = Hooks.create_fold ()
  let disco_local_features = Hooks.create_fold ()

  let never = max_float

  let count_offline_messages luser lserver =
    let username = (luser : Jlib.nodepreped :> string) in
      (* TODO: count() doesn't work *)
    let query =
      <:sql<
	select @(xml)s from spool
	where username=%(username)s
      >>
    in
      try_lwt
	(match_lwt Sql.query lserver query with
	   | [] -> Lwt.return 0
	   | _ -> Lwt.return 1)
      with
	| _ ->
	    Lwt.return 0

  let discard_warn_sender msg =
    let packet = msg.packet in
    let from = msg.from in
    let to' = msg.to' in
    let err_txt = "Your contact offline message queue is full. "
      ^ "The message has been discarded." in
    let lang = Xml.get_tag_attr_s "xml:lang" packet in
    let err = Jlib.make_error_reply packet
      (Jlib.errt_resource_constraint lang err_txt) in
      Router.route to' from err

  let store_packet' msg =
    (* Msgs = receive_all(User, [Msg]),
       Len = length(Msgs),
       MaxOfflineMsgs = get_max_user_messages(AccessMaxOfflineMsgs,
       User, Host), *)
    let max_offline_msgs = max_int in
    let host = (msg.to').Jlib.lserver in
    lwt count =
      if max_offline_msgs <> max_int then (
	count_offline_messages msg.user host
      ) else Lwt.return 0
    in
      if count > max_offline_msgs then (
	discard_warn_sender msg;
	Lwt.return ()
      ) else (
	let username = ((msg.to').Jlib.luser : Jlib.nodepreped :> string) in
	let from = msg.from in
	let to' = msg.to' in
	let `XmlElement (name, attrs, els) = msg.packet in
	let attrs2 = Jlib.replace_from_to_attrs
	  (Jlib.jid_to_string from) (Jlib.jid_to_string to') attrs in
	let timestamp_els =
	  [Jlib.timestamp_to_xml msg.timestamp Jlib.UTC
	     (Jlib.make_jid_exn "" (host:>string) "") "Offline Storage";
	   Jlib.timestamp_to_xml' (Unix.gmtime msg.timestamp)] in
	let packet = `XmlElement
	  (name, attrs2, els @ (timestamp_els :> Xml.element_cdata list)) in
	let xml = Xml.element_to_string packet in
	let query =
	  <:sql<
	    insert into spool(username, xml)
	    values (%(username)s, %(xml)s)
	  >>
	in
	lwt _ = Sql.transaction host (fun () -> Sql.query_t query) in
	  Lwt.return ()
      )

  let rec find_x_event_chatstates els' (a, b, c) =
    match els' with
      | [] ->
	  (a, b, c)
      | (`XmlCdata _) :: els ->
	  find_x_event_chatstates els (a, b, c)
      | ((`XmlElement _) as el) :: els -> (
	  match Xml.get_tag_attr_s "xmlns" el with
	    | <:ns<EVENT>> ->
	      find_x_event_chatstates els (el, b, c)
	    | <:ns<CHATSTATES>> ->
	      find_x_event_chatstates els (a, el, c)
	    | _ ->
		find_x_event_chatstates els (a, b, `True))

  (* Check if the packet has any content about XEP-0022 or XEP-0085 *)
  let check_event_chatstates from to' packet =
    let `XmlElement (name, attrs, els) = packet in
      match find_x_event_chatstates els (`False, `False, `False) with
	| `False, `False, _ ->
	    (* There wasn't any x:event or chatstates subelements *)
	    true
	| `False, cel, `True when cel <> `False ->
	    (* There a chatstates subelement and other stuff, but no x:event *)
	    true
	| `False, cel, `False when cel <> `False ->
	    (* Don't allow offline storage *)
	    false
	| ((`XmlElement _) as el), _, _ -> (
	    match Xml.get_subtag el "id" with
	      | None -> (
		  match Xml.get_subtag el "offline" with
		    | None ->
			true
		    | Some _ ->
			let id = match Xml.get_tag_attr_s "id" packet with
			  | "" ->
			      `XmlElement ("id", [], [])
			  | s ->
			      `XmlElement ("id", [], [`XmlCdata s])
			in
			  Router.route to' from
			    (`XmlElement (name, attrs,
					  [`XmlElement
					     ("x",
					      [("xmlns", <:ns<EVENT>>)],
					      [id;
					       `XmlElement ("offline", [], [])])]));
			  true)
	      | Some _ ->
		  false)

  let rec find_x_expire timestamp els' =
    match els' with
      | [] ->
	  never
      | (`XmlCdata _) :: els ->
	  find_x_expire timestamp els
      | ((`XmlElement _) as el) :: els -> (
	  match Xml.get_tag_attr_s "xmlns" el with
	    | <:ns<EXPIRE>> ->
	      let val' = Xml.get_tag_attr_s "seconds" el in (
		  try
		    let int = int_of_string val' in
		      if int > 0 then (float_of_int int)
		      else never
		  with
		    | _ ->
			never)
	    | _ ->
		find_x_expire timestamp els)

  let store_packet (from, to', packet) =
    let type' = Xml.get_tag_attr_s "type" packet in
      if (type' <> "error"
	  && type' <> "groupchat"
	  && type' <> "headline") then (
	match check_event_chatstates from to' packet with
	  | true ->
	      let luser = to'.Jlib.luser in
	      let timestamp = Unix.time () in
	      let `XmlElement (_name, _attrs, els) = packet in
	      let expire = find_x_expire timestamp els in
	      lwt _ = store_packet' {user = luser;
				     timestamp = timestamp;
				     expire = expire;
				     from = from;
				     to' = to';
				     packet = packet} in
		Lwt.return (Hooks.Stop)
	  | false ->
	      Lwt.return (Hooks.OK)
      ) else
	Lwt.return (Hooks.OK)

  let get_and_del_spool_msg_t luser =
    let euser = (luser : Jlib.nodepreped :> string) in
    let select_query =
      <:sql<
	select @(username)s, @(xml)s from spool
	  where username = %(euser)s order by sec
      >> in
    let delete_query =
      <:sql<delete from spool where username = %(euser)s>>
    in
    lwt res = Sql.query_t select_query in
    lwt _ = Sql.query_t delete_query in
      Lwt.return res

  let pop_offline_messages ls (luser, lserver) =
    try_lwt
      lwt rs = Sql.transaction lserver
	(fun () -> get_and_del_spool_msg_t luser) in
      lwt route_msgs =
	Lwt_list.fold_right_s
	  (fun (_, xml) acc ->
	     try
	       let el = Xml.parse_element xml in
	       let to' = Jlib.string_to_jid_exn (Xml.get_tag_attr_s "to" el) in
	       let from = Jlib.string_to_jid_exn (Xml.get_tag_attr_s "from" el) in
		 Lwt.return ((`Route (from, to', el)) :: acc)
	     with
	       | _ ->
		   Lwt.return acc
	  ) rs [] in
	Lwt.return (Hooks.OK, ls @ route_msgs)
    with
      | _ ->
	  Lwt.return (Hooks.OK, ls)

  let remove_user_h (luser, lserver) =
    let username = (luser : Jlib.nodepreped :> string) in
    let delete_query =
      <:sql<
	delete from spool where username = %(username)s
      >>
    in
    lwt _ = Sql.query lserver delete_query in
      Lwt.return (Hooks.OK)

  let get_sm_features acc (_from, _to, node, _lang) =
    match node with
      | "" ->
	  let feats =
	    match acc with
	      | `Result i -> i
	      | _ -> []
	  in
	    Lwt.return (Hooks.OK, `Result (feats @ [ <:ns<FEATURE_MSGOFFLINE>>]))
      | <:ns<FEATURE_MSGOFFLINE>> ->
	(* override all lesser features... *)
	Lwt.return (Hooks.OK, `Result [])
      | _ ->
	  Lwt.return (Hooks.OK, acc)

  let start host =
    (* ejabberd_hooks:add(webadmin_page_host, Host,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host,
		       ?MODULE, webadmin_user, 50),
    ejabberd_hooks:add(webadmin_user_parse_query, Host,
                       ?MODULE, webadmin_user_parse_query, 50),
    AccessMaxOfflineMsgs = gen_mod:get_opt(access_max_user_messages, Opts, max_user_offline_messages), *)
    Lwt.return (
      [Gen_mod.hook offline_message_hook host store_packet 50;
       Gen_mod.fold_hook resend_offline_messages_hook host pop_offline_messages 50;
       Gen_mod.hook remove_user host remove_user_h 50;
       Gen_mod.hook anonymous_purge_hook host remove_user_h 50;
       Gen_mod.fold_hook disco_sm_features host get_sm_features 50;
       Gen_mod.fold_hook disco_local_features host get_sm_features 50;
      ]
    )

  let stop _host =
    Lwt.return ()

end

let () = Gen_mod.register_mod (module ModOfflineSQL : Gen_mod.Module)

(* WEBADMIN stuff *)
(*
webadmin_page(_, Host,
	      #request{us = _US,
		       path = ["user", U, "queue"],
		       q = Query,
		       lang = Lang} = _Request) ->
    Res = user_queue(U, Host, Query, Lang),
    {stop, Res};

webadmin_page(Acc, _, _) -> Acc.

user_queue(User, Server, Query, Lang) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Username = ejabberd_odbc:escape(LUser),
    US = {LUser, LServer},
    Res = user_queue_parse_query(Username, LServer, Query),
    MsgsAll = case catch ejabberd_odbc:sql_query(
			LServer,
			["select username, xml from spool"
			 "  where username='", Username, "'"
			 "  order by seq;"]) of
	       {selected, ["username", "xml"], Rs} ->
		   lists:flatmap(
		     fun({_, XML}) ->
			     case xml_stream:parse_element(XML) of
				 {error, _Reason} ->
				     [];
				 El ->
				     [El]
			     end
		     end, Rs);
	       _ ->
		   []
	   end,
    Msgs = get_messages_subset(User, Server, MsgsAll),
    FMsgs =
	lists:map(
	  fun({xmlelement, _Name, _Attrs, _Els} = Msg) ->
		  ID = jlib:encode_base64(binary_to_list(term_to_binary(Msg))),
		  Packet = Msg,
		  FPacket = ejabberd_web_admin:pretty_print_xml(Packet),
		  ?XE("tr",
		      [?XAE("td", [{"class", "valign"}], [?INPUT("checkbox", "selected", ID)]),
		       ?XAE("td", [{"class", "valign"}], [?XC("pre", FPacket)])]
		     )
	  end, Msgs),
    [?XC("h1", io_lib:format(?T("~s's Offline Messages Queue"),
			     [us_to_list(US)]))] ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      [?XE("table",
		   [?XE("thead",
			[?XE("tr",
			     [?X("td"),
			      ?XCT("td", "Packet")
			     ])]),
		    ?XE("tbody",
			if
			    FMsgs == [] ->
				[?XE("tr",
				     [?XAC("td", [{"colspan", "4"}], " ")]
				    )];
			    true ->
				FMsgs
			end
		       )]),
	       ?BR,
	       ?INPUTT("submit", "delete", "Delete Selected")
	      ])].

user_queue_parse_query(Username, LServer, Query) ->
    case lists:keysearch("delete", 1, Query) of
	{value, _} ->
	    Msgs = case catch ejabberd_odbc:sql_query(
				LServer,
				["select xml, seq from spool"
				 "  where username='", Username, "'"
				 "  order by seq;"]) of
		       {selected, ["xml", "seq"], Rs} ->
			   lists:flatmap(
			     fun({XML, Seq}) ->
				     case xml_stream:parse_element(XML) of
					 {error, _Reason} ->
					     [];
					 El ->
					     [{El, Seq}]
				     end
			     end, Rs);
		       _ ->
			   []
		   end,
	    F = fun() ->
			lists:foreach(
			  fun({Msg, Seq}) ->
				  ID = jlib:encode_base64(
					 binary_to_list(term_to_binary(Msg))),
				  case lists:member({"selected", ID}, Query) of
				      true ->
					  SSeq = ejabberd_odbc:escape(Seq),
					  catch ejabberd_odbc:sql_query(
						  LServer,
						  ["delete from spool"
						   "  where username='", Username, "'"
						   "  and seq='", SSeq, "';"]);
				      false ->
					  ok
				  end
			  end, Msgs)
		end,
	    mnesia:transaction(F),
	    ok;
	false ->
	    nothing
    end.

us_to_list({User, Server}) ->
    jlib:jid_to_string({User, Server, ""}).

get_queue_length(Username, LServer) ->
    case catch ejabberd_odbc:sql_query(
			    LServer,
			    ["select count(*) from spool"
			     "  where username='", Username, "';"]) of
		   {selected, [_], [{SCount}]} ->
		       SCount;
		   _ ->
		       0
	       end.

get_messages_subset(User, Host, MsgsAll) ->
    Access = gen_mod:get_module_opt(Host, ?MODULE, access_max_user_messages,
				    max_user_offline_messages),
    MaxOfflineMsgs = case get_max_user_messages(Access, User, Host) of
			 Number when is_integer(Number) -> Number;
			 _ -> 100
		     end,
    Length = length(MsgsAll),
    get_messages_subset2(MaxOfflineMsgs, Length, MsgsAll).

get_messages_subset2(Max, Length, MsgsAll) when Length =< Max*2 ->
    MsgsAll;
get_messages_subset2(Max, Length, MsgsAll) ->
    FirstN = Max,
    {MsgsFirstN, Msgs2} = lists:split(FirstN, MsgsAll),
    MsgsLastN = lists:nthtail(Length - FirstN - FirstN, Msgs2),
    IntermediateMsg = {xmlelement, "...", [], []},
    MsgsFirstN ++ [IntermediateMsg] ++ MsgsLastN.

webadmin_user(Acc, User, Server, Lang) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Username = ejabberd_odbc:escape(LUser),
    QueueLen = get_queue_length(Username, LServer),
    FQueueLen = [?AC("queue/", QueueLen)],
    Acc ++ [?XCT("h3", "Offline Messages:")] ++ FQueueLen ++ [?C(" "), ?INPUTT("submit", "removealloffline", "Remove All Offline Messages")].

webadmin_user_parse_query(_, "removealloffline", User, Server, _Query) ->
    case catch odbc_queries:del_spool_msg(Server, User) of
         {'EXIT', Reason} ->
            ?ERROR_MSG("Failed to remove offline messages: ~p", [Reason]),
            {stop, error};
         {error, Reason} ->
            ?ERROR_MSG("Failed to remove offline messages: ~p", [Reason]),
            {stop, error};
         _ ->
            ?INFO_MSG("Removed all offline messages for ~s@~s", [User, Server]),
            {stop, ok}
    end;
webadmin_user_parse_query(Acc, _Action, _User, _Server, _Query) ->
    Acc.
*)
