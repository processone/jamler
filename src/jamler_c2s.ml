open Process

module XMLReceiver = Jamler_receiver
module GenServer = Gen_server
module LJID = Jlib.LJID
module LJIDSet = Jlib.LJIDSet
module Hooks = Jamler_hooks
module Auth = Jamler_auth
module SASL = Jamler_sasl
module Router = Jamler_router
module SM = Jamler_sm

module C2S :
sig
  type msg =
      [ Tcp.msg | XMLReceiver.msg | GenServer.msg
      | SM.msg | `Zxc of string * int ]
  type init_data = Lwt_unix.file_descr
  type state
  val init : init_data -> msg pid -> state
  val handle : msg -> state -> state GenServer.result
  val terminate : state -> unit Lwt.t

  val roster_get_subscription_lists :
    (Jlib.nodepreped * Jlib.namepreped, LJID.t list * LJID.t list)
    Hooks.fold_hook
  val roster_out_subscription :
    (Jlib.nodepreped * Jlib.namepreped * Jlib.jid *
       [ `Subscribe | `Subscribed | `Unsubscribe | `Unsubscribed ])
    Hooks.hook
end =
struct
  type msg =
      [ Tcp.msg | XMLReceiver.msg | GenServer.msg
      | SM.msg | `Zxc of string * int ]

  type c2s_state =
    | Wait_for_stream
    | Wait_for_auth
    | Wait_for_feature_request
    | Wait_for_bind
    | Wait_for_session
    | Wait_for_sasl_response of SASL.t
    | Session_established

  type state =
      {pid : msg pid;
       socket : Tcp.socket;
       xml_receiver : XMLReceiver.t;
       state : c2s_state;
       streamid : string;
       (*sasl_state : int;	(* TODO *)
       access,
       shaper,
       zlib = false,
       tls = false,
       tls_required = false,
       tls_enabled = false,
       tls_options = [],*)
       authenticated : bool;
       user : Jlib.nodepreped;
       server : Jlib.namepreped;
       resource : Jlib.resourcepreped;
       jid : Jlib.jid;
       sid : SM.sid;
       pres_t : LJIDSet.t;
       pres_f : LJIDSet.t;
       pres_a : LJIDSet.t;
       pres_i : LJIDSet.t;
       pres_last : Xml.element option;
       (*pres_pri,*)
       pres_timestamp : float;
       pres_invis : bool;
       (*privacy_list = #userlist{},
       conn = unknown,
       auth_module = unknown,*)
       ip : Unix.inet_addr;
       (*redirect = false,
       aux_fields = [],
       fsm_limit_opts,*)
       lang : string;
      }

  type init_data = Lwt_unix.file_descr

  let init socket self =
    let socket = Tcp.of_fd socket self in
    let xml_receiver = XMLReceiver.create self in
    let state = {pid = self;
		 socket;
		 xml_receiver;
		 state = Wait_for_stream;
		 streamid = "";
		 authenticated = false;
		 user = Jlib.nodeprep_exn "";
		 server = Jlib.nameprep_exn "";
		 resource = Jlib.resourceprep_exn "";
		 jid = Jlib.make_jid_exn "" "" "";
		 sid = (0.0, (self :> SM.msg pid));
		 pres_t = LJIDSet.empty;
		 pres_f = LJIDSet.empty;
		 pres_a = LJIDSet.empty;
		 pres_i = LJIDSet.empty;
		 pres_timestamp = 0.0;
		 pres_invis = false;
		 pres_last = None;
		 ip = Unix.inet_addr_any;	(* TODO *)
		 lang = "";
		}
    in
      Tcp.activate socket self;
      state

  let myname = "localhost"		(* TODO *)
  let is_my_host _server = true		(* TODO *)
  let invalid_ns_err = Jlib.serr_invalid_namespace
  let invalid_xml_err = Jlib.serr_xml_not_well_formed
  let host_unknown_err = Jlib.serr_host_unknown
  let invalid_from = Jlib.serr_invalid_from

  let send_text state text =
    (*Printf.printf "Send XML on stream = %S\n" text; flush stdout;*)
    Tcp.send_async state.socket text

  let send_element state el =
    send_text state (Xml.element_to_string el)

  let send_header state server version lang =
    let version_str =
      match version with
	| "" -> ""
	| _ -> " version='" ^ version ^ "'"
    in
    let lang_str =
      match lang with
	| "" -> ""
	| _ -> " xml:lang='" ^ lang ^ "'"
    in
    let stream_header_fmt =
      "<?xml version='1.0'?>" ^^
	"<stream:stream xmlns='jabber:client' " ^^
	"xmlns:stream='http://etherx.jabber.org/streams' " ^^
	"id='%s' from='%s'%s%s>"
    in
    let header =
      Printf.sprintf stream_header_fmt
	state.streamid
	server
	version_str
	lang_str
    in
      send_text state header

  let send_trailer state =
    let stream_trailer = "</stream:stream>" in
      send_text state stream_trailer

  let rec get_auth_tags (els : Xml.element_cdata list) u p d r =
    match els with
      | `XmlElement (name, _attrs, els) :: l -> (
	  let cdata = Xml.get_cdata els in
	    match name with
	      | "username" -> get_auth_tags l cdata p d r
	      | "password" -> get_auth_tags l u cdata d r
	      | "digest" ->   get_auth_tags l u p cdata r
	      | "resource" -> get_auth_tags l u p d cdata
	      | _ ->          get_auth_tags l u p d r
	)
      | _ :: l ->
	  get_auth_tags l u p d r
      | [] ->
	  (u, p, d, r)

  let is_auth_packet el =
    match Jlib.iq_query_info el with
      | `IQ {Jlib.iq_id = id; Jlib.iq_type = (`Set subel | `Get subel) as type';
	     Jlib.iq_xmlns = <:ns<AUTH>>; _} ->
	  let `XmlElement (_, _, els) = subel in
	  let type' =
	    match type' with
	      | `Set _ -> `Set
	      | `Get _ -> `Get
	  in
	    Some (id, type', get_auth_tags els "" "" "" "")
      | _ ->
	  None

  let new_id () =
    Jlib.get_random_string ()

  let process_unauthenticated_stanza state el =
    let el =
      match Xml.get_tag_attr_s "xml:lang" el with
	| "" -> (
	    match state.lang with
	      | "" -> el
	      | lang -> Xml.replace_tag_attr "xml:lang" lang el
	  )
	| _ -> el
    in
      match Jlib.iq_query_info el with
	| `IQ iq -> (
	    let res = None in		(* TODO *)
	    (*res = ejabberd_hooks:run_fold(c2s_unauthenticated_iq,
					  StateData#state.server,
					  empty,
					  [StateData#state.server, IQ,
					   StateData#state.ip]),
	    *)
	    match res with
	      | None ->
		  (* The only reasonable IQ's here are auth and register IQ's
		     They contain secrets, so don't include subelements to
		     response *)
		  let iq = {iq with
			      Jlib.iq_type = `Error (
				Jlib.err_service_unavailable, None)}
		  in
		  let res =
		    Jlib.replace_from_to
		      (Jlib.make_jid_exn "" (state.server :> string) "")
		      (Jlib.make_jid_exn "" "" "")
		      (Jlib.iq_to_xml iq)
		  in
		  let res = Jlib.remove_attr "to" res in
		    send_element state res
	      | Some res ->
		  send_element state res
	  )
	| _ ->
	    (* Drop any stanza, which isn't IQ stanza *)
	    ()

  let privacy_check_packet state from to' packet dir =
    (* TODO *)
    true
    (*ejabberd_hooks:run_fold(
      privacy_check_packet, StateData#state.server,
      allow,
      [StateData#state.user,
       StateData#state.server,
       StateData#state.privacy_list,
       {From, To, Packet},
       Dir]).
    *)


  let presence_broadcast state from jidset packet =
    LJIDSet.iter
      (fun ljid ->
	 let fjid = Jlib.ljid_to_jid ljid in
	   match privacy_check_packet state from fjid packet `Out with
	     | false -> ()
	     | true ->
		 Router.route from fjid packet
      ) jidset

  let presence_broadcast_to_trusted state from t a packet =
    LJIDSet.iter
      (fun ljid ->
	 if LJIDSet.mem ljid t then (
	   let fjid = Jlib.ljid_to_jid ljid in
	     match privacy_check_packet state from fjid packet `Out with
	       | false -> ()
	       | true ->
		   Router.route from fjid packet
	 )
      ) a


  let presence_broadcast_first from state packet =
    LJIDSet.iter
      (fun ljid ->
	 let fjid = Jlib.ljid_to_jid ljid in
	   Router.route from fjid
	     (`XmlElement ("presence", [("type", "probe")], []))
      ) state.pres_t;
    if state.pres_invis
    then state
    else (
      let pres_a =
	LJIDSet.fold
	  (fun ljid a ->
	     let fjid = Jlib.ljid_to_jid ljid in
	       (match privacy_check_packet state from fjid packet `Out with
		  | false -> ()
		  | true ->
		      Router.route from fjid packet
	       );
	       LJIDSet.add ljid a
	  ) state.pres_f state.pres_a
      in
	{state with pres_a}
    )


  let update_priority priority packet state =
    (*Info = [{ip, StateData#state.ip}, {conn, StateData#state.conn},
	     {auth_module, StateData#state.auth_module}],*)
    let info = [] in
      SM.set_presence
	state.sid state.user state.server state.resource
	priority packet info


  let get_priority_from_presence presence_packet =
    match Xml.get_subtag presence_packet "priority" with
      | None -> 0
      | Some sub_el -> (
	  try
	    int_of_string (Xml.get_tag_cdata sub_el)
	  with
	    | Failure "int_of_string" -> 0
	)


  let resend_offline_messages state =
    (* TODO *)
    ()
    (*case ejabberd_hooks:run_fold(
	   resend_offline_messages_hook, StateData#state.server,
	   [],
	   [StateData#state.user, StateData#state.server]) of
	Rs when is_list(Rs) ->
	    lists:foreach(
	      fun({route,
		   From, To, {xmlelement, _Name, _Attrs, _Els} = Packet}) ->
		      Pass = case privacy_check_packet(StateData, From, To, Packet, in) of
				 allow ->
				     true;
				 deny ->
				     false
			     end,
		      if
			  Pass ->
			      %% Attrs2 = jlib:replace_from_to_attrs(
			      %%		 jlib:jid_to_string(From),
			      %%		 jlib:jid_to_string(To),
			      %%		 Attrs),
			      %% FixedPacket = {xmlelement, Name, Attrs2, Els},
                              %% Use route instead of send_element to go through standard workflow
                              ejabberd_router:route(From, To, Packet);
			      %% send_element(StateData, FixedPacket),
			      %% ejabberd_hooks:run(user_receive_packet,
			      %%			 StateData#state.server,
			      %%			 [StateData#state.jid,
			      %%			  From, To, FixedPacket]);
			  true ->
			      ok
		      end
	      end, Rs)
    end.
    *)

  let resend_subscription_requests ({user = user;
				     server = server; _} as state) =
    (* TODO *)
    ()
    (*PendingSubscriptions = ejabberd_hooks:run_fold(
			     resend_subscription_requests_hook,
			     Server,
			     [],
			     [User, Server]),
    lists:foreach(fun(XMLPacket) ->
			  send_element(StateData,
				       XMLPacket)
		  end,
		  PendingSubscriptions).
    *)


  let check_privacy_route from state from_route to' packet =
    match privacy_check_packet state from to' packet `Out with
      | false ->
	  let lang = state.lang in
	  let err_text = "Your active privacy list has denied the routing of this stanza." in
	  let err =
	    Jlib.make_error_reply packet
	      (Jlib.errt_not_acceptable lang err_text)
	  in
	    Router.route to' from err
      | true ->
	  Router.route from_route to' packet


  let process_presence_probe from to' state =
    let lfrom = Jlib.jid_tolower from in
    let lbfrom =
      Jlib.jid_tolower (Jlib.jid_remove_resource from)
    in
      match state.pres_last with
	| None ->
	    ()
	| Some pres_last ->
	    let cond1 =
	      (not state.pres_invis) &&
		(LJIDSet.mem lfrom state.pres_f ||
		   (lfrom <> lbfrom && LJIDSet.mem lbfrom state.pres_f)) &&
		(not
		   (LJIDSet.mem lfrom state.pres_i ||
		      (lfrom <> lbfrom && LJIDSet.mem lbfrom state.pres_i)))
	    in
	    let cond2 =
	      state.pres_invis &&
		LJIDSet.mem lfrom state.pres_f &&
		LJIDSet.mem lfrom state.pres_a
	    in
	    if cond1 then (
	      let packet = pres_last in
	      let timestamp = state.pres_timestamp in
		    (*Packet1 = maybe_add_delay(Packet, utc, To, "", Timestamp),
		    case ejabberd_hooks:run_fold(
			   privacy_check_packet, StateData#state.server,
			   allow,
			   [StateData#state.user,
			    StateData#state.server,
			    StateData#state.privacy_list,
			    {To, From, Packet1},
			    out]) of
			deny ->
			    ok;
			allow ->
			    Pid=element(2, StateData#state.sid),
			    ejabberd_hooks:run(presence_probe_hook, StateData#state.server, [From, To, Pid]),*)
			    (* Don't route a presence probe to oneself *)
		if lfrom <> Jlib.jid_tolower to' then (
		  Router.route to' from packet
		)
	    ) else if cond2 then (
		    Router.route to' from
		      (`XmlElement ("presence", [], []));
	    ) else ()

  (* User updates his presence (non-directed presence packet) *)
  let presence_update from packet state =
    let `XmlElement (_name, attrs, _els) = packet in
      match Xml.get_attr_s "type" attrs with
	| "unavailable" ->
	    let status =
	      match Xml.get_subtag packet "status" with
		| None -> ""
		| Some status_tag ->
		    Xml.get_tag_cdata status_tag
	    in
	    (*Info = [{ip, StateData#state.ip}, {conn, StateData#state.conn},
		    {auth_module, StateData#state.auth_module}],*)
	    let info = [] in
	      SM.unset_presence
		state.sid state.user state.server state.resource
		status info;
	      presence_broadcast state from state.pres_a packet;
	      presence_broadcast state from state.pres_i packet;
	      {state with
		 pres_last = None;
		 pres_timestamp = 0.0;
		 pres_a = LJIDSet.empty;
		 pres_i = LJIDSet.empty;
		 pres_invis = false}
	| "invisible" ->
	    let new_priority = get_priority_from_presence packet in
	      update_priority new_priority packet state;
	      let state =
		if not state.pres_invis then (
		  presence_broadcast state from state.pres_a packet;
		  presence_broadcast state from state.pres_i packet;
		  let state =
		    {state with
		       pres_last = None;
		       pres_timestamp = 0.0;
		       pres_a = LJIDSet.empty;
		       pres_i = LJIDSet.empty;
		       pres_invis = true}
		  in
		    presence_broadcast_first from state packet
		) else state
	      in
		state
	| "error"
	| "probe"
	| "subscribe"
	| "subscribed"
	| "unsubscribe"
	| "unsubscribed" ->
	    state
	| _ ->
	    let old_priority =
	      match state.pres_last with
		| None -> 0
		| Some old_presence ->
		    get_priority_from_presence old_presence
	    in
	    let new_priority = get_priority_from_presence packet in
	    let timestamp = Unix.gettimeofday () in
	      update_priority new_priority packet state;
	      let from_unavail = state.pres_last = None || state.pres_invis in
		(*?DEBUG("from unavail = ~p~n", [FromUnavail]),*)
	      let state =
                let state = {state with
			       pres_last = Some packet;
                               pres_invis = false;
                               pres_timestamp = timestamp}
		in
		  if from_unavail then (
		    (*ejabberd_hooks:run(user_available_hook,
				       NewStateData#state.server,
				       [NewStateData#state.jid]),*)
		    if new_priority >= 0 then (
		      resend_offline_messages state;
		      resend_subscription_requests state
		    );
		    presence_broadcast_first from state packet
		  ) else (
		    presence_broadcast_to_trusted
		      state from state.pres_f state.pres_a packet;
		    if old_priority < 0 && new_priority >= 0
		    then resend_offline_messages state;
		    state
		  )
	      in
		state


  let roster_out_subscription = Hooks.create ()

  (* User sends a directed presence packet *)
  let presence_track from to' packet state =
    let `XmlElement (_name, attrs, _els) = packet in
    let lto = Jlib.jid_tolower to' in
    let user = state.user in
    let server = state.server in
      match Xml.get_attr_s "type" attrs with
	| "unavailable" ->
	    check_privacy_route from state from to' packet;
	    let pres_i = LJIDSet.remove lto state.pres_i in
	    let pres_a = LJIDSet.remove lto state.pres_a in
	      {state with pres_i; pres_a}
	| "invisible" ->
	    check_privacy_route from state from to' packet;
	    let pres_i = LJIDSet.add lto state.pres_i in
	    let pres_a = LJIDSet.remove lto state.pres_a in
	      {state with pres_i; pres_a}
	| "subscribe" ->
	    Hooks.run roster_out_subscription server
	      (user, server, to', `Subscribe);
	    check_privacy_route from state (Jlib.jid_remove_resource from)
	      to' packet;
	    state
	| "subscribed" ->
	    Hooks.run roster_out_subscription server
	      (user, server, to', `Subscribed);
	    check_privacy_route from state (Jlib.jid_remove_resource from)
	      to' packet;
	    state
	| "unsubscribe" ->
	    Hooks.run roster_out_subscription server
	      (user, server, to', `Unsubscribe);
	    check_privacy_route from state (Jlib.jid_remove_resource from)
	      to' packet;
	    state
	| "unsubscribed" ->
	    Hooks.run roster_out_subscription server
	      (user, server, to', `Unsubscribed);
	    check_privacy_route from state (Jlib.jid_remove_resource from)
	      to' packet;
	    state
	| "error"
	| "probe" ->
	    check_privacy_route from state from to' packet;
	    state
	| _ ->
	    check_privacy_route from state from to' packet;
	    let pres_i = LJIDSet.remove lto state.pres_i in
	    let pres_a = LJIDSet.add lto state.pres_a in
	      {state with pres_i; pres_a}

  let roster_get_subscription_lists = Hooks.create_fold ()

  (* Check from attributes
     returns invalid-from|NewElement *)
  let check_from el from_jid =
    match Xml.get_tag_attr "from" el with
      | None -> true
      | Some sjid -> (
	  let jid' = Jlib.string_to_jid sjid in
	    match jid' with
	      | None -> false
	      | Some jid ->
		    if jid.Jlib.luser = from_jid.Jlib.luser &&
		      jid.Jlib.lserver = from_jid.Jlib.lserver &&
		      jid.Jlib.lresource = from_jid.Jlib.lresource
		    then true
		    else if jid.Jlib.luser = from_jid.Jlib.luser &&
		      jid.Jlib.lserver = from_jid.Jlib.lserver &&
		      (jid.Jlib.lresource :> string) = ""
		    then true
		    else false
	)


  let fsm_next_state state =
    (* TODO *)
    Lwt.return (`Continue state)

  let maybe_migrate state =
    (*PackedStateData = pack(StateData),*)
    let {user = u; server = s; resource = r; sid = sid; _} = state in
    (*case ejabberd_cluster:get_node({jlib:nodeprep(U), jlib:nameprep(S)}) of
	Node when Node == node() ->*)
	    (*Conn = get_conn_type(StateData),
	    Info = [{ip, StateData#state.ip}, {conn, Conn},
		    {auth_module, StateData#state.auth_module}],*)
    let info = [] in
    let presence = state.pres_last in
    let priority =
      match presence with
        | None -> -1
        | Some presence -> get_priority_from_presence presence
    in
      SM.open_session sid u s r priority info;
      fsm_next_state state
	(*Node ->
	    fsm_migrate(StateName, PackedStateData, Node, 0)
    end.*)


  let wait_for_stream msg state =
    match msg with
      | `XmlStreamStart (_name, attrs) -> (
	  let default_lang = "en" in	(* TODO *)
	    match Xml.get_attr_s "xmlns:stream" attrs with
	      | <:ns<STREAM>> -> (
		  let server = Jlib.nameprep (Xml.get_attr_s "to" attrs) in
		    match server with
		      | Some server when is_my_host server -> (
			  let lang =
			    let lang = Xml.get_attr_s "xml:lang" attrs in
			      if String.length lang <= 35 then (
				(* As stated in BCP47, 4.4.1:
				   Protocols or specifications that
				   specify limited buffer sizes for
				   language tags MUST allow for
				   language tags of at least 35 characters. *)
				lang
			      ) else (
				(* Do not store long language tag to
				   avoid possible DoS/flood attacks *)
				""
			      )
			  in
			    (*change_shaper(StateData, jlib:make_jid("", Server, "")),*)
			    match Xml.get_attr_s "version" attrs with
			      | "1.0" -> (
				  send_header state (server :> string) "1.0" default_lang;
				  if not state.authenticated then (
				    (*SASLState =
				      cyrsasl:server_new(
				      "jabber", Server, "", [],
				      fun(U) ->
				      ejabberd_auth:get_password_with_authmodule(
				      U, Server)
				      end,
				      fun(U, P) ->
				      ejabberd_auth:check_password_with_authmodule(
				      U, Server, P)
				      end,
				      fun(U, P, D, DG) ->
				      ejabberd_auth:check_password_with_authmodule(
				      U, Server, P, D, DG)
				      end),*)
				    let mechs =
				      List.map
					(fun s ->
					   `XmlElement ("mechanism", [],
							[`XmlCdata s]))
					(SASL.listmech server)
				    in
				      (*SockMod =
					(StateData#state.sockmod):get_sockmod(
					StateData#state.socket),
					Zlib = StateData#state.zlib,
					CompressFeature =
					case Zlib andalso
					((SockMod == gen_tcp) orelse
					(SockMod == tls)) of
					true ->
					[{xmlelement, "compression",
					[{"xmlns", ?NS_FEATURE_COMPRESS}],
					[{xmlelement, "method",
					[], [{xmlcdata, "zlib"}]}]}];
					_ ->
					[]
					end,
					TLS = StateData#state.tls,
					TLSEnabled = StateData#state.tls_enabled,
					TLSRequired = StateData#state.tls_required,
					TLSFeature =
					case (TLS == true) andalso
					(TLSEnabled == false) andalso
					(SockMod == gen_tcp) of
					true ->
					case TLSRequired of
					true ->
					[{xmlelement, "starttls",
					[{"xmlns", ?NS_TLS}],
					[{xmlelement, "required",
					[], []}]}];
					_ ->
					[{xmlelement, "starttls",
					[{"xmlns", ?NS_TLS}], []}]
					end;
					false ->
					[]
					end,
				      *)
				      send_element
					state
					(`XmlElement
					   ("stream:features", [],
					    (*TLSFeature ++
					      CompressFeature ++*)
					    [`XmlElement
					       ("mechanisms",
						[("xmlns", <:ns<SASL>>)],
						mechs)]
					      (*++
						ejabberd_hooks:run_fold(
						c2s_stream_features,
						Server,
						[], [Server])*))
					);
				      Lwt.return (
					`Continue
					  {state with
					     state = Wait_for_feature_request;
					     server = server;
					     (*sasl_state = SASLState,*)
					     lang = lang});
				  ) else (
				    match (state.resource :> string) with
				      | "" ->
					  (* RosterVersioningFeature =
					     ejabberd_hooks:run_fold(
					     roster_get_versioning_feature,
					     Server, [], [Server]),*)
					  let stream_features =
					    [`XmlElement
					       ("bind",
						[("xmlns", <:ns<BIND>>)], []);
					     `XmlElement
					       ("session",
						[("xmlns", <:ns<SESSION>>)], [])]
					      (*++ RosterVersioningFeature
						++ ejabberd_hooks:run_fold(
						c2s_stream_features,
						Server,
						[], [Server])*)
					  in
					    send_element
					      state
					      (`XmlElement
						 ("stream:features", [],
						  stream_features));
					    Lwt.return (
					      `Continue
						{state with
						   state = Wait_for_bind;
						   server = server;
						   lang = lang})
				      | _ ->
					  send_element
					    state
					    (`XmlElement
					       ("stream:features", [], []));
					  Lwt.return (
					    `Continue
					      {state with
						 state = Wait_for_session;
						 server = server;
						 lang = lang})
				  )
				)
			      | _ ->
				  send_header state (server :> string) "" default_lang;
				  (*if
				    (not StateData#state.tls_enabled) and
				    StateData#state.tls_required ->
				    send_element(
				    StateData,
				    ?POLICY_VIOLATION_ERR(
				    Lang,
				    "Use of STARTTLS required")),
				    send_trailer(StateData),
				    {stop, normal, StateData};
				    true ->*)
				  Lwt.return (
				    `Continue
				      {state with
					 state = Wait_for_auth;
					 server = server;
					 lang = lang})
			)
		      | _ -> (
			  send_header state myname "" default_lang;
			  send_element state host_unknown_err;
			  send_trailer state;
			  Lwt.return (`Stop state)
			)
		)
	      | _ ->
		      send_header state myname "" default_lang;
		      send_element state invalid_ns_err;
		      send_trailer state;
		      Lwt.return (`Stop state)
	)
(*
wait_for_stream(timeout, StateData) ->
    {stop, normal, StateData};
*)

      | `XmlStreamElement _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamEnd _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError _ ->
	  send_header state myname "1.0" "";
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)


  let wait_for_auth msg state =
    match msg with
      | `XmlStreamElement el -> (
	  match is_auth_packet el with
	    | Some (_id, `Get, (u, _, _, _)) -> (
		let `XmlElement (name, attrs, _els) =
		  Jlib.make_result_iq_reply el
		in
		let ucdata =
		  match u with
		    | "" -> [];
		    | _ -> [`XmlCdata u]
		in
		let res =
		  match (*ejabberd_auth:plain_password_required(
			  StateData#state.server)*)false with (* TODO *)
		    | false ->
			  `XmlElement
			    (name, attrs,
			     [`XmlElement
				("query", [("xmlns", <:ns<AUTH>>)],
				 [`XmlElement ("username", [], ucdata);
				  `XmlElement ("password", [], []);
				  `XmlElement ("digest", [], []);
				  `XmlElement ("resource", [], [])
				 ])])
		    | true ->
			  `XmlElement
			    (name, attrs,
			     [`XmlElement
				("query", [("xmlns", <:ns<AUTH>>)],
				 [`XmlElement ("username", [], ucdata);
				  `XmlElement ("password", [], []);
				  `XmlElement ("resource", [], [])
				 ])])
		in
		  send_element state res;
		  Lwt.return
		    (`Continue
		       {state with state = Wait_for_auth})
	      )
	    | Some (_id, `Set, (_u, _p, _d, "")) ->
		let err =
		  Jlib.make_error_reply el
		    (Jlib.err_auth_no_resource_provided state.lang)
		in
		  send_element state err;
		  Lwt.return
		    (`Continue
		       {state with state = Wait_for_auth})
	    | Some (_id, `Set, (u, _p, _d, r)) -> (
		match Jlib.make_jid u (state.server :> string) r with
		  | Some jid (*when 
			       (acl:match_rule(StateData#state.server,
			       StateData#state.access, JID) == allow) *) (* TODO *) -> (
		      (*let dgen = fun pw ->
                        Jlib.sha1 (state.streamid ^ pw)
		      in*)
			match (*ejabberd_auth:check_password_with_authmodule(
			  U, StateData#state.server, P, D, DGen) *) (* TODO *)
			  (true, 0)
			with
			  | (true, _auth_module) -> (
			      (*?INFO_MSG(
				"(~w) Accepted legacy authentication for ~s by ~p",
				[StateData#state.socket,
				jlib:jid_to_string(JID), AuthModule]),*)
			      lwt () = Lwt_io.printf "Accepted legacy authentication for %s\n" (Jlib.jid_to_string jid) in
				match (*need_redirect(StateData#state{user = U})*) (* TODO *)
				  None
				with
                                  | Some host -> (
                                    (*?INFO_MSG("(~w) Redirecting ~s to ~s",
                                              [StateData#state.socket,
                                               jlib:jid_to_string(JID), Host]),*)
				      lwt () = Lwt_io.printf "Redirecting %s to %s\n" (Jlib.jid_to_string jid) host in
                                        send_element state
					  (Jlib.serr_see_other_host host);
                                        send_trailer state;
					Lwt.return
					  (`Stop state);
	                            )
	                          | None ->
                                      let sid =
					(Unix.gettimeofday (),
					 (state.pid :> SM.msg pid))
				      in
				      (*Conn = get_conn_type(StateData),*)
                                      let res = Jlib.make_result_iq_reply el in
					(*Res = setelement(4, Res1, []),*)
					send_element state res;
					(*change_shaper(StateData, JID),*)
					lwt (fs, ts) =
					  Hooks.run_fold
					    roster_get_subscription_lists
					    state.server
					    ([], [])
					    (jid.Jlib.luser, state.server)
					in
					let ljid =
					  Jlib.jid_tolower (Jlib.jid_remove_resource jid)
					in
					let fs = ljid :: fs in
					let ts = ljid :: ts in
					  (*PrivList =
                                          ejabberd_hooks:run_fold(
                                          privacy_get_user_list,
                                          StateData#state.server,
                                          #userlist{},
                                          [U, StateData#state.server]),*)
					let state =
                                          {state with
                                             user = jid.Jlib.luser;
                                             resource = jid.Jlib.lresource;
                                             jid = jid;
                                             sid = sid;
                                               (*conn = Conn,
                                               auth_module = AuthModule,
                                               pres_f = ?SETS:from_list(Fs1),
                                               pres_t = ?SETS:from_list(Ts1),
                                               privacy_list = PrivList*)}
					in
					  (*DebugFlag = ejabberd_hooks:run_fold(
                                            c2s_debug_start_hook,
                                            NewStateData#state.server,
                                            false,
                                            [self(), NewStateData]),*)
					  maybe_migrate
					    {state with
					       state = Session_established}
                            )
			  | _ ->
			    (*?INFO_MSG(
			       "(~w) Failed legacy authentication for ~s",
			       [StateData#state.socket,
				jlib:jid_to_string(JID)]),*)
			      lwt () = Lwt_io.printf "Failed legacy authentication for %s\n" (Jlib.jid_to_string jid) in
		              let err =
				Jlib.make_error_reply el Jlib.err_not_authorized
			      in
				send_element state err;
				Lwt.return
				  (`Continue
				     {state with state = Wait_for_auth})
		    )
		  | None ->
		      (*?INFO_MSG(
			"(~w) Forbidden legacy authentication for "
			"username '~s' with resource '~s'",
			[StateData#state.socket, U, R]),*)
		      lwt () = Lwt_io.printf "Forbidden legacy authentication for %s with resource '%s'\n" u r in
		      let err = Jlib.make_error_reply el Jlib.err_jid_malformed
		      in
			send_element state err;
			Lwt.return
			  (`Continue
			     {state with state = Wait_for_auth})
		  | Some jid ->
		      (*?INFO_MSG(
			"(~w) Forbidden legacy authentication for ~s",
			[StateData#state.socket,
			jlib:jid_to_string(JID)]),*)
		      lwt () = Lwt_io.printf "Forbidden legacy authentication for %s\n" (Jlib.jid_to_string jid) in
		      let err = Jlib.make_error_reply el Jlib.err_not_allowed
		      in
			send_element state err;
			Lwt.return
			  (`Continue
			     {state with state = Wait_for_auth})
	      )
	    | None ->
		process_unauthenticated_stanza state el;
		Lwt.return
		  (`Continue
		     {state with state = Wait_for_auth})
	)
(*
wait_for_auth(timeout, StateData) ->
    {stop, normal, StateData};
*)

      | `XmlStreamStart _ -> assert false

      | `XmlStreamEnd _ ->
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)

  let wait_for_feature_request msg state =
    match msg with
      | `XmlStreamElement el -> (
	  let `XmlElement (name, attrs, els) = el in
	    (* Zlib = StateData#state.zlib,
	       TLS = StateData#state.tls,
	       TLSEnabled = StateData#state.tls_enabled,
	       TLSRequired = StateData#state.tls_required,
	       SockMod = (StateData#state.sockmod):get_sockmod(StateData#state.socket),*)
	    match (Xml.get_attr_s "xmlns" attrs), name with
	      | <:ns<SASL>>, "auth" (*when not ((SockMod == gen_tcp) and
				      TLSRequired)*) (* TODO *) -> (
		  let mech = Xml.get_attr_s "mechanism" attrs in
		  let client_in = Jlib.decode_base64 (Xml.get_cdata els) in
		  let sasl_result =
		    SASL.server_start
		      ~service:"jabber"
		      ~server_fqdn:state.server
		      ~user_realm:""
		      ~get_password:
		      (fun u ->
			 Auth.get_password_with_authmodule u state.server
		      )
		      ~check_password:
		      (fun u p ->
			 Auth.check_password_with_authmodule u state.server p
		      )
		      ~check_password_digest:
		      (fun u p d dg ->
			 Auth.check_password_digest_with_authmodule
			   u state.server p d dg
		      )
		      ~mech
		      client_in
		  in
		    match sasl_result with
		      | SASL.Done props -> (
			  (*catch (StateData#state.sockmod):reset_stream(
                            StateData#state.socket),*)
			  XMLReceiver.reset_stream state.xml_receiver;
			  let u = Jlib.nodeprep_exn (List.assoc `Username props) in	(* TODO *)
			    (*AuthModule = xml:get_attr_s(auth_module, Props),*)
			    (*?INFO_MSG("(~w) Accepted authentication for ~s by ~p",
                              [StateData#state.socket, U, AuthModule]),*)
			  lwt () = Lwt_io.printf "Accepted authentication for %s\n" (u :> string) in
                            send_element state
                              (`XmlElement ("success",
                                            [("xmlns", <:ns<SASL>>)], []));
                            Lwt.return
			      (`Continue
				 {state with
				    state = Wait_for_stream;
                                    streamid = new_id ();
                                    authenticated = true;
                                    (*auth_module = AuthModule,*)
                                    user = u})
			)
		      | SASL.Continue (server_out, sasl_mech) ->
			  send_element state
			    (`XmlElement
			       ("challenge",
				[("xmlns", <:ns<SASL>>)],
				[`XmlCdata (Jlib.encode_base64 server_out)]));
			  Lwt.return
			    (`Continue
			       {state with
				  state = Wait_for_sasl_response sasl_mech})
		      | SASL.ErrorUser (error, username) ->
			  (*?INFO_MSG(
			    "(~w) Failed authentication for ~s@~s",
			    [StateData#state.socket,
			    Username, StateData#state.server]),*)
			  lwt () = Lwt_io.printf "Failed authentication for %s@%s\n" username (state.server :> string) in
			    send_element state
			      (`XmlElement
				 ("failure",
				  [("xmlns", <:ns<SASL>>)],
				  [`XmlElement (error, [], [])]));
			    Lwt.return (
			      `Continue
				{state with
				   state = Wait_for_feature_request})
		      | SASL.Error error ->
			  send_element state
			    (`XmlElement
			       ("failure",
				[("xmlns", <:ns<SASL>>)],
				[`XmlElement (error, [], [])]));
			  Lwt.return (
			    `Continue
			      {state with
				 state = Wait_for_feature_request})
		)
		(* | <:ns<TLS>>, "starttls" (*when TLS == true,
					 TLSEnabled == false,
					 SockMod == gen_tcp*) (* TODO *)
		  ->
	    TLSOpts = case ejabberd_config:get_local_option(
			     {domain_certfile, StateData#state.server}) of
			  undefined ->
			      StateData#state.tls_options;
			  CertFile ->
			      [{certfile, CertFile} |
			       lists:keydelete(
				 certfile, 1, StateData#state.tls_options)]
		      end,
	    Socket = StateData#state.socket,
	    TLSSocket = (StateData#state.sockmod):starttls(
			  Socket, TLSOpts,
			  xml:element_to_binary(
			    {xmlelement, "proceed", [{"xmlns", ?NS_TLS}], []})),
	    fsm_next_state(wait_for_stream,
			   StateData#state{socket = TLSSocket,
					   streamid = new_id(),
					   tls_enabled = true
					  });
	{?NS_COMPRESS, "compress"} when Zlib == true,
					((SockMod == gen_tcp) or
					 (SockMod == tls)) ->
	    case xml:get_subtag(El, "method") of
		false ->
		    send_element(StateData,
				 {xmlelement, "failure",
				  [{"xmlns", ?NS_COMPRESS}],
				  [{xmlelement, "setup-failed", [], []}]}),
		    fsm_next_state(wait_for_feature_request, StateData);
		Method ->
		    case xml:get_tag_cdata(Method) of
			"zlib" ->
			    Socket = StateData#state.socket,
			    ZlibSocket = (StateData#state.sockmod):compress(
					   Socket,
					   xml:element_to_binary(
					     {xmlelement, "compressed",
					      [{"xmlns", ?NS_COMPRESS}], []})),
			    fsm_next_state(wait_for_stream,
			     StateData#state{socket = ZlibSocket,
					     streamid = new_id()
					    });
			_ ->
			    send_element(StateData,
					 {xmlelement, "failure",
					  [{"xmlns", ?NS_COMPRESS}],
					  [{xmlelement, "unsupported-method",
					    [], []}]}),
			    fsm_next_state(wait_for_feature_request,
					   StateData)
		    end
	    end;
*)
	      | _ ->
		  (* TODO *)
	    (*if
		(SockMod == gen_tcp) and TLSRequired ->
		    Lang = StateData#state.lang,
		    send_element(StateData, ?POLICY_VIOLATION_ERR(
					       Lang,
					       "Use of STARTTLS required")),
		    send_trailer(StateData),
		    {stop, normal, StateData};
		true ->*)
		  process_unauthenticated_stanza state el;
		  Lwt.return (
		    `Continue
		      {state with
			 state = Wait_for_feature_request})
	)

(*
wait_for_feature_request(timeout, StateData) ->
    {stop, normal, StateData};
*)

      | `XmlStreamStart _ -> assert false

      | `XmlStreamEnd _ ->
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)


  let wait_for_sasl_response msg sasl_state state =
    match msg with
      | `XmlStreamElement el -> (
	  let `XmlElement (name, attrs, els) = el in
	    match Xml.get_attr_s "xmlns" attrs, name with
	      | <:ns<SASL>>, "response" -> (
		  let client_in = Jlib.decode_base64 (Xml.get_cdata els) in
		    match SASL.server_step sasl_state client_in with
		      | SASL.Done props -> (
			  (*catch (StateData#state.sockmod):reset_stream(
                            StateData#state.socket),*)
			  XMLReceiver.reset_stream state.xml_receiver;
			  let u = Jlib.nodeprep_exn (List.assoc `Username props) in	(* TODO *)
			    (*AuthModule = xml:get_attr_s(auth_module, Props),
			      ?INFO_MSG("(~w) Accepted authentication for ~s by ~p",
			      [StateData#state.socket, U, AuthModule]),*)
			  lwt () = Lwt_io.printf "Accepted authentication for %s\n" (u :> string) in
                            send_element state
                              (`XmlElement ("success",
                                            [("xmlns", <:ns<SASL>>)], []));
                            Lwt.return
			      (`Continue
				 {state with
				    state = Wait_for_stream;
                                    streamid = new_id ();
                                    authenticated = true;
                                    (*auth_module = AuthModule,*)
                                    user = u})
			)
		      | SASL.Continue (server_out, sasl_mech) ->
			  send_element state
			    (`XmlElement
			       ("challenge",
				[("xmlns", <:ns<SASL>>)],
				[`XmlCdata (Jlib.encode_base64 server_out)]));
			  Lwt.return
			    (`Continue
			       {state with
				  state = Wait_for_sasl_response sasl_mech})
		      | SASL.ErrorUser (error, username) ->
			  (*?INFO_MSG(
			    "(~w) Failed authentication for ~s@~s",
			    [StateData#state.socket,
			    Username, StateData#state.server]),*)
			  lwt () = Lwt_io.printf "Failed authentication for %s@%s\n" username (state.server :> string) in
			    send_element state
			      (`XmlElement
				 ("failure",
				  [("xmlns", <:ns<SASL>>)],
				  [`XmlElement (error, [], [])]));
			    Lwt.return (
			      `Continue
				{state with
				   state = Wait_for_feature_request})
		      | SASL.Error error ->
			  send_element state
			    (`XmlElement
			       ("failure",
				[("xmlns", <:ns<SASL>>)],
				[`XmlElement (error, [], [])]));
			  Lwt.return (
			    `Continue
			      {state with
				 state = Wait_for_feature_request})
		)
	      | _ ->
		process_unauthenticated_stanza state el;
		Lwt.return
		  (`Continue
		     {state with state = Wait_for_feature_request})
	)

(*
wait_for_sasl_response(timeout, StateData) ->
    {stop, normal, StateData};
*)

      | `XmlStreamStart _ -> assert false

      | `XmlStreamEnd _ ->
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)


  let wait_for_bind msg state =
    match msg with
      | `XmlStreamElement el -> (
	  match Jlib.iq_query_info el with
	    | `IQ ({Jlib.iq_type = `Set sub_el;
		    Jlib.iq_xmlns = <:ns<BIND>>; _} as iq) -> (
		let u = state.user in
		let r = Xml.get_path_s sub_el [`Elem "resource"; `Cdata] in
		let r =
		  match Jlib.resourceprep r with
		    | None -> None;
		    | Some resource when (resource :> string) = "" ->
			Some (Jlib.resourceprep_exn
				(Jlib.get_random_string () ^
				   string_of_int (int_of_float (Unix.time ())))
			     )
		    | Some resource -> Some resource
		in
		  match r with
		    | None ->
			let err =
			  Jlib.make_error_reply el Jlib.err_bad_request
			in
			  send_element state err;
			  Lwt.return
			    (`Continue
			       {state with state = Wait_for_bind})
		    | Some r ->
			let jid = Jlib.make_jid' u state.server r in
			let res =
			  {iq with
			     Jlib.iq_type =
			      `Result
				(Some (`XmlElement
					 ("bind",
					  [("xmlns", <:ns<BIND>>)],
					  [`XmlElement
					     ("jid", [],
					      [`XmlCdata
						 (Jlib.jid_to_string jid)])])
				      ))}
			in
			  send_element state (Jlib.iq_to_xml res);
			  Lwt.return
			    (`Continue {state with
					  state = Wait_for_session;
					  resource = r;
					  jid = jid})
	      )
	    | _ ->
		Lwt.return (`Continue {state with state = Wait_for_bind})
	)

(*
wait_for_bind(timeout, StateData) ->
    {stop, normal, StateData};
*)

      | `XmlStreamStart _ -> assert false

      | `XmlStreamEnd _ ->
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)

  let wait_for_session msg state =
    match msg with
      | `XmlStreamElement el -> (
	  match Jlib.iq_query_info el with
	    | `IQ ({Jlib.iq_type = `Set sub_el;
		    Jlib.iq_xmlns = <:ns<SESSION>>; _} as iq) -> (
		let u = state.user in
		let jid = state.jid in
		  match (*acl:match_rule(StateData#state.server,
				       StateData#state.access, JID)*)true (* TODO *) with
		    | true ->
			(*?INFO_MSG("(~w) Opened session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(JID)]),*)
		      lwt () = Lwt_io.printf "Opened session for %s\n" (Jlib.jid_to_string jid) in
		      let res = Jlib.make_result_iq_reply el in
			send_element state res;
		    (*change_shaper(StateData, JID),*)
			lwt (fs, ts) =
			  Hooks.run_fold
			    roster_get_subscription_lists
			    state.server
			    ([], [])
			    (u, state.server)
			in
			let ljid =
			  Jlib.jid_tolower (Jlib.jid_remove_resource jid)
			in
			let fs = ljid :: fs in
			let ts = ljid :: ts in
		    (*PrivList =
			ejabberd_hooks:run_fold(
			  privacy_get_user_list, StateData#state.server,
			  #userlist{},
			  [U, StateData#state.server]),*)
                        let sid =
			  (Unix.gettimeofday (), (state.pid :> SM.msg pid))
			in
		    (*Conn = get_conn_type(StateData),
		    %% Info = [{ip, StateData#state.ip}, {conn, Conn},
		    %% 	    {auth_module, StateData#state.auth_module}],
		    %% ejabberd_sm:open_session(
		    %%   SID, U, StateData#state.server, R, Info),
		    *)
			let state =
                          {state with
			     sid = sid;
			       (*conn = Conn;*)
			     pres_f = LJIDSet.from_list fs;
			     pres_t = LJIDSet.from_list ts;
			       (*privacy_list = PrivList*)}
			in
		    (*DebugFlag = ejabberd_hooks:run_fold(c2s_debug_start_hook,
							NewStateData#state.server,
							false,
							[self(), NewStateData]),*)
			  maybe_migrate
			    {state with state = Session_established}
		    | _ ->
		    (*ejabberd_hooks:run(forbidden_session_hook,
				       StateData#state.server, [JID]),
		    ?INFO_MSG("(~w) Forbidden session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(JID)]),*)
			lwt () = Lwt_io.printf "Forbidden session for %s\n" (Jlib.jid_to_string jid) in
			let err = Jlib.make_error_reply el Jlib.err_not_allowed
			in
			  send_element state err;
			  Lwt.return
			    (`Continue
			       {state with state = Wait_for_session})
	      )
	    | _ ->
		Lwt.return
		  (`Continue
		     {state with state = Wait_for_session})
	)
(*
wait_for_session(timeout, StateData) ->
    {stop, normal, StateData};
*)

      | `XmlStreamStart _ -> assert false

      | `XmlStreamEnd _ ->
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)


(* Process packets sent by user (coming from user on c2s XMPP
   connection)
*)
  let session_established2 el state =
    let `XmlElement (name, attrs, _els) = el in
    let user = state.user in
    let server = state.server in
    let from_jid = state.jid in
    let to' = Xml.get_attr_s "to" attrs in
    let to_jid =
      match to' with
	| "" ->
	    Some (Jlib.jid_remove_resource state.jid)
	| _ ->
	    Jlib.string_to_jid to'
    in
    let el = Jlib.remove_attr "xmlns" el in
    let el =
      match Xml.get_attr_s "xml:lang" attrs with
	| "" -> (
	    match state.lang with
	      | "" -> el
	      | lang -> Xml.replace_tag_attr "xml:lang" lang el
	  )
	| _ -> el
    in
    let state =
      match to_jid with
	| None -> (
	    match Xml.get_attr_s "type" attrs with
	      | "error"
	      | "result" -> state
	      | _ ->
		  let err = Jlib.make_error_reply el Jlib.err_jid_malformed in
	  	    send_element state err;
		    state
	  )
	| Some to_jid -> (
	    match name with
	      | "presence" -> (
			(*PresenceEl = ejabberd_hooks:run_fold(
				       c2s_update_presence,
				       Server,
				       NewEl,
				       [User, Server]),
			ejabberd_hooks:run(
			  user_send_packet,
			  Server,
			  [StateData#state.debug, FromJID, ToJID, PresenceEl]),*)
		  if to_jid.Jlib.luser = user &&
		    to_jid.Jlib.lserver = server &&
		    (to_jid.Jlib.lresource :> string) = ""
		  then (
		    (*?DEBUG("presence_update(~p,~n\t~p,~n\t~p)",
		      [FromJID, PresenceEl, StateData]),*)
		    presence_update from_jid el state
		  ) else (
		    presence_track from_jid to_jid el state
		  )
		)
	      | "iq" -> (
		  match Jlib.iq_query_info el with
		    (*| #iq{xmlns = ?NS_PRIVACY} = IQ ->
				ejabberd_hooks:run(
				  user_send_packet,
				  Server,
				  [StateData#state.debug, FromJID, ToJID, NewEl]),
				process_privacy_iq(
				  FromJID, ToJID, IQ, StateData);*)
		    | _ ->
				(*ejabberd_hooks:run(
				  user_send_packet,
				  Server,
                                  [StateData#state.debug, FromJID, ToJID, NewEl]),*)
			check_privacy_route from_jid state from_jid to_jid el;
			state
		)
	      | "message" ->
			(*ejabberd_hooks:run(user_send_packet,
					   Server,
					   [StateData#state.debug, FromJID, ToJID, NewEl]),
			*)
		  check_privacy_route from_jid state from_jid to_jid el;
		  state
	      | _ ->
		  state
	  )
    in
      (*ejabberd_hooks:run(c2s_loop_debug, [{xmlstreamelement, El}]),*)
      fsm_next_state state


  let session_established msg state =
    match msg with
      | `XmlStreamElement el -> (
	  let from_jid = state.jid in
	    (* Check 'from' attribute in stanza RFC 3920 Section 9.1.2 *)
	    match check_from el from_jid with
	      | false ->
		  send_element state invalid_from;
		  send_trailer state;
		  Lwt.return (`Stop state)
	      | true ->
		  session_established2 el state
	)

(*
%% We hibernate the process to reduce memory consumption after a
%% configurable activity timeout
session_established(timeout, StateData) ->
    %% TODO: Options must be stored in state:
    Options = [],
    proc_lib:hibernate(?GEN_FSM, enter_loop,
		       [?MODULE, Options, session_established, StateData]),
    fsm_next_state(session_established, StateData);
*)

      | `XmlStreamStart _ -> assert false

      | `XmlStreamEnd _ ->
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError "XML stanza is too big" -> (* TODO *)
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)


  let handle_xml msg state =
    match state.state, msg with
      | Wait_for_stream, _ -> wait_for_stream msg state
      | Wait_for_auth, _ -> wait_for_auth msg state
      | Wait_for_feature_request, _ -> wait_for_feature_request msg state
      | Wait_for_bind, _ -> wait_for_bind msg state
      | Wait_for_session, _ -> wait_for_session msg state
      | Wait_for_sasl_response sasl_state, _ ->
	  wait_for_sasl_response msg sasl_state state
      | Session_established, _ -> session_established msg state
(*      | _, `XmlStreamStart (name, attrs) ->
          lwt () = Lwt_io.printf "stream start: %s %s\n"
            name (Xml.attrs_to_string attrs)
          in
            Lwt.return (`Continue state)
      | _, `XmlStreamElement el ->
          lwt () = Lwt_io.printf "stream el: %s\n"
            (Xml.element_to_string el)
          in
            Lwt.return (`Continue state)
      | _, `XmlStreamEnd name ->
          lwt () = Lwt_io.printf "stream end: %s\n" name in
            Lwt.return (`Continue state)
      | _, `XmlStreamError error ->
          lwt () = Lwt_io.printf "stream error: %s\n" error in
            Lwt.return (`Stop state)
*)

  let handle_route (`Route (from, to', packet)) state =
    let `XmlElement (name, attrs, els) = packet in
    let (pass, attrs, state) =
      match name with
	| "presence" -> (
		(*State = ejabberd_hooks:run_fold(
			  c2s_presence_in, StateData#state.server,
			  StateData,
			  [{From, To, Packet}]),*)
	    match Xml.get_attr_s "type" attrs with
	      | "probe" ->
		  let lfrom = Jlib.jid_tolower from in
		  let lbfrom =
		    Jlib.jid_tolower (Jlib.jid_remove_resource from)
		  in
		  let state =
		    if LJIDSet.mem lfrom state.pres_a ||
		      LJIDSet.mem lbfrom state.pres_a then (
			state
		      ) else (
			if LJIDSet.mem lfrom state.pres_f then (
			  let a = LJIDSet.add lfrom state.pres_a in
			    {state with pres_a = a}
			) else (
			  if LJIDSet.mem lbfrom state.pres_f then (
			    let a = LJIDSet.add lbfrom state.pres_a in
			      {state with pres_a = a}
			  ) else state
			)
		      )
		  in
		    process_presence_probe from to' state;
		    (false, attrs, state)
	      | "error" ->
		  let a = LJIDSet.remove (Jlib.jid_tolower from) state.pres_a
		  in
		    (true, attrs, {state with pres_a = a})
	      | "invisible" ->
		  let attrs = List.remove_assoc "type" attrs in
		    (true, ("type", "unavailable") :: attrs, state)
	      | "subscribe"
	      | "subscribed"
	      | "unsubscribe"
	      | "unsubscribed" ->
		  let sres = privacy_check_packet state from to' packet `In in
		    (sres, attrs, state)
	      | _ -> (
		  match privacy_check_packet state from to' packet `In with
		    | true -> (
			let lfrom = Jlib.jid_tolower from in
			let lbfrom =
			  Jlib.jid_tolower (Jlib.jid_remove_resource from)
			in
		    if LJIDSet.mem lfrom state.pres_a ||
		      LJIDSet.mem lbfrom state.pres_a then (
			(true, attrs, state)
		      ) else (
			if LJIDSet.mem lfrom state.pres_f then (
			  let a = LJIDSet.add lfrom state.pres_a in
			    (true, attrs, {state with pres_a = a})
			) else (
			  if LJIDSet.mem lbfrom state.pres_f then (
			    let a = LJIDSet.add lbfrom state.pres_a in
			      (true, attrs, {state with pres_a = a})
			  ) else (true, attrs, state)
			)
		      )
		      )
		    | false ->
			(false, attrs, state)
		)
	  )
	| "broadcast" -> (
		(*?DEBUG("broadcast~n~p~n", [Els]),
		case Els of
		    [{item, IJID, ISubscription}] ->
			{false, Attrs,
			 roster_change(IJID, ISubscription,
				       StateData)};
		    [{exit, Reason}] ->
			{exit, Attrs, Reason};
		    [{privacy_list, PrivList, PrivListName}] ->
			case ejabberd_hooks:run_fold(
			       privacy_updated_list, StateData#state.server,
			       false,
			       [StateData#state.privacy_list,
				PrivList]) of
			    false ->
				{false, Attrs, StateData};
			    NewPL ->
				PrivPushIQ =
				    #iq{type = set, xmlns = ?NS_PRIVACY,
					id = "push" ++ randoms:get_string(),
					sub_el = [{xmlelement, "query",
						   [{"xmlns", ?NS_PRIVACY}],
						   [{xmlelement, "list",
						     [{"name", PrivListName}],
						     []}]}]},
				PrivPushEl =
				    jlib:replace_from_to(
				      jlib:jid_remove_resource(
					StateData#state.jid),
				      StateData#state.jid,
				      jlib:iq_to_xml(PrivPushIQ)),
				send_element(StateData, PrivPushEl),
				{false, Attrs, StateData#state{privacy_list = NewPL}}
			end;
		    _ ->*)
	    (false, attrs, state)
	  )
	| "iq" -> (
	    match Jlib.iq_query_info packet with
	      (*| `IQ {Jlib.iq_xmlns = <:ns<LAST>>} ->
			LFrom = jlib:jid_tolower(From),
			LBFrom = jlib:jid_remove_resource(LFrom),
			HasFromSub = (?SETS:is_element(LFrom, StateData#state.pres_f) orelse ?SETS:is_element(LBFrom, StateData#state.pres_f))
			    andalso is_privacy_allow(StateData, To, From, {xmlelement, "presence", [], []}, out),
			case HasFromSub of
			    true ->
				case privacy_check_packet(StateData, From, To, Packet, in) of
				    allow ->
					{true, Attrs, StateData};
				    deny ->
					{false, Attrs, StateData}
				end;
			    _ ->
				Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
				ejabberd_router:route(To, From, Err),
				{false, Attrs, StateData}
			end;
	      *)
	      | (`IQ _ | `Reply) as iq -> (
		  match privacy_check_packet state from to' packet `In, iq with
		    | true, _ ->
			(true, attrs, state)
		    | false, `IQ _ ->
			let err =
			  Jlib.make_error_reply
			    packet Jlib.err_service_unavailable
			in
			  Router.route to' from err;
			  (false, attrs, state)
		    | false, `Reply ->
			(false, attrs, state)
		)
	      | `Invalid
	      | `Not_iq ->
		  (false, attrs, state)
	  )
	| "message" -> (
	    match privacy_check_packet state from to' packet `In with
	      | true -> (
		  (*case ejabberd_hooks:run_fold(
				       feature_check_packet, StateData#state.server,
				       allow,
				       [StateData#state.jid,
					StateData#state.server,
					StateData#state.pres_last,
					{From, To, Packet},
					in]) of
				    allow ->*)
		  (true, attrs, state)
				    (*deny ->
					{false, Attrs, StateData}
				end;*)
		)
	      | false ->
		  (false, attrs, state)
	  )
	| _ ->
	    (true, attrs, state)
    in
      (*if
	Pass == exit ->
	    catch send_trailer(StateData),
	    case NewState of
		rebind ->
		    {stop, normal, StateData#state{authenticated = rebinded}};
		_ ->
		    {stop, normal, StateData}
	    end;*)
      if pass then (
	let attrs =
	  Jlib.replace_from_to_attrs
	    (Jlib.jid_to_string from)
	    (Jlib.jid_to_string to')
	    attrs
	in
	let fixed_packet = `XmlElement (name, attrs, els) in
	  send_element state fixed_packet;
	(*ejabberd_hooks:run(user_receive_packet,
			       StateData#state.server,
			       [StateData#state.debug, StateData#state.jid, From, To, FixedPacket]),
	  ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),*)
	  fsm_next_state state
      ) else (
	(*ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),*)
	fsm_next_state state
      )

  let handle msg state =
    match msg with
      | `Tcp_data (socket, data) when socket == state.socket ->
          (*lwt () = Lwt_io.printf "tcp data %d %S\n" (String.length data) data in*)
            XMLReceiver.parse state.xml_receiver data;
            Tcp.activate state.socket state.pid;
            (*state.pid $! `Zxc (data, 1);*)
            Lwt.return (`Continue state)
      | `Tcp_data (_socket, _data) -> assert false
      | `Tcp_close socket when socket == state.socket ->
          lwt () = Lwt_io.printf "tcp close\n" in
            (*Gc.print_stat stdout;
            Gc.compact ();
            Gc.print_stat stdout; flush stdout;*)
            Lwt.return (`Stop state)
      | `Tcp_close _socket -> assert false
      | #XMLReceiver.msg as m ->
	  handle_xml m state
      | #SM.msg as m ->
	  handle_route m state
      | `Zxc (s, n) ->
          if n <= 1000000 then (
            Tcp.send_async state.socket (string_of_int n ^ s);
            state.pid $! `Zxc (s, n + 1)
          );
          Lwt_main.yield () >>
          Lwt.return (`Continue state)
      | #GenServer.msg -> assert false

  let terminate state =
    XMLReceiver.free state.xml_receiver;
    lwt () =
      if Tcp.state state.socket = Lwt_unix.Opened
      then Tcp.close state.socket
      else Lwt.return ()
    in
      (match state.state with
	 | Session_established -> (
	    (*case StateData#state.authenticated of
		replaced ->
		    ?INFO_MSG("(~w) Replaced session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(StateData#state.jid)]),
		    From = StateData#state.jid,
		    Packet = {xmlelement, "presence",
			      [{"type", "unavailable"}],
			      [{xmlelement, "status", [],
				[{xmlcdata, "Replaced by new connection"}]}]},
		    ejabberd_sm:close_session_unset_presence(
		      StateData#state.sid,
		      StateData#state.user,
		      StateData#state.server,
		      StateData#state.resource,
		      "Replaced by new connection"),
		    presence_broadcast(
		      StateData, From, StateData#state.pres_a, Packet),
		    presence_broadcast(
		      StateData, From, StateData#state.pres_i, Packet);
		_ ->*)
		    (*?INFO_MSG("(~w) Close session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(StateData#state.jid)]),*)
	     lwt () = Lwt_io.printf "Close session for %s\n" (Jlib.jid_to_string state.jid) in
	       (match state with
		  | {pres_last = None;
		     pres_a;
		     pres_i;
		     pres_invis = false; _} when (LJIDSet.is_empty pres_a &&
						    LJIDSet.is_empty pres_i) ->
		      SM.close_session
			state.sid state.user state.server state.resource;
		  | _ ->
		      let from = state.jid in
		      let packet =
			`XmlElement ("presence",
				     [("type", "unavailable")], [])
		      in
			SM.close_session_unset_presence
			  state.sid state.user state.server state.resource "";
			presence_broadcast state from state.pres_a packet;
			presence_broadcast state from state.pres_i packet
	       );
	       Lwt.return ()
	   )
	 | _ ->
	     Lwt.return ()
      );
      Lwt.return ()
end

module C2SServer = GenServer.Make(C2S)
