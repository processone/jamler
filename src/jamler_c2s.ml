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
module Roster = Gen_roster
module Privacy = Mod_privacy_sql

let c2s_stream_features :
    (Jlib.namepreped, Xml.element_cdata list) Hooks.fold_hook =
  Hooks.create_fold ()

let c2s_unauthenticated_iq :
    (Jlib.namepreped * Jlib.iq_query Jlib.iq * Unix.inet_addr,
     Xml.element_cdata option) Hooks.fold_hook =
  Hooks.create_fold ()

module C2S :
sig
  include GenServer.Type with
    type msg =
        [ Socket.msg | XMLReceiver.msg | GenServer.msg
        | SM.msg | `Activate ]
    and type init_data = (Lwt_unix.file_descr *
			    bool Jamler_acl.access_rule *
			    string Jamler_acl.access_rule)
    and type stop_reason = [ GenServer.reason | `Replaced ]
end =
struct
  type msg =
      [ Socket.msg | XMLReceiver.msg | GenServer.msg
      | SM.msg | `Activate ]

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
       socket : Socket.socket;
       receiver : unit Lwt.t;
       xml_receiver : XMLReceiver.t;
       state : c2s_state;
       streamid : string;
       access : bool Jamler_acl.access_rule;
       shaper_rule : string Jamler_acl.access_rule;
       mutable shaper : Jamler_shaper.shaper;
       zlib : bool;
       tls : bool;
       tls_required : bool;
       tls_enabled : bool;
       tls_options : Socket.tls_option list;
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
       privacy_list : Privacy.userlist;
       (*conn = unknown,
       auth_module = unknown,*)
       ip : Unix.inet_addr;
       (*redirect = false,
       aux_fields = [],
       fsm_limit_opts,*)
       lang : string;
       dht_nodes : string list;
      }

  type init_data =
      Lwt_unix.file_descr *
	bool Jamler_acl.access_rule *
	string Jamler_acl.access_rule

  type stop_reason = [ GenServer.reason | `Replaced ]

  let section = Jamler_log.new_section "c2s"

  let c2s_open_timeout = 60.0
  let _c2s_hibernate_timeout = 90.0
  let dht_dups = 3

  let fsm_next_state state =
    let res =
      match state.state with
	| Wait_for_stream ->
	    `ContinueTimeout (state, c2s_open_timeout)
	| Wait_for_auth
	| Wait_for_feature_request
	| Wait_for_bind
	| Wait_for_session
	| Wait_for_sasl_response _ ->
	    `ContinueTimeout (state, c2s_open_timeout)
	| Session_established ->
	    `Continue state
    in
      Lwt.return res

  let init (socket, access, shaper_rule) self =
    let socket = Socket.of_fd socket self in
    let xml_receiver = XMLReceiver.create self in
    let shaper = Jamler_shaper.make "none" in
      (* TODO *)
      (*
    Zlib = lists:member(zlib, Opts),
    StartTLS = lists:member(starttls, Opts),
    StartTLSRequired = lists:member(starttls_required, Opts),
    TLSEnabled = lists:member(tls, Opts),
    TLS = StartTLS orelse StartTLSRequired orelse TLSEnabled,
    TLSOpts1 =
	lists:filter(fun({certfile, _}) -> true;
			(_) -> false
		     end, Opts),
    TLSOpts = [verify_none | TLSOpts1],
      *)
    let zlib = true in
    let starttls = true in
    let starttls_required = false in
    let tls_enabled = false in
    let tls = starttls || starttls_required || tls_enabled in
    let tls_options = [ `Certfile "src/ssl.pem" ] in
    let () =
      if tls_enabled
      then Socket.starttls socket []
    in
    let receiver = Socket.activate socket self in
    let state = {pid = self;
		 socket;
		 receiver;
		 xml_receiver;
		 state = Wait_for_stream;
		 zlib;
		 tls;
		 tls_required = starttls_required;
		 tls_enabled;
		 tls_options;
		 streamid = "";
		 access;
		 shaper_rule;
		 shaper;
		 authenticated = false;
		 user = Jlib.nodeprep_exn "";
		 server = Jlib.nameprep_exn "";
		 resource = Jlib.resourceprep_exn "";
		 jid = Jlib.make_jid_exn "" "" "";
		 sid = (0.0, SM.Local (self :> SM.msg pid));
		 pres_t = LJIDSet.empty;
		 pres_f = LJIDSet.empty;
		 pres_a = LJIDSet.empty;
		 pres_i = LJIDSet.empty;
		 pres_timestamp = 0.0;
		 pres_invis = false;
		 pres_last = None;
		 privacy_list = Privacy.new_userlist ();
		 ip = Unix.inet_addr_any;	(* TODO *)
		 lang = "";
		 dht_nodes = [];
		}
    in
      fsm_next_state state

  let myname = "localhost"		(* TODO *)
  let invalid_ns_err = Jlib.serr_invalid_namespace
  let invalid_xml_err = Jlib.serr_xml_not_well_formed
  let host_unknown_err = Jlib.serr_host_unknown
  let invalid_from = Jlib.serr_invalid_from
  let policy_violation_err _lang _text = Jlib.serr_policy_violation (* TODO *)


  let change_shaper state jid =
    let shaper_name =
      Jamler_acl.match_rule state.server state.shaper_rule jid "none"
    in
    let shaper = Jamler_shaper.make shaper_name in
      state.shaper <- shaper

  let send_text state text =
    (*Printf.printf "Send XML on stream = %S\n" text; flush stdout;*)
    Socket.send_async state.socket text

  let send_string state text =
    send_text state (Bytes.of_string text)

  let send_element state el =
    send_string state (Xml.element_to_string el)

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
      send_string state header

  let send_trailer state =
    let stream_trailer = "</stream:stream>" in
      send_string state stream_trailer

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
	     Jlib.iq_xmlns = [%xmlns "AUTH"]; _} ->
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
	    match%lwt Hooks.run_fold c2s_unauthenticated_iq
	      state.server None (state.server, iq, state.ip) with
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
		      Lwt.return (send_element state res)
		| Some res ->
		    Lwt.return (send_element state res)
	  )
	| _ ->
	    (* Drop any stanza, which isn't IQ stanza *)
	    Lwt.return ()

  let privacy_check_packet state from to' packet dir =
    Hooks.run_fold_plain
      Privacy.privacy_check_packet state.server
      true
      (state.user,
       state.server,
       state.privacy_list,
       (from, to', packet),
       dir)


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
	priority packet info state.dht_nodes


  [@@@warning "-52"]
  let get_priority_from_presence presence_packet =
    match Xml.get_subtag presence_packet "priority" with
      | None -> 0
      | Some sub_el -> (
	  try
	    let priority = int_of_string (Xml.get_tag_cdata sub_el) in
	      if priority < 0
	      then -1
	      else priority
	  with
	    | Failure "int_of_string" -> 0
	)

  let roster_change ijid isubscription state =
    let lijid = (*Jlib.jid_tolower*) ijid in
    let isfrom =
      match isubscription with
	| `Both | `From -> true
	| `To | `None | `Remove -> false
    in
    let isto =
      match isubscription with
	| `Both | `To -> true
	| `From | `None | `Remove -> false
    in
    let oldisfrom = LJIDSet.mem lijid state.pres_f in
    let fset =
      if isfrom
      then LJIDSet.add lijid state.pres_f
      else LJIDSet.remove lijid state.pres_f
    in
    let tset =
      if isto
      then LJIDSet.add lijid state.pres_t
      else LJIDSet.remove lijid state.pres_t
    in
      match state.pres_last with
	| None ->
	    {state with pres_f = fset; pres_t = tset}
	| Some p ->
	    (*?DEBUG("roster changed for ~p~n", [StateData#state.user]),*)
	    let from = state.jid in
	    let to' = Jlib.ljid_to_jid ijid in
	    let cond1 = (not state.pres_invis) && isfrom && (not oldisfrom) in
	    let cond2 = (not isfrom) && oldisfrom
	      && (LJIDSet.mem lijid state.pres_a ||
		    LJIDSet.mem lijid state.pres_i)
	    in
	      if cond1 then (
		(* ?DEBUG("C1: ~p~n", [LIJID]), *)
		let () =
		  match privacy_check_packet state from to' p `Out with
		    | false ->
			()
		    | true ->
			Router.route from to' p
		in
		let a = LJIDSet.add lijid state.pres_a in
		  {state with
		     pres_a = a;
		     pres_f = fset;
		     pres_t = tset}
	      ) else if cond2 then (
		(*?DEBUG("C2: ~p~n", [LIJID]),*)
		let pu =
		  `XmlElement ("presence", [("type", "unavailable")], [])
		in
		let () =
		  match privacy_check_packet state from to' pu `Out with
		    | false ->
			()
		    | true ->
			Router.route from to' pu
		in
		let i = LJIDSet.remove lijid state.pres_i in
		let a = LJIDSet.remove lijid state.pres_a in
		  {state with
		     pres_i = i;
		     pres_a = a;
		     pres_f = fset;
		     pres_t = tset}
	      ) else
		{state with
		   pres_f = fset;
		   pres_t = tset}


  let resend_offline_messages _state =
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

  let resend_subscription_requests ({user = _user;
				     server = _server; _} as _state) =
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
	      let _timestamp = state.pres_timestamp in
		    (*Packet1 = maybe_add_delay(Packet, utc, To, "", Timestamp),*)
		match privacy_check_packet state to' from packet `Out with
		  | false ->
		      ()
		  | true ->
		      (*Pid=element(2, StateData#state.sid),
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
		status info state.dht_nodes;
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
	    ignore (Hooks.run Roster.roster_out_subscription server
	              (user, server, to', `Subscribe));
	    check_privacy_route from state (Jlib.jid_remove_resource from)
	      to' packet;
	    state
	| "subscribed" ->
	    ignore (Hooks.run Roster.roster_out_subscription server
	              (user, server, to', `Subscribed));
	    check_privacy_route from state (Jlib.jid_remove_resource from)
	      to' packet;
	    state
	| "unsubscribe" ->
	    ignore (Hooks.run Roster.roster_out_subscription server
	              (user, server, to', `Unsubscribe));
	    check_privacy_route from state (Jlib.jid_remove_resource from)
	      to' packet;
	    state
	| "unsubscribed" ->
	    ignore (Hooks.run Roster.roster_out_subscription server
	              (user, server, to', `Unsubscribed));
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

  let process_privacy_iq from to' iq state =
    let%lwt res, subel, state =
      match iq with
	| {Jlib.iq_type = `Get subel; _} as iq ->
	    let%lwt res =
	      Hooks.run_fold
		Privacy.privacy_iq_get
		state.server
		(`Error Jlib.err_feature_not_implemented)
		(from, to', iq, state.privacy_list)
	    in
	      Lwt.return (res, subel, state)
	| {Jlib.iq_type = `Set subel; _} as iq -> (
	    match%lwt (Hooks.run_fold Privacy.privacy_iq_set state.server
			 (`Error Jlib.err_feature_not_implemented)
			 (from, to', iq)) with
	      | `ResultList (res, newprivlist) ->
		  Lwt.return (`Result res, subel,
			      {state with privacy_list = newprivlist})
	      | (`Result _ | `Error _) as res ->
		  Lwt.return (res, subel, state)
	  )
    in
    let iqres =
      match res with
	| `Result result ->
	    {iq with Jlib.iq_type = `Result result}
	| `Error error ->
	    {iq with Jlib.iq_type = `Error (error, Some subel)}
    in
      Router.route to' from (Jlib.iq_to_xml iqres);
      Lwt.return state


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

  let (--) xs ys = List.filter (fun x -> not (List.mem x ys)) xs

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
        | None -> -2
        | Some presence -> get_priority_from_presence presence
    in
    let hash = Jamler_cluster.hash_user state.user state.server in
    let module Cluster = Jamler_cluster.JamlerCluster in
    let dht_nodes = Cluster.get_nodes_by_hash hash dht_dups in
    let new_dht_nodes = dht_nodes -- state.dht_nodes in
    let state = {state with dht_nodes = new_dht_nodes @ state.dht_nodes} in
      SM.open_session sid u s r priority info new_dht_nodes;
      fsm_next_state state
	(*Node ->
	    fsm_migrate(StateName, PackedStateData, Node, 0)
    end.*)


  let wait_for_stream msg state =
    match msg with
      | `XmlStreamStart (_name, attrs) -> (
	  let default_lang = "en" in	(* TODO *)
	    match Xml.get_attr_s "xmlns:stream" attrs with
	      | [%xmlns "STREAM"] -> (
		  let server = Jlib.nameprep (Xml.get_attr_s "to" attrs) in
		    match server with
		      | Some server when Jamler_config.is_my_host server -> (
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
			    change_shaper state
			      (Jlib.make_jid_exn "" (server :> string) "");
			    match Xml.get_attr_s "version" attrs with
			      | "1.0" -> (
				  send_header state (server :> string) "1.0" default_lang;
				  if not state.authenticated then (
				    let mechs =
				      List.map
					(fun s ->
					   `XmlElement ("mechanism", [],
							[`XmlCdata s]))
					(SASL.listmech server)
				    in
				    let sockmod = Socket.get_name state.socket in
				    let zlib = state.zlib in
				    let compress_feature =
				      if zlib &&
					(sockmod = `Tcp (*||
							  (SockMod == tls)*))
				      then
					[`XmlElement
					   ("compression",
					    [("xmlns",
					      [%xmlns "FEATURE_COMPRESS"])],
					    [`XmlElement
					       ("method",
						[], [`XmlCdata "zlib"])])]
				      else []
				    in
				    let tls = state.tls in
				    let tls_enabled = state.tls_enabled in
				    let tls_required = state.tls_required in
				    let tls_feature =
				      if tls &&
					not tls_enabled &&
					sockmod = `Tcp
				      then (
					if tls_required
					then
					  [`XmlElement
					     ("starttls",
					      [("xmlns", [%xmlns "TLS"])],
					      [`XmlElement ("required",
							    [], [])])]
					else
					  [`XmlElement
					     ("starttls",
					      [("xmlns", [%xmlns "TLS"])], [])]
				      ) else []
				    in
				    let%lwt stream_feat_els =
				      Hooks.run_fold c2s_stream_features
					server [] (server) in
				      send_element
					state
					(`XmlElement
					   ("stream:features", [],
					    tls_feature @
					      compress_feature @
					      [`XmlElement
						 ("mechanisms",
						  [("xmlns", [%xmlns "SASL"])],
						  mechs)] @ stream_feat_els));
				      fsm_next_state
					{state with
					   state = Wait_for_feature_request;
					   server = server;
					   lang = lang}
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
						[("xmlns", [%xmlns "BIND"])], []);
					     `XmlElement
					       ("session",
						[("xmlns", [%xmlns "SESSION"])], [])]
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
					    fsm_next_state
					      {state with
						 state = Wait_for_bind;
						 server = server;
						 lang = lang}
				      | _ ->
					  send_element
					    state
					    (`XmlElement
					       ("stream:features", [], []));
					  fsm_next_state
					    {state with
					       state = Wait_for_session;
					       server = server;
					       lang = lang}
				  )
				)
			      | _ ->
				  send_header state (server :> string) "" default_lang;
				  if not state.tls_enabled &&
				    state.tls_required
				  then (
				    send_element
				      state
				      (policy_violation_err
					 lang
					 "Use of STARTTLS required");
				    send_trailer state;
				    Lwt.return (`Stop state)
				  ) else
				    fsm_next_state
				      {state with
					 state = Wait_for_auth;
					 server = server;
					 lang = lang}
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

      | `Timeout ->
	  Lwt.return (`Stop state)

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
		  match Auth.plain_password_required state.server with
		    | false ->
			  `XmlElement
			    (name, attrs,
			     [`XmlElement
				("query", [("xmlns", [%xmlns "AUTH"])],
				 [`XmlElement ("username", [], ucdata);
				  `XmlElement ("password", [], []);
				  `XmlElement ("digest", [], []);
				  `XmlElement ("resource", [], [])
				 ])])
		    | true ->
			  `XmlElement
			    (name, attrs,
			     [`XmlElement
				("query", [("xmlns", [%xmlns "AUTH"])],
				 [`XmlElement ("username", [], ucdata);
				  `XmlElement ("password", [], []);
				  `XmlElement ("resource", [], [])
				 ])])
		in
		  send_element state res;
		  fsm_next_state {state with state = Wait_for_auth}
	      )
	    | Some (_id, `Set, (_u, _p, _d, "")) ->
		let err =
		  Jlib.make_error_reply el
		    (Jlib.err_auth_no_resource_provided state.lang)
		in
		  send_element state err;
		  fsm_next_state {state with state = Wait_for_auth}
	    | Some (_id, `Set, (u, p, d, r)) -> (
		match Jlib.make_jid u (state.server :> string) r with
		  | Some jid when (Jamler_acl.match_rule
				     state.server state.access jid false) -> (
		      let dgen pw =
                        Jlib.sha1 (state.streamid ^ pw)
		      in
			match%lwt Auth.check_password_digest_with_authmodule
			  jid.Jlib.luser state.server p d dgen
			with
			  | Some _auth_module -> (
			      (*?INFO_MSG(
				"(~w) Accepted legacy authentication for ~s by ~p",
				[StateData#state.socket,
				jlib:jid_to_string(JID), AuthModule]),*)
			      let%lwt () =
				Lwt_log.notice_f ~section
				  "%a accepted legacy authentication for %S"
				  format_pid state.pid
				  (Jlib.jid_to_string jid)
			      in
				match (*need_redirect(StateData#state{user = U})*) (* TODO *)
				  None
				with
                                  | Some host -> (
                                    (*?INFO_MSG("(~w) Redirecting ~s to ~s",
                                              [StateData#state.socket,
                                               jlib:jid_to_string(JID), Host]),*)
				      let%lwt () = Lwt_log.notice_f ~section "redirecting %s to %s" (Jlib.jid_to_string jid) host in
                                        send_element state
					  (Jlib.serr_see_other_host host);
                                        send_trailer state;
					Lwt.return
					  (`Stop state);
	                            )
	                          | None ->
                                      let sid =
					(Unix.gettimeofday (),
					 SM.Local (state.pid :> SM.msg pid))
				      in
				      (*Conn = get_conn_type(StateData),*)
                                      let res = Jlib.make_result_iq_reply el in
					(*Res = setelement(4, Res1, []),*)
					send_element state res;
					change_shaper state jid;
					let%lwt (fs, ts) =
					  Hooks.run_fold
					    Roster.roster_get_subscription_lists
					    state.server
					    ([], [])
					    (jid.Jlib.luser, state.server)
					in
					let ljid =
					  Jlib.jid_tolower (Jlib.jid_remove_resource jid)
					in
					let fs = ljid :: fs in
					let ts = ljid :: ts in
					let%lwt priv_list =
                                          Hooks.run_fold
                                            Privacy.privacy_get_user_list
                                            state.server
                                            (Privacy.new_userlist ())
                                            (jid.Jlib.luser, state.server)
					in
					let state =
                                          {state with
                                             user = jid.Jlib.luser;
                                             resource = jid.Jlib.lresource;
                                             jid = jid;
                                             sid = sid;
                                               (*conn = Conn,
                                               auth_module = AuthModule,*)
					     pres_f = LJIDSet.from_list fs;
					     pres_t = LJIDSet.from_list ts;
                                             privacy_list = priv_list}
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
			      let%lwt () =
				Lwt_log.notice_f ~section
				  "%a failed legacy authentication for %S"
				  format_pid state.pid
				  (Jlib.jid_to_string jid)
			      in
		              let err =
				Jlib.make_error_reply el Jlib.err_not_authorized
			      in
				send_element state err;
				fsm_next_state
				  {state with state = Wait_for_auth}
		    )
		  | None ->
		      let%lwt () =
			Lwt_log.notice_f ~section
			  "%a forbidden legacy authentication for %S with resource %S"
			  format_pid state.pid
			  u r
		      in
		      let err = Jlib.make_error_reply el Jlib.err_jid_malformed
		      in
			send_element state err;
			fsm_next_state {state with state = Wait_for_auth}
		  | Some jid ->
		      (*?INFO_MSG(
			"(~w) Forbidden legacy authentication for ~s",
			[StateData#state.socket,
			jlib:jid_to_string(JID)]),*)
		      let%lwt () =
			Lwt_log.notice_f ~section
			  "%a forbidden legacy authentication for %S"
			  format_pid state.pid
			  (Jlib.jid_to_string jid)
		      in
		      let err = Jlib.make_error_reply el Jlib.err_not_allowed
		      in
			send_element state err;
			fsm_next_state {state with state = Wait_for_auth}
	      )
	    | None ->
		let%lwt () = process_unauthenticated_stanza state el in
		  fsm_next_state {state with state = Wait_for_auth}
	)

      | `Timeout ->
	  Lwt.return (`Stop state)

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
	  let zlib = state.zlib in
	  let tls = state.tls in
	  let tls_enabled = state.tls_enabled in
	  let tls_required = state.tls_required in
	  let sockmod = Socket.get_name state.socket in
	    match (Xml.get_attr_s "xmlns" attrs), name with
	      | [%xmlns "SASL"], "auth" when (not (sockmod = `Tcp &&
					      tls_required)) -> (
		  let mech = Xml.get_attr_s "mechanism" attrs in
		  let client_in = Jlib.decode_base64 (Xml.get_cdata els) in
		  let%lwt sasl_result =
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
			  XMLReceiver.reset_stream state.xml_receiver;
			  let u = Jlib.nodeprep_exn (List.assoc `Username props) in	(* TODO *)
			    (*AuthModule = xml:get_attr_s(auth_module, Props),*)
			    (*?INFO_MSG("(~w) Accepted authentication for ~s by ~p",
                              [StateData#state.socket, U, AuthModule]),*)
			  let%lwt () =
			    Lwt_log.notice_f ~section
			      "%a accepted authentication for %S"
			      format_pid state.pid
			      (u :> string)
			  in
                            send_element state
                              (`XmlElement ("success",
                                            [("xmlns", [%xmlns "SASL"])], []));
                            fsm_next_state
			      {state with
				 state = Wait_for_stream;
                                 streamid = new_id ();
                                 authenticated = true;
                                 (*auth_module = AuthModule,*)
                                 user = u}
			)
		      | SASL.Continue (server_out, sasl_mech) ->
			  send_element state
			    (`XmlElement
			       ("challenge",
				[("xmlns", [%xmlns "SASL"])],
				[`XmlCdata (Jlib.encode_base64 server_out)]));
			  fsm_next_state
			    {state with
			       state = Wait_for_sasl_response sasl_mech}
		      | SASL.ErrorUser (error, username) ->
			  (*?INFO_MSG(
			    "(~w) Failed authentication for ~s@~s",
			    [StateData#state.socket,
			    Username, StateData#state.server]),*)
			  let%lwt () =
			    Lwt_log.notice_f ~section
			      "%a failed authentication for %s@%s"
			      format_pid state.pid
			      username (state.server :> string)
			  in
			    send_element state
			      (`XmlElement
				 ("failure",
				  [("xmlns", [%xmlns "SASL"])],
				  [`XmlElement (error, [], [])]));
			    fsm_next_state
			      {state with
				 state = Wait_for_feature_request}
		      | SASL.Error error ->
			  send_element state
			    (`XmlElement
			       ("failure",
				[("xmlns", [%xmlns "SASL"])],
				[`XmlElement (error, [], [])]));
			  fsm_next_state
			    {state with
			       state = Wait_for_feature_request}
		)
	      | [%xmlns "TLS"], "starttls" when (tls && not tls_enabled &&
					       sockmod = `Tcp) ->
		let tlsopts =		(* TODO *)
		  (*case ejabberd_config:get_local_option(
			     {domain_certfile, StateData#state.server}) of
			  undefined ->*)
		  state.tls_options
			  (*CertFile ->
			      [{certfile, CertFile} |
			       lists:keydelete(
				 certfile, 1, StateData#state.tls_options)]
		      end,*)
		in
		let socket = state.socket in
		  Lwt.cancel state.receiver;
		  XMLReceiver.reset_stream state.xml_receiver;
		  send_element
		    state
		    (`XmlElement ("proceed", [("xmlns", [%xmlns "TLS"])], []));
		  Socket.starttls socket tlsopts;
		  let receiver = Socket.activate state.socket state.pid in
		    fsm_next_state
		      {state with
			 state = Wait_for_stream;
			 receiver;
			 streamid = new_id();
			 tls_enabled = true;
		      }
	      | [%xmlns "COMPRESS"], "compress" when (
		  zlib && (sockmod = `Tcp (*|| SockMod == tls*))) -> (
		  match Xml.get_subtag el "method" with
		    | None ->
			send_element state
			  (`XmlElement
			     ("failure",
			      [("xmlns", [%xmlns "COMPRESS"])],
			      [`XmlElement ("setup-failed", [], [])]));
			fsm_next_state state
		    | Some method' -> (
			match Xml.get_tag_cdata method' with
			  | "zlib" ->
			      let socket = state.socket in
				Lwt.cancel state.receiver;
				XMLReceiver.reset_stream state.xml_receiver;
				send_element
				  state
				  (`XmlElement
				     ("compressed",
				      [("xmlns", [%xmlns "COMPRESS"])], []));
				Socket.compress socket;
				let receiver =
				  Socket.activate state.socket state.pid
				in
				  fsm_next_state
				    {state with
				       state = Wait_for_stream;
				       receiver;
				       streamid = new_id();
				    }
			  | _ ->
			      send_element state
				(`XmlElement
				   ("failure",
				    [("xmlns", [%xmlns "COMPRESS"])],
				    [`XmlElement
				       ("unsupported-method",
					[], [])]));
			      fsm_next_state state
		      )
		)
	      | _ ->
		  if sockmod = `Tcp && tls_required then (
		    let lang = state.lang in
		    send_element state
		      (policy_violation_err lang
			 "Use of STARTTLS required");
		      send_trailer state;
		      Lwt.return (`Stop state)
		  ) else (
		    let%lwt () = process_unauthenticated_stanza state el in
		      fsm_next_state
			{state with
			   state = Wait_for_feature_request}
		  )
	)

      | `Timeout ->
	  Lwt.return (`Stop state)

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
	      | [%xmlns "SASL"], "response" -> (
		  let client_in = Jlib.decode_base64 (Xml.get_cdata els) in
		    match%lwt SASL.server_step sasl_state client_in with
		      | SASL.Done props -> (
			  XMLReceiver.reset_stream state.xml_receiver;
			  let u = Jlib.nodeprep_exn (List.assoc `Username props) in	(* TODO *)
			    (*AuthModule = xml:get_attr_s(auth_module, Props),
			      ?INFO_MSG("(~w) Accepted authentication for ~s by ~p",
			      [StateData#state.socket, U, AuthModule]),*)
			  let%lwt () =
			    Lwt_log.notice_f ~section
			      "%a accepted authentication for %S"
			      format_pid state.pid
			      (u :> string)
			  in
                            send_element state
                              (`XmlElement ("success",
                                            [("xmlns", [%xmlns "SASL"])], []));
                            fsm_next_state
			      {state with
				 state = Wait_for_stream;
                                 streamid = new_id ();
                                 authenticated = true;
                                 (*auth_module = AuthModule,*)
                                 user = u}
			)
		      | SASL.Continue (server_out, sasl_mech) ->
			  send_element state
			    (`XmlElement
			       ("challenge",
				[("xmlns", [%xmlns "SASL"])],
				[`XmlCdata (Jlib.encode_base64 server_out)]));
			  fsm_next_state
			    {state with
			       state = Wait_for_sasl_response sasl_mech}
		      | SASL.ErrorUser (error, username) ->
			  let%lwt () =
			    Lwt_log.notice_f ~section
			      "%a failed authentication for %S at %s"
			      format_pid state.pid
			      username (state.server :> string)
			  in
			    send_element state
			      (`XmlElement
				 ("failure",
				  [("xmlns", [%xmlns "SASL"])],
				  [`XmlElement (error, [], [])]));
			    fsm_next_state
			      {state with
				 state = Wait_for_feature_request}
		      | SASL.Error error ->
			  send_element state
			    (`XmlElement
			       ("failure",
				[("xmlns", [%xmlns "SASL"])],
				[`XmlElement (error, [], [])]));
			  fsm_next_state
			    {state with
			       state = Wait_for_feature_request}
		)
	      | _ ->
		  let%lwt () = process_unauthenticated_stanza state el in
		    fsm_next_state
		      {state with state = Wait_for_feature_request}
	)

      | `Timeout ->
	  Lwt.return (`Stop state)

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
		    Jlib.iq_xmlns = [%xmlns "BIND"]; _} as iq) -> (
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
			  fsm_next_state
			    {state with state = Wait_for_bind}
		    | Some r ->
			let jid = Jlib.make_jid' u state.server r in
			let res =
			  {iq with
			     Jlib.iq_type =
			      `Result
				(Some (`XmlElement
					 ("bind",
					  [("xmlns", [%xmlns "BIND"])],
					  [`XmlElement
					     ("jid", [],
					      [`XmlCdata
						 (Jlib.jid_to_string jid)])])
				      ))}
			in
			  send_element state (Jlib.iq_to_xml res);
			  fsm_next_state
			    {state with
			       state = Wait_for_session;
			       resource = r;
			       jid = jid}
	      )
	    | _ ->
		fsm_next_state {state with state = Wait_for_bind}
	)

      | `Timeout ->
	  Lwt.return (`Stop state)

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
	    | `IQ ({Jlib.iq_type = `Set _sub_el;
		    Jlib.iq_xmlns = [%xmlns "SESSION"]; _} as _iq) -> (
		let u = state.user in
		let jid = state.jid in
		  match (Jamler_acl.match_rule
			   state.server state.access jid false) with
		    | true ->
		      let%lwt () =
			Lwt_log.notice_f ~section
			  "%a opened session for %s"
			  format_pid state.pid
			  (Jlib.jid_to_string jid)
		      in
		      let res = Jlib.make_result_iq_reply el in
			send_element state res;
			change_shaper state jid;
			let%lwt (fs, ts) =
			  Hooks.run_fold
			    Roster.roster_get_subscription_lists
			    state.server
			    ([], [])
			    (u, state.server)
			in
			let ljid =
			  Jlib.jid_tolower (Jlib.jid_remove_resource jid)
			in
			let fs = ljid :: fs in
			let ts = ljid :: ts in
			let%lwt priv_list =
                          Hooks.run_fold
                            Privacy.privacy_get_user_list
                            state.server
                            (Privacy.new_userlist ())
                            (jid.Jlib.luser, state.server)
			in
                        let sid =
			  (Unix.gettimeofday (),
			   SM.Local (state.pid :> SM.msg pid))
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
			     privacy_list = priv_list}
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
		    *)
			let%lwt () =
			  Lwt_log.notice_f ~section
			    "%a forbidden session for %s"
			    format_pid state.pid
			    (Jlib.jid_to_string jid)
			in
			let err = Jlib.make_error_reply el Jlib.err_not_allowed
			in
			  send_element state err;
			  fsm_next_state
			    {state with state = Wait_for_session}
	      )
	    | _ ->
		fsm_next_state {state with state = Wait_for_session}
	)

      | `Timeout ->
	  Lwt.return (`Stop state)

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
    let%lwt state =
      match to_jid with
	| None -> (
	    match Xml.get_attr_s "type" attrs with
	      | "error"
	      | "result" -> Lwt.return state
	      | _ ->
		  let err = Jlib.make_error_reply el Jlib.err_jid_malformed in
	  	    send_element state err;
		    Lwt.return state
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
		    Lwt.return (presence_update from_jid el state)
		  ) else (
		    Lwt.return (presence_track from_jid to_jid el state)
		  )
		)
	      | "iq" -> (
		  match Jlib.iq_query_info el with
		    | `IQ ({Jlib.iq_xmlns = [%xmlns "PRIVACY"]; _} as iq) ->
				(*ejabberd_hooks:run(
				  user_send_packet,
				  Server,
				  [StateData#state.debug, FromJID, ToJID, NewEl]),*)
			process_privacy_iq from_jid to_jid iq state
		    | _ ->
				(*ejabberd_hooks:run(
				  user_send_packet,
				  Server,
                                  [StateData#state.debug, FromJID, ToJID, NewEl]),*)
			check_privacy_route from_jid state from_jid to_jid el;
			Lwt.return state
		)
	      | "message" ->
			(*ejabberd_hooks:run(user_send_packet,
					   Server,
					   [StateData#state.debug, FromJID, ToJID, NewEl]),
			*)
		  check_privacy_route from_jid state from_jid to_jid el;
		  Lwt.return state
	      | _ ->
		  Lwt.return state
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
      | `Timeout ->
	  fsm_next_state state

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
    match state.state with
      | Wait_for_stream -> wait_for_stream msg state
      | Wait_for_auth -> wait_for_auth msg state
      | Wait_for_feature_request -> wait_for_feature_request msg state
      | Wait_for_bind -> wait_for_bind msg state
      | Wait_for_session -> wait_for_session msg state
      | Wait_for_sasl_response sasl_state ->
	  wait_for_sasl_response msg sasl_state state
      | Session_established -> session_established msg state

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

  let handle_broadcast (`Broadcast data) state =
    let%lwt (stop, state) =
      match data with
	| `RosterItem (ijid, isubscription) ->
	    let state = roster_change ijid isubscription state in
	      Lwt.return (false, state)
	| _ -> (
	    (*
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
	    Lwt.return (false, state)
	  )
    in
      if stop then (
	send_trailer state;
	(*case NewState of
		rebind ->
		    {stop, normal, StateData#state{authenticated = rebinded}};
		_ ->*)
	Lwt.return (`Stop state)
      ) else (
	(*ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),*)
	fsm_next_state state
      )

  let node_up _node state =
    match state.state with
      | Session_established -> (
	  let {user = u; server = s; resource = r; sid = sid; _} = state in
	  let info = [] in
	  let presence = state.pres_last in
	  let priority =
	    match presence with
              | None -> -2
              | Some presence -> get_priority_from_presence presence
	  in
	  let hash = Jamler_cluster.hash_user state.user state.server in
	  let module Cluster = Jamler_cluster.JamlerCluster in
	  let dht_nodes = Cluster.get_nodes_by_hash hash dht_dups in
	  let new_dht_nodes = dht_nodes -- state.dht_nodes in
	    match new_dht_nodes with
	      | [] ->
		  fsm_next_state state
	      | _ ->
		  let state =
		    {state with dht_nodes = new_dht_nodes @ state.dht_nodes}
		  in
		    SM.open_session sid u s r priority info new_dht_nodes;
		    fsm_next_state state
	)
      | _ ->
	  fsm_next_state state

  let node_down node state =
    match state.state with
      | Session_established -> (
	  if List.mem node state.dht_nodes &&
	    List.length state.dht_nodes <= dht_dups
	  then (
	    let {user = u; server = s; resource = r; sid = sid; _} = state in
	  let info = [] in
	  let presence = state.pres_last in
	  let priority =
	    match presence with
              | None -> -2
              | Some presence -> get_priority_from_presence presence
	  in
	  let hash = Jamler_cluster.hash_user state.user state.server in
	  let module Cluster = Jamler_cluster.JamlerCluster in
	  let dht_nodes = Cluster.get_nodes_by_hash hash dht_dups in
	  let old_dht_nodes =
	    List.filter (fun n -> n <> node) state.dht_nodes
	  in
	  let new_dht_nodes = dht_nodes -- state.dht_nodes in
	    match new_dht_nodes with
	      | [] ->
		  let state = {state with dht_nodes = old_dht_nodes} in
		    fsm_next_state state
	      | _ ->
		  let state =
		    {state with dht_nodes = new_dht_nodes @ old_dht_nodes}
		  in
		    SM.open_session sid u s r priority info new_dht_nodes;
		    fsm_next_state state
	  ) else (
	    let dht_nodes = List.filter (fun n -> n <> node) state.dht_nodes in
	    let state = {state with dht_nodes} in
	      fsm_next_state state
	  )
	)
      | _ ->
	  fsm_next_state state


  let handle msg state =
    match msg with
      | `Tcp_data (socket, data) when socket == state.socket ->
          let%lwt () =
	    Lwt_log.debug_f ~section
	      "tcp data %d %S" (String.length data) data
	  in
            XMLReceiver.parse state.xml_receiver data;
	    let pause =
	      Jamler_shaper.update state.shaper (String.length data)
	    in
            let receiver =
	      if pause > 0.0 then (
		let%lwt () = send_after pause state.pid `Activate in
		Lwt.return ()
	      ) else Socket.activate state.socket state.pid
	    in
              fsm_next_state {state with receiver}
      | `Tcp_data (_socket, _data) -> assert false
      | `Tcp_close socket when socket == state.socket ->
          let%lwt () = Lwt_log.debug ~section "tcp close" in
            (*Gc.print_stat stdout;
            Gc.compact ();
            Gc.print_stat stdout; flush stdout;*)
            Lwt.return (`Stop state)
      | `Tcp_close _socket -> assert false
      | `Activate ->
	  Lwt.cancel state.receiver;
	  let receiver = Socket.activate state.socket state.pid in
            fsm_next_state {state with receiver}
      | (#XMLReceiver.msg | `Timeout) as m ->
	  handle_xml m state
      | #Router.msg as m ->
	  handle_route m state
      | `Broadcast _ as m ->
	  handle_broadcast m state
      | `Replaced ->
	  (* TODO *)
	  (*let lang = state.lang in*)
	  send_element
	    state
	    Jlib.serr_conflict (*Lang, "Replaced by new connection"*);
	  send_trailer state;
	  Lwt.return (`StopReason (state, `Replaced))
      | `Node_up node ->
          node_up node state
      | `Node_down node ->
          node_down node state
      | #GenServer.system_msg -> assert false

  let terminate state reason =
    XMLReceiver.free state.xml_receiver;
    let%lwt () = Socket.close state.socket in
    let%lwt () =
      match state.state with
	| Session_established -> (
	    match reason with
	      | `Replaced -> (
		  let%lwt () =
		    Lwt_log.notice_f ~section
		      "%a replaced session for %s"
		      format_pid state.pid
		      (Jlib.jid_to_string state.jid)
		  in
		  let from = state.jid in
		  let packet =
		    `XmlElement
		      ("presence",
		       [("type", "unavailable")],
		       [`XmlElement
			  ("status", [],
			   [`XmlCdata "Replaced by new connection"])])
		  in
		  let info = [] in
		    SM.close_session_unset_presence
		      state.sid
		      state.user state.server state.resource
		      "Replaced by new connection"
		      info
		      state.dht_nodes;
		    presence_broadcast
		      state from state.pres_a packet;
		    presence_broadcast
		      state from state.pres_i packet;
		    Lwt.return ()
		)
	      | _ -> (
		  let%lwt () =
		    Lwt_log.notice_f ~section
		      "%a close session for %s"
		      format_pid state.pid
		      (Jlib.jid_to_string state.jid)
		  in
		    (match state with
		       | {pres_last = None;
			  pres_a;
			  pres_i;
			  pres_invis = false; _} when
			   (LJIDSet.is_empty pres_a &&
			      LJIDSet.is_empty pres_i) ->
			   let info = [] in
			     SM.close_session
			       state.sid state.user state.server state.resource
			       info state.dht_nodes;
		       | _ ->
			   let from = state.jid in
			   let packet =
			     `XmlElement ("presence",
					  [("type", "unavailable")], [])
			   in
			   let info = [] in
			     SM.close_session_unset_presence
			       state.sid
			       state.user state.server state.resource ""
			       info state.dht_nodes;
			     presence_broadcast state from state.pres_a packet;
			     presence_broadcast state from state.pres_i packet
		    );
		    Lwt.return ()
		)
	  )
	| _ ->
	    Lwt.return ()
    in
      Lwt.return ()
end

module C2SServer = GenServer.Make(C2S)

module C2SListen : Jamler_listener.ListenModule =
struct
  let name = "c2s"
  module JSON = Yojson.Safe
  let listen_parser =
    Jamler_config.(
      P (function
	   | `Assoc assoc ->
	       let access =
		 try
		   match List.assoc "access" assoc with
		     | `String access_name ->
			 Jamler_acl.(get_rule access_name access)
		     | json ->
			 raise (Error
				  (Printf.sprintf
				     "access value must be a string, got %s"
				     (JSON.to_string json)))
		 with
		   | Not_found ->
		       Jamler_acl.all
	       in
	       let shaper =
		 try
		   match List.assoc "shaper" assoc with
		     | `String access_name ->
			 Jamler_acl.(get_rule access_name string)
		     | json ->
			 raise (Error
				  (Printf.sprintf
				     "shaper value must be a string, got %s"
				     (JSON.to_string json)))
		 with
		   | Not_found ->
		       Jamler_acl.none_string
	       in
		 (fun socket ->
		    any_pid (C2SServer.start (socket, access, shaper)))
	   | json ->
	       raise (Error
			(Printf.sprintf "expected JSON object value, got %s"
			   (JSON.to_string json)))
	)
    )
end

let () =
  Jamler_listener.register_mod
    (module C2SListen : Jamler_listener.ListenModule)
