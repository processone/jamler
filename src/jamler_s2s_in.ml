open Process

let section = Jamler_log.new_section "s2s_in"

module XMLReceiver = Jamler_receiver
module GenServer = Gen_server
module LJID = Jlib.LJID
module LJIDSet = Jlib.LJIDSet
module Hooks = Jamler_hooks
module Auth = Jamler_auth
module SASL = Jamler_sasl
module Router = Jamler_router
module SM = Jamler_sm
module ACL = Jamler_acl
module S2S = Jamler_s2s

module S2SIn :
sig
  type validation_msg = Jamler_s2s_lib.validation_msg
  type msg =
      [ Tcp.msg | XMLReceiver.msg | GenServer.msg | validation_msg ]
  type init_data = Lwt_unix.file_descr
  type state
  val init : init_data -> msg pid -> state Lwt.t
  val handle : msg -> state -> state GenServer.result
  val terminate : state -> unit Lwt.t

  val s2s_stream_features :
    (Jlib.namepreped, Xml.element_cdata list) Hooks.fold_hook
  val s2s_receive_packet : (Jlib.jid * Jlib.jid * Xml.element) Hooks.hook
  val s2s_loop_debug : (XMLReceiver.msg) Hooks.hook

end =
struct
  type validation_msg = Jamler_s2s_lib.validation_msg
  type msg =
      [ Tcp.msg | XMLReceiver.msg | GenServer.msg | validation_msg ]

  type s2s_in_state =
    | Wait_for_stream
    | Wait_for_feature_request
    | Stream_established

  type connection_state =
    | Wait_for_verification
    | Established

  type from_to = (Jlib.namepreped * Jlib.namepreped)

  type state =
      {pid: msg pid;
       socket: Tcp.socket;
       xml_receiver : XMLReceiver.t;
       state : s2s_in_state;
       streamid : string;
       server : Jlib.namepreped;
       auth_domain : Jlib.namepreped;
       connections : (from_to, connection_state) Hashtbl.t;
       tls : bool;
       tls_enabled : bool;
       tls_required : bool;
       tls_certverify : bool;
       tls_options : (string * string) list;
       authenticated : bool;
       (* TODO
	  shaper,
	  timer *)}

  type init_data = Lwt_unix.file_descr

  let s2s_stream_features = Hooks.create_fold ()
  let s2s_receive_packet = Hooks.create ()
  let s2s_loop_debug = Hooks.create ()

  let new_id () =
    Jlib.get_random_string ()

  let send_text state text =
    Tcp.send_async state.socket text

  let send_element state el =
    send_text state (Xml.element_to_string el)

  let stream_header_fmt =
    "<?xml version='1.0'?>" ^^
      "<stream:stream " ^^
      "xmlns:stream='http://etherx.jabber.org/streams' " ^^
      "xmlns='jabber:component:accept' " ^^
      "id='%s'%s>"

  let stream_header id version =
    Printf.sprintf stream_header_fmt id version

  let send_trailer state =
    let stream_trailer = "</stream:stream>" in
    send_text state stream_trailer

  let init socket self =
    lwt () = Lwt_log.debug ~secion "started" in
    let socket = Tcp.of_fd socket self in
    let xml_receiver = XMLReceiver.create self in
    let state = {pid = self;
                 socket;
                 xml_receiver;
                 state = Wait_for_stream;
                 streamid = new_id ();
		 connections = Hashtbl.create 5;
		 authenticated = false;
		 (* TODO *)
		 tls = false;
		 tls_enabled = false;
		 tls_required = false;
		 tls_certverify = false;
		 tls_options = [];
		 server = Jlib.nameprep_exn "";
		 auth_domain = Jlib.nameprep_exn "";
		 (* shaper, timer *)}
    in
      Tcp.activate socket self;
      Lwt.return state

  let invalid_ns_err = Jlib.serr_invalid_namespace
  let invalid_xml_err = Jlib.serr_xml_not_well_formed

  type key_packet = | Key of (string * string * string * string)
		    | Verify of (string * string * string * string)
		    | None

  let is_key_packet = function
    | `XmlElement ("db:result", attrs, els) ->
      Key (Xml.get_attr_s "to" attrs,
	   Xml.get_attr_s "from" attrs,
	   Xml.get_attr_s "id" attrs,
	   Xml.get_cdata els)
    | `XmlElement ("db:verify", attrs, els) ->
      Verify (Xml.get_attr_s "to" attrs,
	      Xml.get_attr_s "from" attrs,
	      Xml.get_attr_s "id" attrs,
	      Xml.get_cdata els)
    | `XmlElement _ ->
      None

  let wait_for_stream msg state =
    match msg with
      | `XmlStreamStart (_name, attrs) -> (
	match (Xml.get_attr_s "xmlns" attrs,
	       Xml.get_attr_s "xmlns:db" attrs,
	       Jlib.nameprep (Xml.get_attr_s "to" attrs),
	       (Xml.get_attr_s "version" attrs) = "1.0") with
	  | "jabber:server", _, (Some lserver), true
	    when (state.tls && not state.authenticated) ->
	    send_text state (stream_header state.streamid " version='1.0'");
	    (* SASL =
		if
		    StateData#state.tls_enabled ->
			case (StateData#state.sockmod):get_peer_certificate(
			       StateData#state.socket) of
			    {ok, Cert} ->
				case (StateData#state.sockmod):get_verify_result(StateData#state.socket) of
				    0 ->
					[{xmlelement, "mechanisms",
					  [{"xmlns", ?NS_SASL}],
					  [{xmlelement, "mechanism", [],
					    [{xmlcdata, "EXTERNAL"}]}]}];
				    CertVerifyRes ->
					case StateData#state.tls_certverify of
					    true -> {error_cert_verif, CertVerifyRes, Cert};
					    false -> []
					end
				end;
			    error ->
				[]
			end;
		    true ->
			[]
		end, *)
	    let sasl = [] in
	    let starttls =
	      if state.tls_enabled then
		[]
	      else if (not state.tls_enabled && not state.tls_required) then
		[`XmlElement ("starttls", [("xmlns", <:ns<TLS>>)], [])]
	      else if (not state.tls_enabled && state.tls_required) then
		[`XmlElement ("starttls", [("xmlns", <:ns<TLS>>)],
			      [`XmlElement ("required", [], [])])]
	      else
		assert false in
	      (* case SASL of
		{error_cert_verif, CertVerifyResult, Certificate} ->
		    CertError = tls:get_cert_verify_string(CertVerifyResult, Certificate),
		    RemoteServer = xml:get_attr_s("from", Attrs),
		    ?INFO_MSG("Closing s2s connection: ~s <--> ~s (~s)", [StateData#state.server, RemoteServer, CertError]),
		    send_text(StateData, xml:element_to_string(?SERRT_POLICY_VIOLATION("en", CertError))),
		    {atomic, Pid} = ejabberd_s2s:find_connection(jlib:make_jid("", Server, ""), jlib:make_jid("", RemoteServer, "")),
		    ejabberd_s2s_out:stop_connection(Pid),

		    {stop, normal, StateData};
		*)
	    lwt feats = Hooks.run_fold s2s_stream_features lserver [] lserver in
	    send_element state (`XmlElement ("stream:features", [],
					     sasl @ starttls @ feats));
	    Lwt.return (`Continue {state
				   with state = Wait_for_feature_request;
				     server = lserver})
	  | "jabber:server", _, (Some lserver), true when state.authenticated ->
	    send_text state (stream_header state.streamid " version='1.0'");
	    lwt feats = Hooks.run_fold s2s_stream_features lserver [] lserver in
	    send_element state (`XmlElement ("stream:features", [], feats));
	    Lwt.return (`Continue {state
				   with state = Stream_established;
				     server = lserver})
	  | "jabber:server", "jabber:server:dialback", (Some lserver), _ ->
	    send_text state (stream_header state.streamid "");
	    Lwt.return (`Continue {state
				   with state = Stream_established;
				     server = lserver})
	  | _ ->
	    send_element state invalid_ns_err;
	    Lwt.return (`Stop state))
      | `XmlStreamError _ ->
	send_text state (stream_header state.streamid "");
	send_element state invalid_xml_err;
	send_trailer state;
	Lwt.return (`Stop state)
      | `XmlStreamEnd _ ->
	send_trailer state;
	Lwt.return (`Stop state)
      | `XmlStreamElement _ ->
	send_trailer state;
	Lwt.return (`Stop state)
      | `Valid _
      | `Invalid _ -> assert false

  let stream_established msg state =
    match msg with
      | `XmlStreamElement el -> (
	(* cancel_timer(StateData#state.timer),
	   Timer = erlang:start_timer(?S2STIMEOUT, self(), []), *)
	(match is_key_packet el with
	  | Key (to', from, id, key) ->
	    lwt () = Lwt_log.debug_f ~section
		       "GET KEY: to = %s, from = %s, id = %s, key = %s"
		       to' from id key in
	    (match (Jlib.nameprep to', Jlib.nameprep from) with
	      | Some lto, Some lfrom ->
		(match (Jamler_s2s_lib.allow_host lto lfrom,
			List.mem lto (Router.dirty_get_all_domains ())) with
		  | true, true ->
		    (*
		      S2SOut.terminate_if_waiting_delay lto lfrom;
		      S2SOut.start lto lfrom (Verify (state.pid, key, state.streamid));
		    *)
		    Hashtbl.replace state.connections
		      (lfrom, lto) Wait_for_verification;
		    (* change_shaper(StateData, LTo, jlib:make_jid("", LFrom, "")), *)
		    Lwt.return (`Continue state) (* timer = Timer *)
		  | _, false ->
		    send_element state Jlib.serr_host_unknown;
		    Lwt.return (`Stop state)
		  | false, _ ->
		    send_element state Jlib.serr_invalid_from;
		    Lwt.return (`Stop state))
	      | _, _ ->
		send_element state Jlib.serr_host_unknown;
		Lwt.return (`Stop state))
	  | Verify (to', from, id, key) ->
	    lwt () = Lwt_log.debug_f ~section
		       "VERIFY KEY: to = %s, from = %s, id = %s, key =%s"
		       to' from id key in
	    let type' = match (Jlib.nameprep to', Jlib.nameprep from) with
	      | Some lto, Some lfrom -> (
		match Jamler_s2s_lib.has_key (lto, lfrom) key with
		  | true -> "valid"
		  | false -> "invalid")
	      | _, _ ->
		"invalid" in
	    send_element state (`XmlElement ("db:verify",
					     [("from", to');
					      ("to", from);
					      ("id", id);
					      ("type", type')],
					     []));
	    Lwt.return (`Continue state) (* timer = Timer *)
	  | None ->
	    let newel = Jlib.remove_attr "xmlns" el in
	    let `XmlElement (name, attrs, _els) = newel in
	    let from_s = Xml.get_attr_s "from" attrs in
	    let to_s = Xml.get_attr_s "to" attrs in
	    lwt () =
	      (match (Jlib.string_to_jid from_s, Jlib.string_to_jid to_s) with
		 | Some from, Some to' ->
		     let lfrom = from.Jlib.lserver in
		     let lto = to'.Jlib.lserver in
		       if state.authenticated then (
			 if (lfrom = state.auth_domain &&
			     List.mem lto (Router.dirty_get_all_domains ())) then (
			   if (name = "iq" || name = "message" || name = "presence")
			   then (
			     lwt () =
			       Hooks.run s2s_receive_packet lto (from, to', newel)
			     in
			       Router.route from to' newel;
			       Lwt.return ()
			   ) else Lwt.return ()
			 ) else Lwt.return ()
		       ) else (
			 try
			   if (Hashtbl.find state.connections (lfrom, lto) = Established
			       && (name = "iq" || name = "message" || name = "presence"))
			   then (
			     lwt () =
			       Hooks.run s2s_receive_packet lto (from, to', newel)
			     in
        		       Router.route from to' newel;
			       Lwt.return ()
			   ) else Lwt.return ()
			 with Not_found ->
			   Lwt.return ()
		       );
		 | _, _ ->
		     Lwt.return ())
	    in
	    lwt () =
	      Hooks.run s2s_loop_debug state.server (`XmlStreamElement el)
	    in
	      Lwt.return (`Continue state)))
      | `Valid (from, to') ->
	  send_element state
	    (`XmlElement
	       ("db:result",
		[("from", (to' : Jlib.namepreped :> string));
		 ("to", (from : Jlib.namepreped :> string));
		 ("type", "valid")],
		[]));
	  Hashtbl.replace state.connections (from, to') Established;
	  Lwt.return (`Continue state)

      | `Invalid (from, to') ->
	  send_element state
	    (`XmlElement
	       ("db:result",
		[("from", (to' : Jlib.namepreped :> string));
		 ("to", (from : Jlib.namepreped :> string));
		 ("type", "invalid")],
		[]));
	  Hashtbl.remove state.connections (from, to');
	  Lwt.return (`Continue state)

      | `XmlStreamEnd _ ->
	Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	send_element state Jlib.serr_invalid_xml;
	send_trailer state;
	Lwt.return (`Stop state)
      | `XmlStreamStart _ ->
	Lwt.return (`Stop state)

(*
stream_established(timeout, StateData) ->
    {stop, normal, StateData};

stream_established(closed, StateData) ->
    {stop, normal, StateData}.
*)		      

  let wait_for_feature_request msg state =
    match msg with
      | `XmlStreamElement _ ->
	(* TODO: TLS/Mutual-Auth stuff
	   TLS = StateData#state.tls,
    TLSEnabled = StateData#state.tls_enabled,
    SockMod = (StateData#state.sockmod):get_sockmod(StateData#state.socket),
    case {xml:get_attr_s("xmlns", Attrs), Name} of
	{?NS_TLS, "starttls"} when TLS == true,
				   TLSEnabled == false,
				   SockMod == gen_tcp ->
	    ?DEBUG("starttls", []),
	    Socket = StateData#state.socket,
	    TLSOpts = case ejabberd_config:get_local_option(
			     {domain_certfile,
			      StateData#state.server}) of
			  undefined ->
			      StateData#state.tls_options;
			  CertFile ->
			      [{certfile, CertFile} |
			       lists:keydelete(
				 certfile, 1,
				 StateData#state.tls_options)]
		      end,
	    TLSSocket = (StateData#state.sockmod):starttls(
			  Socket, TLSOpts,
			  xml:element_to_binary(
			    {xmlelement, "proceed", [{"xmlns", ?NS_TLS}], []})),
	    {next_state, wait_for_stream,
	     StateData#state{socket = TLSSocket,
			     streamid = new_id(),
			     tls_enabled = true,
			     tls_options = TLSOpts
			    }};
	{?NS_SASL, "auth"} when TLSEnabled ->
	    Mech = xml:get_attr_s("mechanism", Attrs),
	    case Mech of
		"EXTERNAL" ->
		    Auth = jlib:decode_base64(xml:get_cdata(Els)),
		    AuthDomain = jlib:nameprep(Auth),
		    AuthRes =
			case (StateData#state.sockmod):get_peer_certificate(
			       StateData#state.socket) of
			    {ok, Cert} ->
				case (StateData#state.sockmod):get_verify_result(
				       StateData#state.socket) of
				    0 ->
					case AuthDomain of
					    error ->
						false;
					    _ ->
						case idna:domain_utf8_to_ascii(AuthDomain) of
						    false ->
							false;
						    PCAuthDomain ->
							lists:any(
							  fun(D) ->
								  match_domain(
								    PCAuthDomain, D)
							  end, get_cert_domains(Cert))
						end
					end;
				    _ ->
					false
				end;
			    error ->
				false
			end,
		    if
			AuthRes ->
			    (StateData#state.sockmod):reset_stream(
			      StateData#state.socket),
			    send_element(StateData,
					 {xmlelement, "success",
					  [{"xmlns", ?NS_SASL}], []}),
			    ?DEBUG("(~w) Accepted s2s authentication for ~s",
				      [StateData#state.socket, AuthDomain]),
			    {next_state, wait_for_stream,
			     StateData#state{streamid = new_id(),
					     authenticated = true,
					     auth_domain = AuthDomain
					    }};
			true ->
			    send_element(StateData,
					 {xmlelement, "failure",
					  [{"xmlns", ?NS_SASL}], []}),
			    send_text(StateData, ?STREAM_TRAILER),
			    {stop, normal, StateData}
		    end;
		_ ->
		    send_element(StateData,
				 {xmlelement, "failure",
				  [{"xmlns", ?NS_SASL}],
				  [{xmlelement, "invalid-mechanism", [], []}]}),
		    {stop, normal, StateData}
	    end; *)
	stream_established msg {state with state = Stream_established}
      | `XmlStreamEnd _ ->
	send_trailer state;
	Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	send_element state invalid_xml_err;
	send_trailer state;
	Lwt.return (`Stop state)
      | `XmlStreamStart _ ->
	Lwt.return (`Stop state)
      | `Valid _
      | `Invalid _ -> assert false

  let handle_xml (msg : [ XMLReceiver.msg | validation_msg ]) state =
    match state.state with
      | Wait_for_stream -> wait_for_stream msg state
      | Wait_for_feature_request -> wait_for_feature_request msg state
      | Stream_established -> stream_established msg state

  let handle (msg : msg) state =
    match msg with
      | `Tcp_data (socket, data) when socket == state.socket ->
	lwt () =
            Lwt_log.debug_f ~section
              "tcp data %d %S" (String.length data) data
          in
            XMLReceiver.parse state.xml_receiver data;
            Tcp.activate state.socket state.pid;
            Lwt.return (`Continue state)
      | `Tcp_data (_socket, _data) -> assert false
      | `Tcp_close socket when socket == state.socket ->
	lwt () = Lwt_log.debug ~section "tcp close" in
        Lwt.return (`Stop state)
      | `Tcp_close _socket -> assert false
      | #XMLReceiver.msg
      | #validation_msg as m ->
          handle_xml m state
      | #GenServer.msg -> assert false

  let terminate state =
    lwt () = Lwt_log.debug ~section "terminated" in
    XMLReceiver.free state.xml_receiver;
    lwt () =
      if Tcp.state state.socket = Lwt_unix.Opened
      then Tcp.close state.socket
      else Lwt.return () in
    Lwt.return()

end

module S2SInServer = GenServer.Make(S2SIn)
