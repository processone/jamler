open Process

let section = Jamler_log.new_section "s2s_out"

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

module S2SOut :
sig
  type msg =
      [ Tcp.msg | XMLReceiver.msg | GenServer.msg | SM.msg
      | `Init | `Closed | `Timeout
      | `Send_element of Xml.element]
  type init_data
  type state
  val init : init_data -> msg pid -> state Lwt.t
  val handle : msg -> state -> state GenServer.result
  val terminate : state -> unit Lwt.t

end =
struct
  type msg =
      [ Tcp.msg | XMLReceiver.msg | GenServer.msg | SM.msg
      | `Init | `Closed | `Timeout
      | `Send_element of Xml.element]

  type s2s_out_state =
    | Open_socket
    | Wait_for_stream
    | Wait_for_validation
    | Wait_for_features
    | Wait_for_auth_result
    | Wait_for_starttls_proceed
	(* | Relay_to_bridge *)
    | Reopen_socket
    | Wait_before_retry
    | Stream_established

  type tls_option =
    | CertFile of string
    | Connect

  type start_type =
      [ `New of string | `Verify of (msg pid * string * string)]

  type init_data = Jlib.namepreped * Jlib.namepreped * start_type

  type state =
      {pid: msg pid;
       socket : Tcp.socket option;
       xml_receiver : XMLReceiver.t;
       state: s2s_out_state;
       streamid : string;
       use_v10 : bool;
       tls : bool;
       tls_required : bool;
       tls_enabled : bool;
       tls_options : tls_option list;
       authenticated : bool;
       db_enabled : bool;
       try_auth : bool;
       myname : Jlib.namepreped;
       server : Jlib.namepreped;
       queue : Xml.element Queue.t;
       delay_to_retry: int;
       new' : string option;
       verify : (msg pid * string * string) option;
	 (* bridge, timer *)}

  let new_id () =
    Jlib.get_random_string ()

  let start_connection pid =
    pid $! `Init

  let stop_connection pid =
    pid $! `Closed

  let socket_default_result = None

  type verify_res = | Result of (string * string * string * string)
                    | Verify of (string * string * string * string)
                    | False

  let is_verify_res = function
    | `XmlElement ("db:result", attrs, _) ->
	Result (Xml.get_attr_s "to" attrs,
		Xml.get_attr_s "from" attrs,
		Xml.get_attr_s "id" attrs,
		Xml.get_attr_s "type" attrs)
    | `XmlElement ("db:verify", attrs, _) ->
	Verify (Xml.get_attr_s "to" attrs,
		Xml.get_attr_s "from" attrs,
		Xml.get_attr_s "id" attrs,
		Xml.get_attr_s "type" attrs)
    | _ ->
	False

  let init (from, server, start_type) self =
    (* ?DEBUG("started: ~p", [{From, Server, Type}]),
       {TLS, TLSRequired} = case ejabberd_config:get_local_option(s2s_use_starttls) of
       UseTls when (UseTls==undefined) or (UseTls==false) ->
       {false, false};
       UseTls when (UseTls==true) or (UseTls==optional) ->
       {true, false};
       UseTls when (UseTls==required) or (UseTls==required_trusted) ->
       {true, true}
       end,
       UseV10 = TLS,
       TLSOpts = case ejabberd_config:get_local_option(s2s_certfile) of
       undefined ->
       [connect];
       CertFile ->
       [{certfile, CertFile}, connect]
       end,*)
    let tls = false in
    let tls_required = false in
    let use_v10 = tls in
    let tls_opts = [Connect] in
    let xml_receiver = XMLReceiver.create self in
    let new', verify' =
      (match start_type with
	 | `New key ->
	     Some key, None
	 | `Verify (pid, key, sid) ->
	     start_connection self;
	     None, Some (pid, key, sid)) in
      (* Timer = erlang:start_timer(?S2STIMEOUT, self(), []), *)
    let state = {pid = self;
		 socket = None;
		 xml_receiver;
		 state = Open_socket;
		 streamid = new_id ();
		 use_v10 = use_v10;
		 tls = tls;
		 tls_required = tls_required;
		 tls_enabled = false;
		 tls_options = tls_opts;
		 authenticated = false;
		 db_enabled = true;
		 try_auth = true;
		 myname = from;
		 server = server;
		 queue = Queue.create ();
		 delay_to_retry = 0;
		 new' = new';
		 verify = verify'}
    in Lwt.return state

  let outgoing_s2s_timeout () =
    (* case ejabberd_config:get_local_option(outgoing_s2s_options) of
       {_, Timeout} when is_integer(Timeout) ->
       Timeout;
       {_, infinity} ->
       infinity;
       undefined ->
       %% 10 seconds
       10000
       end. *)
    10000

  let open_socket2 addr' port self =
    let timeout = outgoing_s2s_timeout () in
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let addr = Unix.ADDR_INET (addr', port) in
    lwt () = Lwt_unix.connect socket addr in
    let tcpsock = Tcp.of_fd socket self in
      Tcp.activate tcpsock self;
      Lwt.return tcpsock

  let open_socket1 = open_socket2
    (* open_socket1({_,_,_,_} = Addr, Port) ->
       open_socket2(inet, Addr, Port);

       %% IPv6
       open_socket1({_,_,_,_,_,_,_,_} = Addr, Port) ->
       open_socket2(inet6, Addr, Port);

       %% Hostname
       open_socket1(Host, Port) ->
       lists:foldl(fun(_Family, {ok, _Socket} = R) ->
       R;
       (Family, _) ->
       Addrs = get_addrs(Host, Family),
       lists:foldl(fun(_Addr, {ok, _Socket} = R) ->
       R;
       (Addr, _) ->
       open_socket1(Addr, Port)
       end, ?SOCKET_DEFAULT_RESULT, Addrs)
       end, ?SOCKET_DEFAULT_RESULT, outgoing_s2s_families()). *)

  let log_s2s_out new' myname server tls =
    (* Human readable S2S logging: Log only new outgoing connections as notice *)
    if new' = None then (
      (* Do not log dialback *)
      Lwt.return ()
    ) else (
      (* Log new outgoing connections: *)
      Lwt_log.notice_f ~section
	"Trying to open s2s connection: %s -> %s with TLS=%B"
	myname server tls
    )

  let get_addr_port ascii_addr =
    []

  let close_socket state =
    match state.socket with
      | Some tcpsock ->
	  Tcp.close tcpsock
      | None ->
	  Lwt.return ()

  let open_socket msg state =
    match msg with
      | `Init ->
	  lwt () = log_s2s_out state.new'
	    (state.myname :> string) (state.server :> string) state.tls in
	  lwt () = Lwt_log.debug_f ~section
            "open_socket: %s -> %s"
            (* TODO "new = %s, verify = %s" *)
	    state.myname state.server in
	  let addr_list =
	    (try
	       let ascii_addr = Idna.domain_utf8_to_ascii (state.server :> string) in
		 get_addr_port ascii_addr
	     with
	       | _ -> []) in
	    (match (List.fold_left
		      (fun (addr, port) acc ->
			 match acc with
			   | Some socket ->
			       Some socket
			   | None -> (
			       try_lwt
				 lwt s = open_socket1 addr port state.pid in
				   Some s
			       with | _ -> None))
		      addr_list None) with
	       | Some socket ->
		   let version = match state.use_v10 with
		     | true -> " version='1.0'"
		     | false -> "" in
		   let new_state = {state with socket = Some socket;
	                              tls_enabled = false;
				      stream_id = new_id();
				      state = Wait_for_stream} in
		     send_text new_state (stream_header state.myname state.server version);
		     Lwt.return (`Continue new_state) (* ?FSMTIMEOUT *)
	       | None ->
		   lwt () = Lwt_log.notice_f ~section
		     "s2s connection: %s -> %s (remote server not found)"
		     state.myname state.server in
		     (* case ejabberd_hooks:run_fold(find_s2s_bridge,
			undefined,
			[StateData#state.myname,
			StateData#state.server]) of
			{Mod, Fun, Type} ->
			?INFO_MSG("found a bridge to ~s for: ~s -> ~s",
			[Type, StateData#state.myname,
			StateData#state.server]),
			NewStateData = StateData#state{bridge={Mod, Fun}},
			{next_state, relay_to_bridge, NewStateData};
		       _ ->
			wait_before_reconnect(StateData) *)
		     wait_before_reconnect state)
      | `Closed ->
	  lwt () = Lwt_log.notice_f ~section
	    "s2s connection: %s -> %s (stopped in open socket)"
	    state.myname state.server in
            Lwt.return (`Stop state)
      | `Timeout ->
	  lwt () = Lwt_log.notice_f ~section
	    "s2s connection: %s -> %s (timeout in open socket)"
	    state.myname state.server in
            Lwt.return (`Stop state)

  let wait_for_stream msg state =
    match msg with
      | `XmlStreamStart (name, attrs) -> (
	  match (Xml.get_attr_s "xmlns" attrs,
		 Xml.get_attr_s "xmlns:db" attrs,
		 Xml.get_attr_s "version" attrs = "1.0") with
	    | "jabber:server", "jabber:server:dialback", false ->
		send_db_request state
	    | "jabber:server", "jabber:server:dialback", true when state.use_v10 ->
		Lwt.return (`Continue {state with state = Wait_for_features}) (* ?FSMTIMEOUT *)
	    | "jabber:server", "jabber:server:dialback", true when not state.use_v10 ->
		(* Clause added to handle Tigase's workaround for an old ejabberd bug *)
		send_db_request state
	    | "jabber:server", "", true when state.use_v10 ->
		Lwt.return (`Continue {state with db_enabled = false;
	                                 state = Wait_for_features}) (* ?FSMTIMEOUT *)
	    | ns_provided, db, _ ->
		send_element state Jlib.serr_invalid_namespace;
		lwt () = Lwt_log.notice_f ~section
 		  "Closing s2s connection: %s -> %s (invalid namespace):"
		  ^ "namespace provided = %s, "
		  ^ "namespace expected = \"jabber:server\", "
		  ^ "xmlns:db provided = %s"
		  state.myname state.server ns_provided db in
		  Lwt.return (`Stop state))
      | `XmlStreamError _ ->
	  send_element state Jlib.serr_xml_not_well_formed;
	  send_trailer state;
	  lwt () = Lwt_log.notice_f ~section
            "Closing s2s connection: %s -> %s (invalid xml)"
	    state.myname state.server in
            Lwt.return (`Stop state)
      | `XmlStreamEnd _ ->
	  lwt () = Lwt_log.notice_f ~section
	    "Closing s2s connection: %s -> %s (xmlstreamend)"
	    state.myname state.server in
	    Lwt.return (`Stop state)
      | `Timeout ->
	  lwt () = Lwt_log.notice_f ~section
	    "Closing s2s connection: %s -> %s (timeout in wait_for_stream)"
	    state.myname state.server in
	    Lwt.return (`Stop state)
      | `Closed ->
	  lwt () = Lwt_log.notice_f ~section
	    "Closing s2s connection: ~s -> ~s (close in wait_for_stream)"
	    state.myname state.server in
	    Lwt.return (`Stop state)

  let wait_for_validation msg state =
    match msg with
      | `XmlStreamElement el -> (
	  match is_verify_res el with
	    | Result (to', from, id, type') ->
		lwt () = Lwt_log.debug_f ~section
		  "recv result: from = %s, to = %s, id = %, type = %s"
		  from to' id type' in
		  (match (type', state.tls_enabled, state.tls_required) with
		     | "valid", enabled, required when (enabled = true || required = false) ->
			 send_queue state state.queue;
			 lwt () = Lwt_log.notice_f ~section
			   "Connection established: %s -> %s with TLS=%B"
			   state.myname state.servr state.tls_enabled in
			   (* ejabberd_hooks:run(s2s_connect_hook,
			      [StateData#state.myname,
			      StateData#state.server]), *)
			   Lwt.return (`Continue {state
						  with state = Stream_established;
						    queue = Queue.create ()}) 
		     | _ ->
			 (* TODO: bounce packets *)
			 lwt () = Lwt_log.notice_f ~section
			   "Closing s2s connection: %s -> %s (invalid dialback key)"
			   state.myname state.server in
			   Lwt.return (`Stop state))
	    | Verify (to', from, id, type') ->
		lwt () = Lwt_log.debug_f ~section
		  "recv verify: from = %s, to = %s, id = %, type = %s"
		  from to' id type' in
		  (match state.verify with
		     | None ->
			 (* TODO: Should'nt we close the connection here ? *)
			 let next_state = Wait_for_validation in
			   Lwt.return (`Continue {state with state = next_state})
			     (* get_timeout_interval(NextState) *)
		     | Some (pid, _key, _sid) ->
			 (match type' with
			    | "valid" ->
				pid $! (`Valid (state.server, state.myname))
			    | _ ->
				pid $! (`Invalid (state.server, state.myname)));
			 if state.verify = None then (
			   Lwt.return (`Stop state)
			 ) else (
			   let next_state = Wait_for_validation in
			     Lwt.return (`Continue {state with state = next_state})
			       (* get_timeout_interval(NextState) *)
			 ))
	    | None ->
		Lwt.return (`Continue state)) (* ?FSMTIMEOUT *)
      | `XmlStreamEnd _ ->
	  lwt () = Lwt_log.notice_f ~section
	    "wait for validation: %s -> %s (xmlstreamend)"
	    state.myname state.server in
	    Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	  lwt () = Lwt_log.notice_f ~section
	    "wait for validation: %s -> %s (xmlstreamerror)"
	    state.myname state.server in
	    send_element state Jlib.serr_xml_not_well_formed;
	    send_trailer state;
	    Lwt.return (`Stop state)
      | `Timeout -> (
	  match state.verify with
	    | Some (vpid, vkey, sid) ->
		(* This is an auxiliary s2s connection for dialback.
		   This timeout is normal and doesn't represent a problem. *)
		lwt () = Lwt_log.debug_f ~section
		  "wait_for_validation: %s -> %s (timeout in verify connection)"
		  state.myname state.server in
		  Lwt.return (`Stop state)
	    | None ->
		lwt () = Lwt_log.notice_f ~section
		  "wait_for_validation: %s -> %s (connect timeout)"
		  state.myname state.server in
		  Lwt.return (`Stop state))
      | `Closed ->
	  lwt () = Lwt_log.notice_f ~section
	    "wait for validation: ~s -> ~s (closed)"
	    state.myname state.server in
	    Lwt.return (`Stop state)
	      
  let wait_for_features msg state =
    match msg with
      | `XmlStreamElement el -> (
	  match el with
	    | `XmlElement ("stream:features", _attrs, els) -> (
		let sasl_ext, start_tls, start_tls_required =
		  List.fold_left
		    (fun el' acc -> (
		       match el' with
			 | `XmlElement ("mechanisms", attrs1, els1) ->
			     let (_sext, stls, stlsreq) = acc in (
				 match Xml.get_attr_s "xmlns" attrs1 with
				   | <:ns<SASL>> ->
				     let new_sext = List.exist
				       (function
					  | `XmlElement ("mechanism", _, els2) ->
					      (match Xml.get_cdata els2 with
						 | "EXTERNAL" -> true
						 | _ -> false)
					  | _ ->
					      false) els1
				     in
				       (new_sext, stls, stlsreq)
				   | _ ->
				       acc)
			 | `XmlElement ("starttls", attrs1, _els1) as el1 ->
			     let (sext, _stls, _stlsreq) = acc in (
				 match Xml.get_attr_s "xmlns" attrs1 with
				   | <:ns<TLS>> ->
				     let req = (
				       match Xml.get_subtag el1 "required" with
					 | Some _ -> true;
					 | None -> false) in
				       (sext, true, req)
				   | _ ->
				       acc)
			 | _ ->
			     acc))
		    (false, false, false) els
		in
		  if (not sasl_ext) && (not start_tls) && state.authenticated then (
		    send_queue state state.queue;
		    lwt () = Lwt_log.notice_f ~section
		      "Connection established: ~s -> ~s"
		      state.myname state.server in
		      (* ejabberd_hooks:run(s2s_connect_hook,
				       [StateData#state.myname,
					StateData#state.server]), *)
		      Lwt.return (`Continue {state with
					       state = Stream_established;
					       queue = Queue.create()})
		  ) else if sasl_ext && state.try_auth && state.new' <> None then (
		    send_element state
		      (`XmlElement ("auth",
				    [("xmlns", <:ns<SASL>>);
				     ("mechanism", "EXTERNAL")],
				    [`XmlCdata (Jlib.encode_base64
						  state.myname)]));
		    Lwt.return (`Continue {state with
					     state = Wait_for_auth_result;
					     try_auth = false}) (* ?FSMTIMEOUT *)
		  ) else if start_tls && state.tls && (not state.tls_enabled) then (
		    send_element state
		      (`XmlElement ("starttls", [("xmlns", <:ns<SASL>>)], []));
		    Lwt.return (`Continue {state with
					     state = Wait_for_starttls_proceed}) (* ?FSMTIMEOUT *)
		  ) else if start_tls_required && (not state.tls) then (
		    lwt () = Lwt_log.debug_f ~section
		      "restarted: %s -> %s"
		      state.myname state.server in
		      close_socket state;
		      (* ejabberd_socket:close(StateData#state.socket), *)
		      Lwt.return (`Continue {state with
					       state = reopen_socket;
					       use_v10 = false;
					       socket = None}) (* ?FSMTIMEOUT *)
		  ) else if db_enabled then (
		    send_db_request state
		  ) else (
		    lwt () = Lwt_log.debug_f ~section
		      "restarted: %s -> %s"
		      state.myserver state.server in
		      (* TODO: clear message queue *)
		      close_socket state;
		      Lwt.return (`Continue {state with
					       socket = None;
					       use_v10 = false;
					       state = Reopen_socket}))) (* ?FSMTIMEOUT *)
	    | _ ->
		send_element state Jlib.serr_bad_format;
		send_trailer;
		lwt () = Lwt_log.notice_f ~section
		  "Closing s2s connection: ~s -> ~s (bad format)"
		  state.myname state.server in
		  Lwt.return (`Stop state))
      | `XmlStreamEnd _ ->
	  lwt () = Lwt_log.notice ~section
	    "wait_for_features: xmlstreamend" in
	    Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	  send_element state Jlib.serr_xml_not_well_formed;
	  send_trailer state;
	  lwt () = Lwt_log.notice ~section
	    "wait_for_features: xmlstreamerror" in
	    Lwt.return (`Stop state)
      | `Timeout ->
	  lwt () = Lwt_log.notice ~section
	    "wait_for_features: timeout" in
	    Lwt.return (`Stop state)
      | `Closed ->
	  lwt () = Lwt_log.notice ~section
	    "wait_for_features: closed" in
	    Lwt.return (`Stop state)

  let wait_for_auth_result msg state =
    match msg with
      | `XmlStreamElement (`XmlElement ("success", attrs, _els)) -> (
	  match Xml.get_attr_s "xmlns" attrs with
	    | <:ns<SASL>> ->
	      lwt () = Lwt_log.debug_f ~section
		"auth: %s -> %s"
		state.myname state.server in
		XMLReceiver.reset_stream state.xml_receiver;
		send_element state
		  (stream_header state.myname state.server " version='1.0'");
		Lwt.return (`Continue {state with
					 state = Wait_for_stream;
					 stream_id = new_id();
					 authenticated = true}) (* ?FSMTIMEOUT *)
	    | _ ->
		send_element state Jlib.serr_bad_format;
		send_trailer state;
		lwt () = Lwt_log.notice_f ~section
		  "Closing s2s connection: %s -> %s (bad format)"
		  state.myname state.server in
		  Lwt.return (`Stop state))
      | `XmlStreamElement (`XmlElement ("failure", attrs, _els)) -> (
	  match Xml.get_attr_s "xmlns" attrs with
	    | <:ns<SASL>> ->
	      lwt () = Lwt_log.debug_f ~section
		"restarted: %s -> %s"
		state.myname state.server in
		close_socket state;
		Lwt.return (`Continue {state with
					 state = Reopen_socket;
					 socket = None}) (* ?FSMTIMEOUT *)
	    | _ ->
		send_element state Jlib.serr_bad_format;
		send_trailer state;
		lwt () = Lwt_log.notice_f ~section
		  "Closing s2s connection: %s -> %s (bad format)"
		  state.myname state.server in
		  Lwt.return (`Stop state))
      | `XmlStreamElement _ ->
	  send_element state Jlib.serr_bad_format;
	  send_trailer state;
	  lwt () = Lwt_log.notice_f ~section
	    "Closing s2s connection: %s -> %s (bad format)"
	    state.myname state.server in
	    Lwt.return (`Stop state)
      | `XmlStreamEnd _ ->
	  lwt () = Lwt_log.notice ~section
	    "wait for auth result: xmlstreamend" in
	    Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	  send_element state Jlib.serr_xml_not_well_formed;
	  send_trailer state;
	  lwt () = Lwt_log.notice ~section
	    "wait for auth result: xmlstreamerror" in
	    Lwt.return (`Stop state)
      | `Timeout ->
	  lwt () = Lwt_log.notice ~section
	    "wait for auth result: timeout" in
	    Lwt.return (`Stop state)
      | `Closed ->
	  lwt () = Lwt_log.notice ~section
	    "wait for auth result: closed" in
	    Lwt.return (`Stop state)

  let wait_for_starttls_proceed msg state =
    match msg with
      | `XmlStreamElement (`XmlElement ("proceed", attrs, _els)) -> (
	  match Xml.get_attr_s "xmlns" attrs with
	    | <:ns<TLS>> ->
	      lwt () = Lwt_log.debug_f ~section
		"starttls: %s -> %s"
		state.myname state.server in
		(* Socket = StateData#state.socket,
		    TLSOpts = case ejabberd_config:get_local_option(
				     {domain_certfile,
				      StateData#state.myname}) of
				  undefined ->
				      StateData#state.tls_options;
				  CertFile ->
				      [{certfile, CertFile} |
				       lists:keydelete(
					 certfile, 1,
					 StateData#state.tls_options)]
			      end,
		    TLSSocket = ejabberd_socket:starttls(Socket, TLSOpts),
		    NewStateData = StateData#state{socket = TLSSocket,
						   streamid = new_id(),
						   tls_enabled = true,
						   tls_options = TLSOpts
						  },
		    send_text(NewStateData,
			      io_lib:format(?STREAM_HEADER,
					    [StateData#state.myname, StateData#state.server,
					     " version='1.0'"])),
		    {next_state, wait_for_stream, NewStateData, ?FSMTIMEOUT}; *)
	      lwt () = Lwt_log.error_f ~section
		"don't support starttls yet" in
		Lwt.return (`Stop state)
	    | _ ->
		send_element state Jlib.serr_bad_format;
		send_trailer state;
		lwt () = Lwt_log.notice_f ~section
		  "Closing s2s connection: %s -> %s (bad format)"
		  state.myname state.server in
		  Lwt.return (`Stop state))
      | `XmlStreamElement _ ->
	  lwt () = Lwt_log.notice_f ~section
	    "Closing s2s connection: %s -> %s (bad format)"
	    state.myname state.server in
	    Lwt.return (`Stop state)
      | `XmlStreamEnd _ ->
	  lwt () = Lwt_log.notice ~section
	    "wait for starttls proceed: xmlstreamend" in
	    Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	  send_element state Jlib.serr_xml_not_well_formed;
	  send_trailer state;
	  lwt () = Lwt_log.notice ~section
	    "wait for starttls proceed: xmlstreamerror" in
	    Lwt.return (`Stop state)
      | `Timeout ->
	  lwt () = Lwt_log.notice ~section
	    "wait for starttls proceed: timeout" in
	    Lwt.return (`Stop state)
      | `Closed ->
	  lwt () = Lwt_log.notice ~section
	    "wait for starttls proceed: closed" in
	    Lwt.return (`Stop state)

  let reopen_socket msg state =
    match msg with
      | `XmlStreamElement _ ->
	  Lwt.return (`Continue state) (* ?FSMTIMEOUT *)
      | `XmlStreamEnd _ ->
	  Lwt.return (`Continue state) (* ?FSMTIMEOUT *)
      | `XmlStreamError _ ->
	  Lwt.return (`Continue state) (* ?FSMTIMEOUT *)
      | `Timeout ->
	  lwt () = Lwt_log.notice ~section
	    "reopen socket: timeout" in
	    Lwt.return (`Stop state)
      | `Closed ->
	  start_connection state.pid;
	  Lwt.return (`Continue {state with state = Open_socket}) (* ?FSMTIMEOUT *)

  let wait_before_retry msg state =
    (* This state is use to avoid reconnecting to often to bad sockets *)
    Lwt.return (`Continue state) (* ?FSMTIMEOUT *)

  (* relay_to_bridge(stop, StateData) ->
     wait_before_reconnect(StateData);
     relay_to_bridge(closed, StateData) ->
     ?INFO_MSG("relay to bridge: ~s -> ~s (closed)",
     [StateData#state.myname, StateData#state.server]),
     {stop, normal, StateData};
     relay_to_bridge(_Event, StateData) ->
     {next_state, relay_to_bridge, StateData}. *)

  let stream_established msg state =
    match msg with
      | `XmlStreamElement el ->
	  lwt () = Lwt_log.debug ~section
	    "s2s stream established" in
	    (match is_verify_res el with
	       | Verify (vto, vfrom, vid, vtype) ->
		   lwt () = Lwt_log.debug_f ~section
		     "recv verify: to = %s, from = %s, id = %s, type = %s"
		     vto vfrom vid vtype in
		     (match state.verify with
			| Some (vpid, _vkey, _sid) when vtype = "valid" ->
			    vpid $! (`Valid (state.server, state.myname))
			| _ ->
			    vpid $! (`Invalid (state.server, state.myname)));
		     Lwt.return (`Continue state)
	       | _ ->
		   Lwt.return (`Continue state))
      | `XmlStreamEnd _ ->
	  lwt () = Lwt_log.notice_f ~section
	    "Connection closed in stream established: %s -> %s (xmlstreamend)"
	    state.myname state.server in
	    Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	  send_element state Jlib.serr_xml_not_well_formed;
	  send_trailer state;
	  lwt () = Lwt_log.notice_f ~section
	    "stream established: %s -> %s (xmlstreamerror)"
	    state.myname state.server in
	    Lwt.return (`Stop state)
      | `Timeout ->
	  lwt () = Lwt_log.notice_f ~section
	    "stream established: %s -> %s (timeout)"
	    state.myname state.server in
	    Lwt.return (`Stop state)
      | `Closed ->
	  lwt () = Lwt_log.notice_f ~section
	    "stream established: %s -> %s (closed)"
	    state.myname state.server in
	    Lwt.return (`Stop state)

  let handle' msg state =
    match state.state with
      | Open_socket -> open_socket msg state
      | Wait_for_stream -> wait_for_stream msg state
      | Wait_for_validation -> wait_for_validation msg state
      | Wait_for_features -> wait_for_features msg state
      | Wait_for_auth_result -> wait_for_auth_result msg state
      | Wait_for_starttls_proceed -> wait_for_starttls_proceed msg state
	  (* | Relay_to_bridge -> relay_to_bridge msg state *)
      | Reopen_socket -> reopen_socket msg state
      | Wait_before_retry -> wait_before_retry msg state
      | Stream_established -> stream_established msg state

  let handle msg state =
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
      | `Init as m
      | `Closed as m
      | `Timeout as m
      | `Send_element el as m
      | #XMLReceiver.msg as m ->
	  handle' m state
      | #GenServer.msg -> assert false
			  
end

module S2SOutServer = GenServer.Make(S2SOut)
