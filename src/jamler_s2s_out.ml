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
module S2SLib = Jamler_s2s_lib

module S2SOut :
sig
  type start_type =
      [ `New of string
      | `Verify of (Jamler_s2s_lib.validation_msg pid * string * string)
      ]
  include GenServer.Type with
    type msg =
        [ Socket.msg | XMLReceiver.msg | GenServer.msg
        | unit timer_msg
        | Jamler_s2s_lib.s2s_out_msg ]
    and type init_data = Jlib.namepreped * Jlib.namepreped * start_type
    and type stop_reason = GenServer.reason
  val start_connection : Jamler_s2s_lib.s2s_out_msg pid -> unit
  val stop_connection : Jamler_s2s_lib.s2s_out_msg pid -> unit
end =
struct
  type msg =
      [ Socket.msg | XMLReceiver.msg | GenServer.msg
      | unit timer_msg
      | Jamler_s2s_lib.s2s_out_msg ]

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
      [ `New of string
      | `Verify of (Jamler_s2s_lib.validation_msg pid * string * string)
      ]

  type init_data = Jlib.namepreped * Jlib.namepreped * start_type

  type stop_reason = GenServer.reason

  type state =
      {pid: msg pid;
       socket : Socket.socket option;
       xml_receiver : XMLReceiver.t;
       state: s2s_out_state;
       stream_id : string;
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
       delay_to_retry: float;
       new' : string option;
       verify : (Jamler_s2s_lib.validation_msg pid * string * string) option;
       timer : timer;
	 (* bridge *)}

  let new_id () =
    Jlib.get_random_string ()

  let s2stimeout = 600.0

  let send_text state text =
    match state.socket with
      | Some socket ->
	  Socket.send_async socket text
      | None -> assert false

  let send_element state el =
    send_text state (Xml.element_to_string el)

  let send_queue state =
    Queue.iter (send_element state) state.queue

(* Bounce a single message (xmlelement) *)
  let bounce_element el error =
    let `XmlElement (_name, attrs, _subtags) = el in
      match Xml.get_attr_s "type" attrs with
	| "error"
	| "result" -> ()
	| _ ->
	    let err = Jlib.make_error_reply el error in
	    let from = Jlib.string_to_jid_exn (Xml.get_tag_attr_s "from" el) in
	    let to' = Jlib.string_to_jid_exn (Xml.get_tag_attr_s "to" el) in
	      Jamler_router.route to' from err

  let start_connection pid =
    pid $! `Init

  let stop_connection pid =
    pid $! `Closed

  let stream_header_fmt =
    "<?xml version='1.0'?>" ^^
      "<stream:stream " ^^
      "xmlns:stream='http://etherx.jabber.org/streams' " ^^
      "xmlns='jabber:server' " ^^
      "xmlns:db='jabber:server:dialback' " ^^
      "from='%s' " ^^
      "to='%s'%s>"

  let stream_header id server version =
    Printf.sprintf stream_header_fmt id server version

  let send_trailer state =
    let stream_trailer = "</stream:stream>" in
    send_text state stream_trailer

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

  let enqueue el state =
    Queue.add el state.queue;
    Lwt.return (`Continue state) (* get_timeout_interval(StateName) *)

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
    let timer =
      start_timer
	s2stimeout
	(self :> unit timer_msg pid)
	()
    in
    let state = {pid = self;
		 socket = None;
		 xml_receiver;
		 state = Open_socket;
		 stream_id = new_id ();
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
		 delay_to_retry = 0.0;
		 new' = new';
		 verify = verify';
		 timer = timer}
    in
      Lwt.return (`Continue state)

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
    lwt () = Lwt_unix.connect socket addr in (* TODO *)
    let tcpsock = Socket.of_fd socket self in
      Socket.activate tcpsock self;
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
	"trying to open s2s connection: %s -> %s with tls=%B"
	myname server tls
    )

  let get_addr_port ascii_addr =
    (* TODO: srv *)
    lwt h = Lwt_lib.gethostbyname ascii_addr in
    let res =
      List.map
	(fun addr -> (addr, 5269))
	(Array.to_list h.Unix.h_addr_list)
    in
      Lwt.return res

(* Maximum delay to wait before retrying to connect after a failed attempt.
   Specified in miliseconds. Default value is 5 minutes. *)
  let max_retry_delay = 300.0

(* @doc Get the maximum allowed delay for retry to reconnect (in miliseconds).
   The default value is 5 minutes.
   The option {s2s_max_retry_delay, Seconds} can be used (in seconds).
   @spec () -> integer() *)
  let get_max_retry_delay () =
    (*case ejabberd_config:get_local_option(s2s_max_retry_delay) of
	Seconds when is_integer(Seconds) ->
	    Seconds*1000;
	_ ->*)
    max_retry_delay

(* This function is intended to be called at the end of a state
   function that want to wait for a reconnect delay before stopping. *)
  let wait_before_reconnect state =
    (* TODO *)
    (* bounce queue manage by process and Erlang message queue *)
    (*bounce_queue(StateData#state.queue, ?ERR_REMOTE_SERVER_NOT_FOUND),
    bounce_messages(?ERR_REMOTE_SERVER_NOT_FOUND),*)
    cancel_timer state.timer;
    let delay =
      match state.delay_to_retry with
	| 0.0 ->
	    (* The initial delay is random between 1 and 15 seconds
	       Return a random integer between 1000 and 15000 *)
	    Random.float 14.0 +. 1.0
	| delay ->
	    (* Duplicate the delay with each successive failed
	       reconnection attempt, but don't exceed the max *)
	    min (delay *. 2.0) (get_max_retry_delay ())
    in
    let timer =
      start_timer
	delay
	(state.pid :> unit timer_msg pid)
	()
    in
      Lwt.return
	(`Continue {state with
		      state = Wait_before_retry;
		      timer;
		      delay_to_retry = delay;
		      queue = Queue.create ();})

  let send_db_request state =
    let server = state.server in
    let new' =
      match state.new' with
	| None -> (
	    match (S2SLib.try_register (state.myname, server)
		     (state.pid :> Jamler_s2s_lib.s2s_out_msg pid)) with
	      | Some key ->
		  Some key
	      | None ->
		  None
	  )
	| Some key ->
	    Some key
    in
    let state = {state with new'} in
      (match new' with
	 | None -> ()
	 | Some key ->
	     send_element state
	       (`XmlElement ("db:result",
			     [("from", (state.myname :> string));
			      ("to", (server :> string))],
			     [`XmlCdata key]))
      );
      (match state.verify with
	 | None -> ()
	 | Some (_pid, key, sid) ->
	     send_element state
	       (`XmlElement ("db:verify",
			     [("from", (state.myname :> string));
			      ("to", (state.server :> string));
			      ("id", sid)],
			     [`XmlCdata key]))
      );
      Lwt.return (`Continue {state with state = Wait_for_validation}) (* ?FSMTIMEOUT*6 *)


  let close_socket state =
    match state.socket with
      | Some tcpsock ->
	  Socket.close tcpsock
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
	  lwt addr_list =
	    (try_lwt
	       let ascii_addr = Idna.domain_utf8_to_ascii (state.server :> string) in
		 get_addr_port ascii_addr
	     with
	       | _ -> Lwt.return [])
	  in
	  lwt open_res =
	    Lwt_list.fold_left_s
	      (fun acc (addr, port) ->
		 match acc with
		   | Some socket ->
		       Lwt.return acc
		   | None -> (
		       try_lwt
			 lwt s = open_socket1 addr port state.pid in
			   Lwt.return (Some s)
		       with
			 | _ -> Lwt.return None
		     )
	      ) None addr_list
	  in
	    (match open_res with
	       | Some socket ->
		   let version = match state.use_v10 with
		     | true -> " version='1.0'"
		     | false -> "" in
		   let new_state = {state with socket = Some socket;
	                              tls_enabled = false;
				      stream_id = new_id();
				      state = Wait_for_stream} in
		     send_text new_state
		       (stream_header (state.myname :> string)
			  (state.server :> string) version);
		     Lwt.return (`Continue new_state) (* ?FSMTIMEOUT *)
	       | None ->
		   lwt () = Lwt_log.notice_f ~section
		     "s2s connection: %s -> %s (remote server not found)"
		     (state.myname :> string) (state.server :> string)
		   in
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
		     wait_before_reconnect state
	    )
      | `Closed ->
	  lwt () = Lwt_log.notice_f ~section
	    "s2s connection: %s -> %s (stopped in open socket)"
	    (state.myname :> string) (state.server :> string) in
            Lwt.return (`Stop state)
      | `Timeout ->
	  lwt () = Lwt_log.notice_f ~section
	    "s2s connection: %s -> %s (timeout in open socket)"
	    (state.myname :> string) (state.server :> string) in
            Lwt.return (`Stop state)
      | `Send_element el -> enqueue el state
      | `XmlStreamElement _
      | `XmlStreamEnd _
      | `XmlStreamError _
      | `XmlStreamStart _ -> assert false

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
 		  ("closing s2s connection: %s -> %s (invalid namespace):"
		  ^^ "namespace provided = %s, "
		  ^^ "namespace expected = \"jabber:server\", "
		  ^^ "xmlns:db provided = %s")
		  (state.myname :> string)
		  (state.server :> string)
		  ns_provided
		  db
		in
		  Lwt.return (`Stop state))
      | `XmlStreamError _ ->
	  send_element state Jlib.serr_xml_not_well_formed;
	  send_trailer state;
	  lwt () = Lwt_log.notice_f ~section
            "closing s2s connection: %s -> %s (invalid xml)"
	    (state.myname :> string) (state.server :> string)
	  in
            Lwt.return (`Stop state)
      | `XmlStreamEnd _ ->
	  lwt () = Lwt_log.notice_f ~section
	    "closing s2s connection: %s -> %s (xmlstreamend)"
	    (state.myname :> string) (state.server :> string) in
	    Lwt.return (`Stop state)
      | `Timeout ->
	  lwt () = Lwt_log.notice_f ~section
	    "closing s2s connection: %s -> %s (timeout in wait_for_stream)"
	    (state.myname :> string) (state.server :> string) in
	    Lwt.return (`Stop state)
      | `Closed ->
	  lwt () = Lwt_log.notice_f ~section
	    "closing s2s connection: %s -> %s (close in wait_for_stream)"
	    (state.myname :> string) (state.server :> string) in
	    Lwt.return (`Stop state)
      | `Send_element el -> enqueue el state
      | `Init
      | `XmlStreamElement _ -> assert false

  let wait_for_validation msg state =
    match msg with
      | `XmlStreamElement el -> (
	  match is_verify_res el with
	    | Result (to', from, id, type') ->
		lwt () = Lwt_log.debug_f ~section
		  "recv result: from = %s, to = %s, id = %, type = %s"
		  from to' id type'
		in
		  (match (type', state.tls_enabled, state.tls_required) with
		     | "valid", enabled, required when (enabled = true || required = false) ->
			 send_queue state;
			 lwt () = Lwt_log.notice_f ~section
			   "connection established: %s -> %s with tls=%B"
			   (state.myname :> string)
			   (state.server :> string) state.tls_enabled in
			   (* ejabberd_hooks:run(s2s_connect_hook,
			      [StateData#state.myname,
			      StateData#state.server]), *)
			   Lwt.return
			     (`Continue
				{state with
				   state = Stream_established;
				   queue = Queue.create ()})
		     | _ ->
			 (* TODO: bounce packets *)
			 lwt () = Lwt_log.notice_f ~section
			   "closing s2s connection: %s -> %s (invalid dialback key)"
			   (state.myname :> string)
			   (state.server :> string)
			 in
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
	    | False ->
		Lwt.return (`Continue state)) (* ?FSMTIMEOUT *)
      | `XmlStreamEnd _ ->
	  lwt () = Lwt_log.notice_f ~section
	    "wait for validation: %s -> %s (xmlstreamend)"
	    (state.myname :> string) (state.server :> string)
	  in
	    Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	  lwt () = Lwt_log.notice_f ~section
	    "wait for validation: %s -> %s (xmlstreamerror)"
	    (state.myname :> string) (state.server :> string)
	  in
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
		  (state.myname :> string) (state.server :> string) in
		  Lwt.return (`Stop state))
      | `Closed ->
	  lwt () = Lwt_log.notice_f ~section
	    "wait for validation: %s -> %s (closed)"
	    (state.myname :> string) (state.server :> string)
	  in
	    Lwt.return (`Stop state)
      | `Send_element el -> enqueue el state
      | `Init
      | `XmlStreamStart _ -> assert false

  let wait_for_features msg state =
    match msg with
      | `XmlStreamElement el -> (
	  match el with
	    | `XmlElement ("stream:features", _attrs, els) -> (
		let sasl_ext, start_tls, start_tls_required =
		  List.fold_left
		    (fun acc el' -> (
		       match el' with
			 | `XmlElement ("mechanisms", attrs1, els1) ->
			     let (_sext, stls, stlsreq) = acc in (
				 match Xml.get_attr_s "xmlns" attrs1 with
				   | <:ns<SASL>> ->
				     let new_sext = List.exists
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
		    send_queue state;
		    lwt () = Lwt_log.notice_f ~section
		      "connection established: %s -> %s"
		      (state.myname :> string) (state.server :> string)
		    in
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
						  (state.myname :> string))]));
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
		      state.myname state.server
		    in
		    lwt () = close_socket state in
		      Lwt.return (`Continue {state with
					       state = Reopen_socket;
					       use_v10 = false;
					       socket = None}) (* ?FSMTIMEOUT *)
		  ) else if state.db_enabled then (
		    send_db_request state
		  ) else (
		    lwt () = Lwt_log.debug_f ~section
		      "restarted: %s -> %s"
		      state.myserver state.server in
		      (* TODO: clear message queue *)
		    lwt () = close_socket state in
		      Lwt.return (`Continue {state with
					       socket = None;
					       use_v10 = false;
					       state = Reopen_socket}))) (* ?FSMTIMEOUT *)
	    | _ ->
		send_element state Jlib.serr_bad_format;
		send_trailer state;
		lwt () = Lwt_log.notice_f ~section
		  "closing s2s connection: %s -> %s (bad format)"
		  (state.myname :> string) (state.server :> string)
		in
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
      | `Send_element el -> enqueue el state
      | `Init
      | `XmlStreamStart _ -> assert false

  let wait_for_auth_result msg state =
    match msg with
      | `XmlStreamElement (`XmlElement ("success", attrs, _els)) -> (
	  match Xml.get_attr_s "xmlns" attrs with
	    | <:ns<SASL>> ->
	      lwt () = Lwt_log.debug_f ~section
		"auth: %s -> %s"
		state.myname state.server in
		XMLReceiver.reset_stream state.xml_receiver;
		send_text state
		  (stream_header
		     (state.myname :> string)
		     (state.server :> string)
		     " version='1.0'");
		Lwt.return (`Continue {state with
					 state = Wait_for_stream;
					 stream_id = new_id();
					 authenticated = true}) (* ?FSMTIMEOUT *)
	    | _ ->
		send_element state Jlib.serr_bad_format;
		send_trailer state;
		lwt () = Lwt_log.notice_f ~section
		  "closing s2s connection: %s -> %s (bad format)"
		  (state.myname :> string) (state.server :> string)
		in
		  Lwt.return (`Stop state))
      | `XmlStreamElement (`XmlElement ("failure", attrs, _els)) -> (
	  match Xml.get_attr_s "xmlns" attrs with
	    | <:ns<SASL>> ->
	      lwt () = Lwt_log.debug_f ~section
		"restarted: %s -> %s"
		state.myname state.server
	      in
	      lwt () = close_socket state in
		Lwt.return (`Continue {state with
					 state = Reopen_socket;
					 socket = None}) (* ?FSMTIMEOUT *)
	    | _ ->
		send_element state Jlib.serr_bad_format;
		send_trailer state;
		lwt () = Lwt_log.notice_f ~section
		  "closing s2s connection: %s -> %s (bad format)"
		  (state.myname :> string) (state.server :> string)
		in
		  Lwt.return (`Stop state))
      | `XmlStreamElement _ ->
	  send_element state Jlib.serr_bad_format;
	  send_trailer state;
	  lwt () = Lwt_log.notice_f ~section
	    "closing s2s connection: %s -> %s (bad format)"
	    (state.myname :> string) (state.server :> string)
	  in
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
      | `Send_element el -> enqueue el state
      | `Init
      | `XmlStreamStart _ -> assert false

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
						   stream_id = new_id(),
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
		  "closing s2s connection: %s -> %s (bad format)"
		  (state.myname :> string) (state.server :> string)
		in
		  Lwt.return (`Stop state))
      | `XmlStreamElement _ ->
	  lwt () = Lwt_log.notice_f ~section
	    "closing s2s connection: %s -> %s (bad format)"
	    (state.myname :> string) (state.server :> string)
	  in
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
      | `Send_element el -> enqueue el state
      | `Init
      | `XmlStreamStart _ -> assert false

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
      | `Send_element el -> enqueue el state
      | `Init
      | `XmlStreamStart _ -> assert false

  (* This state is use to avoid reconnecting to often to bad sockets *)
  let wait_before_retry msg state =
    match msg with
      | `Send_element el ->
	  bounce_element el Jlib.err_remote_server_not_found;
	  Lwt.return (`Continue state)
(*      | `Timeout (timer, ()) when timer == state.timer ->
	  lwt () =
	    Lwt_log.notice_f ~section
	      "Reconnect delay expired: Will now retry to connect to %s when needed"
	      (state.server :> string)
	  in
	    Lwt.return (`Stop state)
*)
      | _ ->
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
			| Some (vpid, _vkey, _sid) ->
			    if vtype = "valid"
			    then vpid $! (`Valid (state.server, state.myname))
			    else vpid $! (`Invalid (state.server, state.myname))
			| _ ->
			    ()
		     );
		     Lwt.return (`Continue state)
	       | _ ->
		   Lwt.return (`Continue state))
      | `XmlStreamEnd _ ->
	  lwt () = Lwt_log.notice_f ~section
	    "connection closed in stream established: %s -> %s (xmlstreamend)"
	    (state.myname :> string) (state.server :> string) in
	    Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	  send_element state Jlib.serr_xml_not_well_formed;
	  send_trailer state;
	  lwt () = Lwt_log.notice_f ~section
	    "stream established: %s -> %s (xmlstreamerror)"
	    (state.myname :> string) (state.server :> string)
	  in
	    Lwt.return (`Stop state)
      | `Timeout ->
	  lwt () = Lwt_log.notice_f ~section
	    "stream established: %s -> %s (timeout)"
	    (state.myname :> string) (state.server :> string)
	  in
	    Lwt.return (`Stop state)
      | `Closed ->
	  lwt () = Lwt_log.notice_f ~section
	    "stream established: %s -> %s (closed)"
	    (state.myname :> string) (state.server :> string)
	  in
	    Lwt.return (`Stop state)
      | `Send_element el ->
	  cancel_timer state.timer;
	  let timer =
	    start_timer
	      s2stimeout
	      (state.pid :> unit timer_msg pid)
	      ()
	  in
	    send_element state el;
	    Lwt.return (`Continue {state with timer})
      | `Init
      | `XmlStreamStart _ -> assert false

  let handle_timer (`TimerTimeout (timer, ())) state =
    if state.timer == timer then (
      match state.state with
	| wait_before_retry ->
	    lwt () =
	      Lwt_log.notice_f ~section
		"reconnect delay expired: will now retry to connect to %s when needed"
		(state.server :> string)
	    in
	      Lwt.return (`Stop state)
	| _ ->
	    lwt () =
	      Lwt_log.notice_f ~section
		"closing connection with %s: timeout"
		(state.server :> string)
	    in
	      Lwt.return (`Stop state)
    ) else (
      Lwt.return (`Continue state)
    )

  let handle' (msg : [ Jamler_s2s_lib.s2s_out_msg | `Timeout | XMLReceiver.msg ]) state =
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

  let handle (msg : msg) state =
    match msg with
      | `Tcp_data (socket, data) -> (
	  match state.socket with
	    | Some socket' when socket == socket' ->
		lwt () =
		  Lwt_log.debug_f ~section
		    "tcp data %d %S" (String.length data) data
		in
		  XMLReceiver.parse state.xml_receiver data;
		  Socket.activate socket state.pid;
		  Lwt.return (`Continue state)
	    | _ -> assert false
	)
      | `Tcp_close socket -> (
	  match state.socket with
	    | Some socket' when socket == socket' ->
		lwt () = Lwt_log.debug ~section "tcp close" in
		  Lwt.return (`Stop state)
	    | _ -> assert false
	)
      | #Jamler_s2s_lib.s2s_out_msg
      | `Timeout
      | #XMLReceiver.msg as m ->
	  handle' m state
      | `TimerTimeout _ as m ->
	  handle_timer m state
      | #GenServer.msg -> assert false

  let terminate state _reason =
    lwt () = Lwt_log.debug ~section "terminated" in
      (match state.new' with
	 | None -> ()
	 | Some key ->
	     S2SLib.remove_connection
	       (state.myname, state.server)
	       (state.pid :> Jamler_s2s_lib.s2s_out_msg pid)
	       key
      );
      (* bounce queue manage by process and Erlang message queue *)
      (* TODO *)
    (*bounce_queue(StateData#state.queue, ?ERR_REMOTE_SERVER_NOT_FOUND),
      bounce_messages(?ERR_REMOTE_SERVER_NOT_FOUND),*)
      match state.socket with
	| None ->
	    Lwt.return ()
	| Some socket ->
	    lwt () = Socket.close socket in
	      Lwt.return ()


end

module S2SOutServer = GenServer.Make(S2SOut)
