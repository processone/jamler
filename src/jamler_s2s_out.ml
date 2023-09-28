open Process

let src = Jamler_log.new_src "s2s_out"

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
      | `Verify of (pid * string * string)
      ]
  include GenServer.Type with
    type init_data = Jlib.namepreped * Jlib.namepreped * start_type
    and type stop_reason = GenServer.reason
  val start_connection : pid -> unit
  val stop_connection : pid -> unit
end =
struct
  type msg += Unit

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
    (*| CertFile of string*)
    | Connect

  type start_type =
      [ `New of string
      | `Verify of (pid * string * string)
      ]

  type init_data = Jlib.namepreped * Jlib.namepreped * start_type

  type stop_reason = GenServer.reason

  type state =
      {pid: pid;
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
       verify : (pid * string * string) option;
       timer : timer;
	 (* bridge *)}

  let new_id () =
    Jlib.get_random_string ()

  let s2stimeout = 600.0

  let send_text state text =
    match state.socket with
    | Some socket ->
       Socket.send socket text
    | None -> assert false

  let send_string state text =
    send_text state (Bytes.of_string text)

  let send_element state el =
    send_string state (Xml.element_to_string el)

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
    pid $! Jamler_s2s_lib.S2SOut `Init

  let stop_connection pid =
    pid $! Jamler_s2s_lib.S2SOut `Closed

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
    send_string state stream_trailer

  let _socket_default_result = None

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
    `Continue state (* get_timeout_interval(StateName) *)

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
      start_timer s2stimeout self Unit
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
    `Continue state

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
    let _timeout = outgoing_s2s_timeout () in
    let socket =
      Eio.Net.connect
        ~sw:(Process.get_global_switch ())
        (Process.get_global_env ())#net
        (`Tcp (Eio_unix__Net.Ipaddr.of_unix addr', port))
    in
    let tcpsock = Socket.of_fd socket self in
    ignore (Socket.activate tcpsock self);
    tcpsock

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
      ()
    ) else (
      (* Log new outgoing connections: *)
      Logs.info ~src
	(fun m ->
          m "trying to open s2s connection: %s -> %s with tls=%B"
	    myname server tls);
    )

  let get_addr_port ascii_addr =
    (* TODO: srv *)
    let h =
      Eio_unix.run_in_systhread (fun () -> Unix.gethostbyname ascii_addr)
    in
    let res =
      List.map
	(fun addr -> (addr, 5269))
	(Array.to_list h.Unix.h_addr_list)
    in
    res

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
      start_timer delay state.pid Unit
    in
    `Continue {state with
	state = Wait_before_retry;
	timer;
	delay_to_retry = delay;
	queue = Queue.create ();}

  let send_db_request state =
    let server = state.server in
    let new' =
      match state.new' with
      | None -> (
	match (S2SLib.try_register (state.myname, server) state.pid) with
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
    `Continue {state with state = Wait_for_validation} (* ?FSMTIMEOUT*6 *)


  let close_socket state =
    match state.socket with
    | Some tcpsock ->
       Socket.close tcpsock
    | None ->
       ()

  let open_socket msg state =
    match msg with
    | `Init ->
       log_s2s_out state.new'
	 (state.myname :> string) (state.server :> string) state.tls;
       Logs.debug ~src
	 (fun m ->
           m "open_socket: %s -> %s"
             (* TODO "new = %s, verify = %s" *)
	     (state.myname :> string) (state.server :> string));
       let addr_list =
	 try
	   let ascii_addr = Idna.domain_utf8_to_ascii (state.server :> string) in
	   get_addr_port ascii_addr
	 with
	 | _ -> []
       in
       let open_res =
	 List.fold_left
	   (fun acc (addr, port) ->
	     match acc with
	     | Some _socket ->
		acc
	     | None -> (
	       try
		 let s = open_socket1 addr port state.pid in
		 Some s
	       with
	       | _ -> None
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
	   send_string new_state
	     (stream_header (state.myname :> string)
		(state.server :> string) version);
	   `Continue new_state (* ?FSMTIMEOUT *)
	| None ->
           Logs.info ~src
	     (fun m ->
               m "s2s connection: %s -> %s (remote server not found)"
		 (state.myname :> string) (state.server :> string));
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
       Logs.info ~src
	 (fun m ->
           m "s2s connection: %s -> %s (stopped in open socket)"
	     (state.myname :> string) (state.server :> string));
       `Stop state
    | `Timeout ->
       Logs.info ~src
	 (fun m ->
           m "s2s connection: %s -> %s (timeout in open socket)"
	     (state.myname :> string) (state.server :> string));
       `Stop state
    | `Send_element el -> enqueue el state
    | `XmlStreamElement _
    | `XmlStreamEnd _
    | `XmlStreamError _
    | `XmlStreamStart _ -> assert false

  let wait_for_stream msg state =
    match msg with
      | `XmlStreamStart (_name, attrs) -> (
	  match (Xml.get_attr_s "xmlns" attrs,
		 Xml.get_attr_s "xmlns:db" attrs,
		 Xml.get_attr_s "version" attrs = "1.0") with
	    | "jabber:server", "jabber:server:dialback", false ->
		send_db_request state
	    | "jabber:server", "jabber:server:dialback", true when state.use_v10 ->
		`Continue {state with state = Wait_for_features} (* ?FSMTIMEOUT *)
	    | "jabber:server", "jabber:server:dialback", true when not state.use_v10 ->
		(* Clause added to handle Tigase's workaround for an old ejabberd bug *)
		send_db_request state
	    | "jabber:server", "", true when state.use_v10 ->
	       `Continue {state with db_enabled = false;
	                             state = Wait_for_features} (* ?FSMTIMEOUT *)
	    | ns_provided, db, _ ->
	       send_element state Jlib.serr_invalid_namespace;
               Logs.info ~src
	         (fun m ->
                   m ("closing s2s connection: %s -> %s (invalid namespace):"
		      ^^ "namespace provided = %s, "
		      ^^ "namespace expected = \"jabber:server\", "
		      ^^ "xmlns:db provided = %s")
		     (state.myname :> string)
		     (state.server :> string)
		     ns_provided
		     db);
	       `Stop state)
      | `XmlStreamError _ ->
	  send_element state Jlib.serr_xml_not_well_formed;
	  send_trailer state;
          Logs.info ~src
	    (fun m ->
              m "closing s2s connection: %s -> %s (invalid xml)"
	        (state.myname :> string) (state.server :> string));
          `Stop state
      | `XmlStreamEnd _ ->
         Logs.info ~src
	   (fun m ->
             m "closing s2s connection: %s -> %s (xmlstreamend)"
	       (state.myname :> string) (state.server :> string));
	 `Stop state
      | `Timeout ->
         Logs.info ~src
	   (fun m ->
             m "closing s2s connection: %s -> %s (timeout in wait_for_stream)"
	       (state.myname :> string) (state.server :> string));
	 `Stop state
      | `Closed ->
         Logs.info ~src
	   (fun m ->
             m "closing s2s connection: %s -> %s (close in wait_for_stream)"
	       (state.myname :> string) (state.server :> string));
	 `Stop state
      | `Send_element el -> enqueue el state
      | `Init
      | `XmlStreamElement _ -> assert false

  let wait_for_validation msg state =
    match msg with
    | `XmlStreamElement el -> (
      match is_verify_res el with
      | Result (to', from, id, type') ->
         Logs.debug ~src
	   (fun m ->
             m "recv result: from = %s, to = %s, id = %s, type = %s"
	       from to' id type');
	 (match (type', state.tls_enabled, state.tls_required) with
	  | "valid", enabled, required when (enabled = true || required = false) ->
	     send_queue state;
             Logs.info ~src
	       (fun m ->
                 m "connection established: %s -> %s with tls=%B"
		   (state.myname :> string)
		   (state.server :> string) state.tls_enabled);
	     (* ejabberd_hooks:run(s2s_connect_hook,
		[StateData#state.myname,
		StateData#state.server]), *)
	     `Continue
	       {state with
		 state = Stream_established;
		 queue = Queue.create ()}
	  | _ ->
	     (* TODO: bounce packets *)
             Logs.info ~src
	       (fun m ->
                 m "closing s2s connection: %s -> %s (invalid dialback key)"
		   (state.myname :> string)
		   (state.server :> string));
	     `Stop state)
      | Verify (to', from, id, type') ->
         Logs.debug ~src
	   (fun m ->
             m "recv verify: from = %s, to = %s, id = %s, type = %s"
	       from to' id type');
	 (match state.verify with
	  | None ->
	     (* TODO: Should'nt we close the connection here ? *)
	     let next_state = Wait_for_validation in
	     `Continue {state with state = next_state}
	  (* get_timeout_interval(NextState) *)
	  | Some (pid, _key, _sid) ->
	     (match type' with
	      | "valid" ->
		 pid $! (Jamler_s2s_lib.S2SValidation (`Valid (state.server, state.myname)))
	      | _ ->
		 pid $! (Jamler_s2s_lib.S2SValidation (`Invalid (state.server, state.myname))));
	     if state.verify = None then (
	       `Stop state
	     ) else (
	       let next_state = Wait_for_validation in
	       `Continue {state with state = next_state}
	 (* get_timeout_interval(NextState) *)
	 ))
      | False ->
	 `Continue state) (* ?FSMTIMEOUT *)
    | `XmlStreamEnd _ ->
       Logs.info ~src
	 (fun m ->
           m "wait for validation: %s -> %s (xmlstreamend)"
	     (state.myname :> string) (state.server :> string));
       `Stop state
    | `XmlStreamError _ ->
       Logs.info ~src
	 (fun m ->
           m "wait for validation: %s -> %s (xmlstreamerror)"
	     (state.myname :> string) (state.server :> string));
       send_element state Jlib.serr_xml_not_well_formed;
       send_trailer state;
       `Stop state
    | `Timeout -> (
      match state.verify with
      | Some (_vpid, _vkey, _sid) ->
	 (* This is an auxiliary s2s connection for dialback.
	    This timeout is normal and doesn't represent a problem. *)
         Logs.debug ~src
	   (fun m ->
             m "wait_for_validation: %s -> %s (timeout in verify connection)"
	       (state.myname :> string) (state.server :> string));
	 `Stop state
      | None ->
         Logs.info ~src
	   (fun m ->
             m "wait_for_validation: %s -> %s (connect timeout)"
	       (state.myname :> string) (state.server :> string));
	 `Stop state)
    | `Closed ->
       Logs.info ~src
	 (fun m ->
           m "wait for validation: %s -> %s (closed)"
	     (state.myname :> string) (state.server :> string));
       `Stop state
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
		      | [%xmlns "SASL"] ->
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
		      | [%xmlns "TLS"] ->
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
          Logs.info ~src
	    (fun m ->
              m "connection established: %s -> %s"
		(state.myname :> string) (state.server :> string));
	  (* ejabberd_hooks:run(s2s_connect_hook,
	     [StateData#state.myname,
	     StateData#state.server]), *)
	  `Continue {state with
	      state = Stream_established;
	      queue = Queue.create()}
	) else if sasl_ext && state.try_auth && state.new' <> None then (
	  send_element state
	    (`XmlElement ("auth",
			  [("xmlns", [%xmlns "SASL"]);
			   ("mechanism", "EXTERNAL")],
			  [`XmlCdata (Jlib.encode_base64
					(state.myname :> string))]));
	  `Continue {state with
	      state = Wait_for_auth_result;
	      try_auth = false} (* ?FSMTIMEOUT *)
	) else if start_tls && state.tls && (not state.tls_enabled) then (
	  send_element state
	    (`XmlElement ("starttls", [("xmlns", [%xmlns "SASL"])], []));
	  `Continue {state with
	      state = Wait_for_starttls_proceed} (* ?FSMTIMEOUT *)
	) else if start_tls_required && (not state.tls) then (
          Logs.debug ~src
	    (fun m ->
              m "restarted: %s -> %s"
		(state.myname :> string) (state.server :> string));
	  close_socket state;
	  `Continue {state with
	      state = Reopen_socket;
	      use_v10 = false;
	      socket = None} (* ?FSMTIMEOUT *)
	) else if state.db_enabled then (
	  send_db_request state
	) else (
          Logs.debug ~src
	    (fun m ->
              m "restarted: %s -> %s"
		(state.myname :> string) (state.server :> string));
	  (* TODO: clear message queue *)
	  close_socket state;
	  `Continue {state with
	      socket = None;
	      use_v10 = false;
	      state = Reopen_socket})) (* ?FSMTIMEOUT *)
      | _ ->
	 send_element state Jlib.serr_bad_format;
	 send_trailer state;
         Logs.info ~src
	   (fun m ->
             m "closing s2s connection: %s -> %s (bad format)"
	       (state.myname :> string) (state.server :> string));
	 `Stop state
    )
    | `XmlStreamEnd _ ->
       Logs.info ~src
	 (fun m ->
           m "wait_for_features: xmlstreamend");
       `Stop state
    | `XmlStreamError _ ->
       send_element state Jlib.serr_xml_not_well_formed;
       send_trailer state;
       Logs.info ~src
	 (fun m ->
           m "wait_for_features: xmlstreamerror");
       `Stop state
    | `Timeout ->
       Logs.info ~src
	 (fun m ->
           m "wait_for_features: timeout");
       `Stop state
    | `Closed ->
       Logs.info ~src
	 (fun m ->
           m "wait_for_features: closed");
       `Stop state
    | `Send_element el -> enqueue el state
    | `Init
    | `XmlStreamStart _ -> assert false

  let wait_for_auth_result msg state =
    match msg with
    | `XmlStreamElement (`XmlElement ("success", attrs, _els)) -> (
      match Xml.get_attr_s "xmlns" attrs with
      | [%xmlns "SASL"] ->
         Logs.debug ~src
	   (fun m ->
             m "auth: %s -> %s"
	       (state.myname :> string) (state.server :> string));
	 XMLReceiver.reset_stream state.xml_receiver;
	 send_string state
	   (stream_header
	      (state.myname :> string)
	      (state.server :> string)
	      " version='1.0'");
	 `Continue {state with
	     state = Wait_for_stream;
	     stream_id = new_id();
	     authenticated = true} (* ?FSMTIMEOUT *)
      | _ ->
	 send_element state Jlib.serr_bad_format;
	 send_trailer state;
         Logs.info ~src
	   (fun m ->
             m "closing s2s connection: %s -> %s (bad format)"
	       (state.myname :> string) (state.server :> string));
	 `Stop state
    )
    | `XmlStreamElement (`XmlElement ("failure", attrs, _els)) -> (
      match Xml.get_attr_s "xmlns" attrs with
      | [%xmlns "SASL"] ->
         Logs.debug ~src
	   (fun m ->
             m "restarted: %s -> %s"
	       (state.myname :> string) (state.server :> string));
	 close_socket state;
	 `Continue {state with
	     state = Reopen_socket;
	     socket = None} (* ?FSMTIMEOUT *)
      | _ ->
	 send_element state Jlib.serr_bad_format;
	 send_trailer state;
         Logs.info ~src
	   (fun m ->
             m "closing s2s connection: %s -> %s (bad format)"
	       (state.myname :> string) (state.server :> string));
	 `Stop state
    )
    | `XmlStreamElement _ ->
       send_element state Jlib.serr_bad_format;
       send_trailer state;
       Logs.info ~src
	 (fun m ->
           m "closing s2s connection: %s -> %s (bad format)"
	     (state.myname :> string) (state.server :> string));
       `Stop state
    | `XmlStreamEnd _ ->
       Logs.info ~src
	 (fun m ->
           m "wait for auth result: xmlstreamend");
       `Stop state
    | `XmlStreamError _ ->
       send_element state Jlib.serr_xml_not_well_formed;
       send_trailer state;
       Logs.info ~src
	 (fun m ->
           m "wait for auth result: xmlstreamerror");
       `Stop state
    | `Timeout ->
       Logs.info ~src
	 (fun m ->
           m "wait for auth result: timeout");
       `Stop state
    | `Closed ->
       Logs.info ~src
	 (fun m ->
           m "wait for auth result: closed");
       `Stop state
    | `Send_element el -> enqueue el state
    | `Init
    | `XmlStreamStart _ -> assert false

  let wait_for_starttls_proceed msg state =
    match msg with
    | `XmlStreamElement (`XmlElement ("proceed", attrs, _els)) -> (
      match Xml.get_attr_s "xmlns" attrs with
      | [%xmlns "TLS"] ->
         Logs.debug ~src
	   (fun m ->
             m "starttls: %s -> %s"
	       (state.myname :> string) (state.server :> string));
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
         Logs.err ~src
	   (fun m ->
             m "don't support starttls yet");
	 `Stop state
      | _ ->
	 send_element state Jlib.serr_bad_format;
	 send_trailer state;
         Logs.info ~src
	   (fun m ->
             m "closing s2s connection: %s -> %s (bad format)"
	       (state.myname :> string) (state.server :> string));
	 `Stop state
    )
    | `XmlStreamElement _ ->
       Logs.info ~src
	 (fun m ->
           m "closing s2s connection: %s -> %s (bad format)"
	     (state.myname :> string) (state.server :> string));
       `Stop state
    | `XmlStreamEnd _ ->
       Logs.info ~src
	 (fun m ->
           m "wait for starttls proceed: xmlstreamend");
       `Stop state
    | `XmlStreamError _ ->
       send_element state Jlib.serr_xml_not_well_formed;
       send_trailer state;
       Logs.info ~src
	 (fun m ->
           m "wait for starttls proceed: xmlstreamerror");
       `Stop state
    | `Timeout ->
       Logs.info ~src
	 (fun m ->
           m "wait for starttls proceed: timeout");
       `Stop state
    | `Closed ->
       Logs.info ~src
	 (fun m ->
           m "wait for starttls proceed: closed");
       `Stop state
    | `Send_element el -> enqueue el state
    | `Init
    | `XmlStreamStart _ -> assert false

  let reopen_socket msg state =
    match msg with
    | `XmlStreamElement _ ->
       `Continue state (* ?FSMTIMEOUT *)
    | `XmlStreamEnd _ ->
       `Continue state (* ?FSMTIMEOUT *)
    | `XmlStreamError _ ->
       `Continue state (* ?FSMTIMEOUT *)
    | `Timeout ->
       Logs.info ~src
	 (fun m ->
           m "reopen socket: timeout");
       `Stop state
    | `Closed ->
       start_connection state.pid;
       `Continue {state with state = Open_socket} (* ?FSMTIMEOUT *)
    | `Send_element el -> enqueue el state
    | `Init
    | `XmlStreamStart _ -> assert false

  (* This state is use to avoid reconnecting to often to bad sockets *)
  let wait_before_retry msg state =
    match msg with
    | `Send_element el ->
       bounce_element el Jlib.err_remote_server_not_found;
       `Continue state
(*      | `Timeout (timer, ()) when timer == state.timer ->
	  lwt () =
	    Lwt_log.notice_f ~section
	      "Reconnect delay expired: Will now retry to connect to %s when needed"
	      (state.server :> string)
	  in
	    Lwt.return (`Stop state)
*)
    | _ ->
       `Continue state (* ?FSMTIMEOUT *)

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
       Logs.debug ~src
	 (fun m ->
           m "s2s stream established");
       (match is_verify_res el with
	| Verify (vto, vfrom, vid, vtype) ->
           Logs.debug ~src
	     (fun m ->
               m "recv verify: to = %s, from = %s, id = %s, type = %s"
		 vto vfrom vid vtype);
	   (match state.verify with
	    | Some (vpid, _vkey, _sid) ->
	       if vtype = "valid"
	       then vpid $! (Jamler_s2s_lib.S2SValidation (`Valid (state.server, state.myname)))
	       else vpid $! (Jamler_s2s_lib.S2SValidation (`Invalid (state.server, state.myname)))
	    | _ ->
	       ()
	   );
	   `Continue state
	| _ ->
	   `Continue state
       )
    | `XmlStreamEnd _ ->
       Logs.info ~src
	 (fun m ->
           m "connection closed in stream established: %s -> %s (xmlstreamend)"
	     (state.myname :> string) (state.server :> string));
       `Stop state
    | `XmlStreamError _ ->
       send_element state Jlib.serr_xml_not_well_formed;
       send_trailer state;
       Logs.info ~src
	 (fun m ->
           m "stream established: %s -> %s (xmlstreamerror)"
	     (state.myname :> string) (state.server :> string));
       `Stop state
    | `Timeout ->
       Logs.info ~src
	 (fun m ->
           m "stream established: %s -> %s (timeout)"
	     (state.myname :> string) (state.server :> string));
       `Stop state
    | `Closed ->
       Logs.info ~src
	 (fun m ->
           m "stream established: %s -> %s (closed)"
	     (state.myname :> string) (state.server :> string));
       `Stop state
    | `Send_element el ->
       cancel_timer state.timer;
       let timer =
	 start_timer s2stimeout state.pid Unit
       in
       send_element state el;
       `Continue {state with timer}
    | `Init
    | `XmlStreamStart _ -> assert false

  let handle_timer timer state =
    if state.timer == timer then (
      match state.state with
      | Wait_before_retry ->
         Logs.info ~src
	   (fun m ->
             m "reconnect delay expired: will now retry to connect to %s when needed"
	       (state.server :> string));
	 `Stop state
      | _ ->
         Logs.info ~src
	   (fun m ->
             m "closing connection with %s: timeout"
	       (state.server :> string));
	 `Stop state
    ) else (
      `Continue state
    )

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

  let handle (msg : msg) state =
    match msg with
    | Socket.Tcp_data (socket, data) -> (
      match state.socket with
      | Some socket' when socket == socket' ->
         ignore (Logs.debug ~src
	           (fun m ->
                     m "tcp data %d %S" (String.length data) data);
		 XMLReceiver.parse state.xml_receiver data;
           );
	 ignore (Socket.activate socket state.pid);
	 `Continue state
      | _ -> assert false
    )
    | Socket.Tcp_close socket -> (
      match state.socket with
      | Some socket' when socket == socket' ->
         Logs.debug ~src (fun m -> m "tcp close");
	 `Stop state
      | _ -> assert false
    )
    | GenServer.Timeout ->
       handle' `Timeout state
    | Jamler_s2s_lib.S2SOut m ->
       handle' m state
    | XMLReceiver.Xml m ->
       handle' (m :> [ XMLReceiver.xml_msg | Jamler_s2s_lib.s2s_out_msg ]) state
    | TimerTimeout (timer, _) ->
       handle_timer timer state
    | _ ->
       (* TODO: add a warning *)
       `Continue state

  let terminate state _reason =
    Logs.debug ~src (fun m -> m "terminated");
    (match state.new' with
     | None -> ()
     | Some key ->
	S2SLib.remove_connection (state.myname, state.server) state.pid key
    );
    (* bounce queue manage by process and Erlang message queue *)
    (* TODO *)
    (*bounce_queue(StateData#state.queue, ?ERR_REMOTE_SERVER_NOT_FOUND),
      bounce_messages(?ERR_REMOTE_SERVER_NOT_FOUND),*)
    match state.socket with
    | None ->
       ()
    | Some socket ->
       Socket.close socket;
       ()

end

module S2SOutServer = GenServer.Make(S2SOut)
