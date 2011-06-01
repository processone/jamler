type -'a pid

type 'a proc = {queue : 'a Queue.t;
		mutable wakener : 'a Lwt.u option}

external pid_to_proc : 'a pid -> 'a proc = "%identity"
external proc_to_pid : 'a proc -> 'a pid = "%identity"

let spawn f =
  let proc = {queue = Queue.create ();
	      wakener = None}
  in
  let pid = proc_to_pid proc in
  let _ =
    try_lwt
      f pid
    with
      | exn ->
	  lwt () =
            Lwt_io.eprintf "Process raised an exception: %s\n"
	      (Printexc.to_string exn)
	  in
            Lwt.fail exn
  in
    pid

exception Queue_limit

let send pid msg =
  let proc = pid_to_proc pid in
    (match proc.wakener with
       | None ->
	   if Queue.length proc.queue > 10000
	   then raise Queue_limit;
	   Queue.add msg proc.queue
       | Some wakener ->
	   Lwt.wakeup wakener msg
    )

let ($!) = send

let receive pid =
  let proc = pid_to_proc pid in
    if Queue.is_empty proc.queue then (
      let (waiter, wakener) = Lwt.wait () in
	proc.wakener <- Some wakener;
	lwt msg = waiter in
          proc.wakener <- None;
          Lwt.return msg
    ) else (
      Lwt.return (Queue.take proc.queue)
    )

let (exit_waiter, exit_wakener) = Lwt.wait ()

module Tcp =
struct
  type socket = {fd : Lwt_unix.file_descr;
		 pid : msg pid;
		 mutable writer : unit Lwt.u option;
		 mutable buffer : Buffer.t;
		 mutable buffer_limit : int;
		 mutable waiters : unit Lwt.u list;
		 mutable timeout : float;
		}

  and msg =
      [ `Tcp_data of socket * string
      | `Tcp_close of socket ]

  let rec writer socket =
    let rec write socket str pos len =
      lwt n = Lwt_unix.write socket.fd str pos len in
        if len = n
	then Lwt.return ()
	else write socket str (pos + n) (len - n)
    in
    let len = Buffer.length socket.buffer in
      if len > 0 then (
	let data = Buffer.contents socket.buffer in
	  Buffer.reset socket.buffer;
	  lwt () =
	    try_lwt
	      write socket data 0 len
            with
	      | exn ->
	          lwt () = Lwt_unix.close socket.fd in
		  lwt () =
                    Lwt_io.eprintf "Writer raised exception: %s\n"
		      (Printexc.to_string exn)
                  in
		  let senders = socket.waiters in
                    socket.waiters <- [];
	            List.iter (fun w -> Lwt.wakeup_exn w exn) senders;
		    socket.pid $! `Tcp_close socket;
                    Lwt.fail exn
          in
	    writer socket
      ) else (
	let senders = socket.waiters in
	  socket.waiters <- [];
	  List.iter (fun w -> Lwt.wakeup w ()) senders;
	  if Buffer.length socket.buffer = 0 then (
	    let waiter, wakener = Lwt.wait () in
	      socket.writer <- Some wakener;
	      lwt () = waiter in
	        socket.writer <- None;
	        writer socket
          ) else writer socket
      )

  let of_fd fd pid =
    let socket =
      {fd;
       pid = (pid :> msg pid);
       writer = None;
       buffer = Buffer.create 100;
       buffer_limit = -1;
       waiters = [];
       timeout = -1.0;
      }
    in
      ignore (writer socket);
      socket

  let set_timeout socket t =
    socket.timeout <- t

  let set_buffer_limit socket limit =
    socket.buffer_limit <- limit

  let close' socket =
    ignore (Lwt_unix.close socket.fd);
    Buffer.reset socket.buffer;
    socket.pid $! `Tcp_close socket

  let close socket =
    lwt () = Lwt_unix.close socket.fd in
      Buffer.reset socket.buffer;
      Lwt.return ()

  let buf_size = 4096
  let buf = String.make buf_size '\000'

  let activate socket pid =
    ignore (
      lwt len = Lwt_unix.read socket.fd buf 0 buf_size in
        if len > 0 then (
	  let data = String.sub buf 0 len in
	    pid $! `Tcp_data (socket, data)
	) else (
	  close' socket
	);
        Lwt.return ()
    )

  exception Closed

  let send' socket data =
    if Lwt_unix.state socket.fd <> Lwt_unix.Opened
    then raise Closed
    else (
      match socket.writer with
	| None ->
	    Buffer.add_string socket.buffer data
	| Some writer ->
	    Buffer.add_string socket.buffer data;
	    Lwt.wakeup writer ()
    )

  let send socket data =
    let waiter, wakener = Lwt.wait () in
      socket.waiters <- wakener :: socket.waiters;
      if socket.timeout <= 0.0 then (
	send' socket data;
	waiter
      ) else (
	Lwt_unix.with_timeout socket.timeout
	  (fun () ->
	     send' socket data;
	     waiter
	  )
      )

  let send_async socket data =
    if socket.buffer_limit >= 0 &&
      socket.buffer_limit < Buffer.length socket.buffer
    then (
      close' socket
    );
    ignore (
      try_lwt
	send socket data
      with
	| Lwt_unix.Timeout as exn ->
	    close' socket;
	    Lwt.fail exn
    )

end

module XMLReceiver =
struct
  type msg =
      [ `XmlStreamStart of Xml.name * Xml.attribute list
      | `XmlStreamElement of Xml.element
      | `XmlStreamEnd of Xml.name
      | `XmlStreamError of string
      ]

  type t = {pid : msg pid;
	    xml_parser : Xml.t}

  let create pid =
    let pid = (pid :> msg pid) in
    let element_callback el =
      pid $! `XmlStreamElement el
    in
    let start_callback name attrs =
      pid $! `XmlStreamStart (name, attrs)
    in
    let end_callback name =
      pid $! `XmlStreamEnd name
    in
    let xml_parser =
      Xml.create_parser
	~depth:1
	~element_callback
	~start_callback
	~end_callback
	()
    in
      {pid;
       xml_parser}

  let parse st data =
    try
      Xml.parse st.xml_parser data false
    with
      | Expat.Parse_error error ->
	  st.pid $! `XmlStreamError error

  let free st =
    Expat.parser_free st.xml_parser
end


module GenServer =
struct
  type msg = [ `System ]
  type 'a result =
      [ `Continue of 'a
      | `Stop of 'a
      ] Lwt.t

  module type Type =
  sig
    type msg
    type state
    type init_data
    val init : init_data -> msg pid -> state
    val handle : msg -> state -> state result
    val terminate : state -> unit Lwt.t
  end

  module type S =
  sig
    type msg
    type init_data
    val start : init_data -> msg pid
  end

  module Make (T : Type with type msg = private [> msg]) :
  sig
    type msg = [ `System ]
    type init_data = T.init_data
    val start : init_data -> T.msg pid
  end =
  struct
    type msg = [ `System ]
    type init_data = T.init_data
    let start init_data =
      let rec loop self state =
	lwt msg = receive self in
          match msg with
	    | #msg ->
		loop self state
	    | m ->
		lwt result =
	          try_lwt
		    T.handle m state
		  with
		    | exn ->
			lwt () =
                          Lwt_io.eprintf "GenServer raised an exception: %s\n"
			    (Printexc.to_string exn)
                        in
                          Lwt.return (`Stop state)
                in
		  match result with
		    | `Continue state ->
			loop self state
		    | `Stop state ->
			T.terminate state
      in
        spawn (fun self ->
		 let state = T.init init_data self in
		   loop self state)
  end
end

module Jlib =
struct
  type jid = {user : string;
	      server : string;
	      resource : string;
	      luser : string;
	      lserver : string;
	      lresource : string;
	     }

  let nameprep = Stringprep.nameprep
  let nodeprep = Stringprep.nodeprep
  let resourceprep = Stringprep.resourceprep

  exception Bad_jid

  let make_jid user server resource =
    let luser = nodeprep user
    and lserver = nameprep server
    and lresource = resourceprep resource in
      {user; server; resource; luser; lserver; lresource}

  let string_to_jid str =
    let rec parse1 str i =
      if i < String.length str then (
	match str.[i] with
	  | '@' ->
	      if i = 0
	      then raise Bad_jid
	      else parse2 str (String.sub str 0 i) (i + 1) (i + 1)
	  | '/' ->
	      if i = 0
	      then raise Bad_jid
	      else parse3 str "" (String.sub str 0 i) (i + 1)
	  | _ -> parse1 str (i + 1)
      ) else (
	if i = 0
	then raise Bad_jid
	else make_jid "" str ""
      )
    and parse2 str u p i =
      if i < String.length str then (
	match str.[i] with
	  | '@' ->
	      raise Bad_jid
	  | '/' ->
	      if i = p
	      then raise Bad_jid
	      else parse3 str u (String.sub str p (i - p)) (i + 1)
	  | _ -> parse2 str u p (i + 1)
      ) else (
	if i = p
	then raise Bad_jid
	else make_jid u (String.sub str p (i - p)) ""
      )
    and parse3 str u s i =
      make_jid u s (String.sub str i (String.length str - i))
    in
      parse1 str 0


  let stream_error condition cdata =
    Xml.Element
      ("stream:error",
       [],
       [Xml.Element (condition, [("xmlns", <:ns<STREAMS>>)],
		     [Xml.Cdata cdata])])

  let serr_bad_format = stream_error "bad-format" ""
  let serr_bad_namespace_prefix = stream_error "bad-namespace-prefix" ""
  let serr_conflict = stream_error "conflict" ""
  let serr_connection_timeout = stream_error "connection-timeout" ""
  let serr_host_gone = stream_error "host-gone" ""
  let serr_host_unknown = stream_error "host-unknown" ""
  let serr_improper_addressing = stream_error "improper-addressing" ""
  let serr_internal_server_error = stream_error "internal-server-error" ""
  let serr_invalid_from = stream_error "invalid-from" ""
  let serr_invalid_id = stream_error "invalid-id" ""
  let serr_invalid_namespace = stream_error "invalid-namespace" ""
  let serr_invalid_xml = stream_error "invalid-xml" ""
  let serr_not_authorized = stream_error "not-authorized" ""
  let serr_policy_violation = stream_error "policy-violation" ""
  let serr_remote_connection_failed = stream_error "remote-connection-failed" ""
  let serr_resourse_constraint = stream_error "resource-constraint" ""
  let serr_restricted_xml = stream_error "restricted-xml" ""
  let serr_see_other_host host = stream_error "see-other-host" host
  let serr_system_shutdown = stream_error "system-shutdown" ""
  let serr_unsupported_encoding = stream_error "unsupported-encoding" ""
  let serr_unsupported_stanza_type = stream_error "unsupported-stanza-type" ""
  let serr_unsupported_version = stream_error "unsupported-version" ""
  let serr_xml_not_well_formed = stream_error "xml-not-well-formed" ""
end

module C2S :
sig
  type msg = [ Tcp.msg | XMLReceiver.msg | GenServer.msg | `Zxc of string * int ]
  type init_data = Lwt_unix.file_descr
  type state
  val init : init_data -> msg pid -> state
  val handle : msg -> state -> state GenServer.result
  val terminate : state -> unit Lwt.t
end =
struct
  type msg = [ Tcp.msg | XMLReceiver.msg | GenServer.msg | `Zxc of string * int ]

  type c2s_state =
    | Wait_for_stream
    | Wait_for_auth
    | Wait_for_feature_request
    | Wait_for_bind
    | Wait_for_session
    | Wait_for_sasl_response
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
       user : string;
       server : string;
       resource : string;
       jid : Jlib.jid;
       (*sid : string;
       pres_t = ?SETS:new(),
       pres_f = ?SETS:new(),
       pres_a = ?SETS:new(),
       pres_i = ?SETS:new(),
       pres_last, pres_pri,
       pres_timestamp,
       pres_invis = false,
       privacy_list = #userlist{},
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
		 user = "";
		 server = "";
		 resource = "";
		 jid = Jlib.make_jid "" "" "";
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

  let send_text state text =
    Printf.printf "Send XML on stream = %S\n" text; flush stdout;
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


  let wait_for_stream msg state =
    match msg with
      | `XmlStreamStart (_name, attrs) -> (
	  let default_lang = "en" in	(* TODO *)
	    match Xml.get_attr_s "xmlns:stream" attrs with
	      | <:ns<STREAM>> -> (
		  let server = Jlib.nameprep (Xml.get_attr_s "to" attrs) in
		    if is_my_host server then (
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
			      send_header state server "1.0" default_lang;
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
				let mechs = [] in
				    (*Mechs = lists:map(
					      fun(S) ->
						      {xmlelement, "mechanism", [],
						       [{xmlcdata, S}]}
					      end, cyrsasl:listmech(Server)),*)
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
				      (Xml.Element
					 ("stream:features", [],
					  (*TLSFeature ++
					    CompressFeature ++*)
					  [Xml.Element
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
				match state.resource with
				  | "" ->
				      (* RosterVersioningFeature =
					 ejabberd_hooks:run_fold(
					 roster_get_versioning_feature,
					 Server, [], [Server]),*)
				      let stream_features =
					[Xml.Element
					   ("bind",
					    [("xmlns", <:ns<BIND>>)], []);
					 Xml.Element
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
					  (Xml.Element
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
					(Xml.Element
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
			      send_header state server "" default_lang;
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
		    ) else (
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

  let handle_xml msg state =
    match state.state, msg with
      | Wait_for_stream, _ -> wait_for_stream msg state
      | _, `XmlStreamStart (name, attrs) ->
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

  let handle msg state =
    match msg with
      | `Tcp_data (socket, data) when socket == state.socket ->
          lwt () = Lwt_io.printf "tcp data %d %S\n" (String.length data) data in
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
    lwt () = Tcp.close state.socket in
      Lwt.return ()
end

module C2SServer = GenServer.Make(C2S)


let rec accept listen_socket =
  lwt (socket, _) = Lwt_unix.accept listen_socket in
    (*ignore (spawn (C2S.start socket));*)
    ignore (C2SServer.start socket);
    accept listen_socket

let listener_start () =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_INET (Unix.inet_addr_any, 5222) in
    Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
    Lwt_unix.bind socket addr;
    Lwt_unix.listen socket 1024;
    accept socket

let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let main () =
  let _ = listener_start () in
    exit_waiter

let () = Lwt_main.run (main ())
