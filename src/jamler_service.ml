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
module ACL = Jamler_acl

module ExtService :
sig
  type msg =
      [ Tcp.msg | XMLReceiver.msg | GenServer.msg
      | SM.msg | `Zxc of string * int ]
  type init_data = Lwt_unix.file_descr
  type state
  val init : init_data -> msg pid -> state Lwt.t
  val handle : msg -> state -> state GenServer.result
  val terminate : state -> unit Lwt.t

end =
struct
  type msg =
      [ Tcp.msg | XMLReceiver.msg | GenServer.msg
      | SM.msg | `Zxc of string * int ]

  type service_state =
    | Wait_for_stream
    | Wait_for_handshake
    | Stream_established

  type state =
      {pid: msg pid;
       socket: Tcp.socket;
       xml_receiver : XMLReceiver.t;
       state : service_state;
       streamid : string;
       password : string;
       check_from : bool;
       hosts : Jlib.namepreped list;
       access: bool Jamler_acl.access_rule;
      }

  type init_data = Lwt_unix.file_descr

  let section = Jamler_log.new_section "service"

  let new_id () =
    Jlib.get_random_string ()

  let init socket self =
    lwt () = Lwt_log.notice_f ~section "external service connected from %s"
      (Jamler_listener.sockaddr_to_string (Lwt_unix.getpeername socket)) in
    let socket = Tcp.of_fd socket self in
    let xml_receiver = XMLReceiver.create self in
      (* TODO *)
    let access = Jamler_acl.all in
      (*
    Access = case lists:keysearch(access, 1, Opts) of
		 {value, {_, A}} -> A;
		 _ -> all
	     end,
    {Hosts, Password} =
	case lists:keysearch(hosts, 1, Opts) of
	    {value, {_, Hs, HOpts}} ->
		case lists:keysearch(password, 1, HOpts) of
		    {value, {_, P}} ->
			{Hs, P};
		    _ ->
			% TODO: generate error
			false
		end;
	    _ ->
		case lists:keysearch(host, 1, Opts) of
		    {value, {_, H, HOpts}} ->
			case lists:keysearch(password, 1, HOpts) of
			    {value, {_, P}} ->
				{[H], P};
			    _ ->
				% TODO: generate error
				false
			end;
		    _ ->
			% TODO: generate error
			false
		end
	end,
    Shaper = case lists:keysearch(shaper_rule, 1, Opts) of
		 {value, {_, S}} -> S;
		 _ -> none
	     end,
    CheckFrom = case lists:keysearch(service_check_from, 1, Opts) of
		 {value, {_, CF}} -> CF;
		 _ -> true
	     end,
    SockMod:change_shaper(Socket, Shaper),
      *)
    let state = {pid = self;
                 socket;
                 xml_receiver;
                 state = Wait_for_stream;
                 streamid = new_id ();
		 access;
		 check_from = true;
		 password = "password";
		 hosts = [Jlib.nameprep_exn "service.localhost"];}
    in
      Tcp.activate socket self;
      Lwt.return state

  let myname = "localhost"              (* TODO *)
  let invalid_ns_err = Jlib.serr_invalid_namespace
  let invalid_xml_err = Jlib.serr_xml_not_well_formed
  let invalid_header_err =
    "<stream:stream " ^
      "xmlns:stream='http://etherx.jabber.org/streams'>" ^
      "<stream:error>Invalid Stream Header</stream:error>" ^
      "</stream:stream>"
  let invalid_handshake_err =
    "<stream:error>" ^
      "<not-authorized xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>" ^
      "<text xmlns='urn:ietf:params:xml:ns:xmpp-streams' xml:lang='en'>" ^
      "Invalid Handshake</text>" ^
      "</stream:error>" ^
      "</stream:stream>"

  let send_text state text =
    Tcp.send_async state.socket text

  let send_element state el =
    send_text state (Xml.element_to_string el)

  let stream_header_fmt =
    "<?xml version='1.0'?>" ^^
      "<stream:stream " ^^
      "xmlns:stream='http://etherx.jabber.org/streams' " ^^
      "xmlns='jabber:component:accept' " ^^
      "id='%s' from='%s'>"

  let stream_header id from =
    Printf.sprintf stream_header_fmt id from

  let send_trailer state =
    let stream_trailer = "</stream:stream>" in
    send_text state stream_trailer

  let wait_for_stream msg state =
    match msg with
      | `XmlStreamStart (_name, attrs) -> (
	match Xml.get_attr_s "xmlns" attrs with
	  | "jabber:component:accept" ->
	    let to' = Xml.get_attr_s "to" attrs in
	    let header = stream_header state.streamid (Xml.crypt to') in
	    send_text state header;
	    Lwt.return (`Continue {state with state = Wait_for_handshake})
	  | _ ->
	    send_text state invalid_header_err;
	    Lwt.return (`Stop state))
      | `XmlStreamError _ ->
	let header = stream_header "none" myname in
	send_text state header;
	send_element state invalid_xml_err;
	send_trailer state;
	Lwt.return (`Stop state)
      | `XmlStreamEnd _ ->
	Lwt.return (`Stop state)
      | `XmlStreamElement _ ->
	Lwt.return (`Stop state)

  let wait_for_handshake msg state =
    match msg with
      | `XmlStreamElement (`XmlElement (name, _attrs, els)) -> (
	match name, (Xml.get_cdata els) with
	  | "handshake", digest -> (
	      if Jlib.sha1 (state.streamid ^ state.password) = digest then (
		send_text state "<handshake/>";
		lwt () =
		  Lwt_list.iter_s
		    (fun (h : Jlib.namepreped) ->
		       Router.register_route h (state.pid :> Router.msg pid);
		       Lwt_log.notice_f ~section
			 "route registered for service %s\n" (h :> string)
		    )
		  state.hosts
		in
		  Lwt.return (`Continue {state with state = Stream_established})
	      ) else (
		send_text state invalid_handshake_err;
		Lwt.return (`Stop state)
	      )
	    )
	  | _ ->
	    Lwt.return (`Continue state))
      | `XmlStreamEnd _ ->
	Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	send_element state invalid_xml_err;
	send_trailer state;
	Lwt.return (`Stop state)
      | `XmlStreamStart _ -> assert false

  let stream_established msg state =
    match msg with
      | `XmlStreamElement el ->
	let newel = Jlib.remove_attr "xmlns" el in
	let `XmlElement (name, attrs, els) = newel in
	let from = Xml.get_attr_s "from" attrs in
	let from_jid = match state.check_from with
	  | false ->
	    (* If the admin does not want to check the from field
               when accept packets from any address.
               In this case, the component can send packet of
               behalf of the server users. *)
	    Jlib.string_to_jid from
	  | true ->
	    (* The default is the standard behaviour in XEP-0114 *)
	    match Jlib.string_to_jid from with
	      | Some jid -> (
		match List.mem jid.Jlib.lserver state.hosts with
		  | true -> Some jid
		  | false -> None)
	      | None ->
		None
	in
	let to' = Xml.get_attr_s "to" attrs in
	let to_jid = Jlib.string_to_jid to' in
	let _ = match to_jid, from_jid with
	  | Some to', Some from when (name = "iq"
				      or name = "message"
				      or name = "presence") ->
	    Router.route from to' newel
	  | _ ->
	    let err = Jlib.make_error_reply el Jlib.err_bad_request in
	    send_element state err
	in Lwt.return (`Continue state)
      | `XmlStreamEnd _ ->
	(* TODO *)
	Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	send_element state invalid_xml_err;
	send_trailer state;
	Lwt.return (`Stop state)
      | `XmlStreamStart _ -> assert false
	  

  let handle_route (`Route (from, to', packet)) state =
    let _ = match ACL.match_global_rule state.access from false with
      | true ->
	let `XmlElement (name, attrs, els) = packet in
	let attrs' = Jlib.replace_from_to_attrs
	  (Jlib.jid_to_string from) (Jlib.jid_to_string to') attrs in
	send_element state (`XmlElement (name, attrs', els))
      | false ->
	let err = Jlib.make_error_reply packet Jlib.err_not_allowed in
	Router.route to' from err
    in Lwt.return (`Continue state)
    
  let handle_xml msg state =
    match state.state with
      | Wait_for_stream -> wait_for_stream msg state
      | Wait_for_handshake -> wait_for_handshake msg state
      | Stream_established -> stream_established msg state

  let handle msg state =
    match msg with
      | `Tcp_data (socket, data) when socket == state.socket ->
          lwt () =
	    Lwt_log.debug_f ~section
	      "tcp data %d %S\n" (String.length data) data
	  in
            XMLReceiver.parse state.xml_receiver data;
            Tcp.activate state.socket state.pid;
            (*state.pid $! `Zxc (data, 1);*)
            Lwt.return (`Continue state)
      | `Tcp_data (_socket, _data) -> assert false
      | `Tcp_close socket when socket == state.socket ->
          lwt () = Lwt_log.debug ~section "tcp close\n" in
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
	 | Stream_established ->
	     lwt () =
	       Lwt_list.iter_s
		 (fun (h : Jlib.namepreped) ->
		    Router.unregister_route h (state.pid :> Router.msg pid);
		    Lwt_log.notice_f ~section
		      "route unregistered for service %s\n" (h :> string)
		 )
		 state.hosts
	     in
	       Lwt.return ()
	 | _ ->
	     Lwt.return ()
      );
      Lwt.return ()

end

module Service = GenServer.Make(ExtService)

module ServiceListen : Jamler_listener.ListenModule =
struct
  let name = "service"
  let listen_parser =
    Jamler_config.P
      (function _json ->
	 (fun socket -> ignore (Service.start socket))
      )
end

let () =
  Jamler_listener.register_mod
    (module ServiceListen : Jamler_listener.ListenModule)
