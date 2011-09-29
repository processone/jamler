open Process

let section = Jamler_log.new_section "erl_epmd"

module Packet = Jamler_packet
module GenServer = Gen_server

let nodename = ref "jamler"		(* TODO *)
let nodehost = ref "localhost"		(* TODO *)
let node_creation = ref 0
let epmd_port = 4369
let cookie = ref "YDZZQPNLWAMUSODVCMLA"	(* TODO *)

(* TODO: move somewhere *)
let add_int8 buf x =
  Buffer.add_char buf (Char.chr (x land 0xff))

let add_int16_be buf x =
  Buffer.add_char buf (Char.chr ((x lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (x land 0xff))

let add_int32_be buf x =
  Buffer.add_char buf (Char.chr ((x lsr 24) land 0xff));
  Buffer.add_char buf (Char.chr ((x lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((x lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (x land 0xff))

type packet_msg = [ `Packet of string ]

module ErlEPMD :
sig
  include GenServer.Type with
    type msg =
        [ Socket.msg | packet_msg | GenServer.msg | `Init ]
    and type init_data = unit Lwt.u
    and type stop_reason = GenServer.reason
end =
struct
  type msg =
      [ Socket.msg | packet_msg | GenServer.msg | `Init ]

  type init_data = unit Lwt.u

  type stop_reason = GenServer.reason

  type conn_state =
    | Open_socket of unit Lwt.u
    | Wait_for_alive2_resp of unit Lwt.u * Socket.socket
    | Connection_established of Socket.socket

  type state =
      {pid : msg pid;
       state: conn_state;
       p : Packet.t;
      }

  let send_text socket text =
    Socket.send_async socket text

  let send_packet state packet =
    send_text state (Packet.decorate `BE2 packet)

  let init wakener self =
    let state = Open_socket wakener in
      self $! `Init;
      Lwt.return
	(`Continue
	   {pid = self;
	    state;
	    p = Packet.create `BE2;
	   })

  let open_socket addr port self =
    (*let timeout = 1.0 in*)
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let addr = Unix.ADDR_INET (addr, port) in
    lwt () = Lwt_unix.connect socket addr in
    let tcpsock = Socket.of_fd socket self in
      ignore (Socket.activate tcpsock self);
      Lwt.return tcpsock

  let close_socket socket =
    Socket.close socket

  let make_alive2_req port name =
    let b = Buffer.create 20 in
      add_int8 b 120;
      add_int16_be b port;
      add_int8 b 77;
      add_int8 b 0;
      add_int16_be b 5;
      add_int16_be b 5;
      add_int16_be b (String.length name);
      Buffer.add_string b name;
      Buffer.contents b

  let handle_msg msg state =
    match state.state, msg with
      | Open_socket wakener, `Init ->
	  lwt socket =
	    try_lwt
	      open_socket Unix.inet_addr_loopback epmd_port state.pid
	    with
	      | exn ->
		  lwt () = Lwt_log.fatal ~exn ~section
		    "failed to open epmd connection"
		  in
		    Lwt.fail exn
	  in
	    send_packet socket (make_alive2_req 666 !nodename);
	    Lwt.return
	      (`Continue
		 {state with
		    state = Wait_for_alive2_resp (wakener, socket)})
      | Open_socket _, `Packet _ -> assert false
      | Wait_for_alive2_resp (wakener, socket), `Packet data ->
	  if String.length data >= 4 &&
	    data.[0] = '\121' &&
	      data.[1] = '\000'
	  then (
	    let creation = Char.code data.[3] land 3 in
	      node_creation := creation;
	      Lwt.wakeup wakener ();
	      Lwt.return
		(`Continue
		   {state with
		      state = Connection_established socket})
	  ) else (
	    Lwt.wakeup_exn wakener (Failure "can't register nodename");
	    Lwt.return (`Stop state)
	  )
      | Wait_for_alive2_resp (_wakener, _socket), `Init -> assert false
      | Connection_established _socket, _ -> assert false

  let handle (msg : msg) state =
    match msg with
      | `Tcp_data (socket, data) -> (
	  lwt () =
	    Lwt_log.notice_f ~section
	      "tcp data %d %S" (String.length data) data
	  in
	    (*Packet.parse state.p data;*)
	    state.pid $! `Packet data;
	    ignore (Socket.activate socket state.pid);
	    Lwt.return (`Continue state)
	)
      | `Tcp_close _socket -> (
	  lwt () = Lwt_log.debug ~section "tcp close" in
	    Lwt.return (`Stop state)
	)
      | #packet_msg
      | `Init as m ->
	  handle_msg m state
      | #GenServer.msg -> assert false

  let terminate state _reason =
    let () =
      match state.state with
	| Open_socket wakener
	| Wait_for_alive2_resp (wakener, _) ->
	    Lwt.wakeup_exn wakener (Failure "can't register nodename");
	| Connection_established _ -> ()
    in
    lwt () =
      match state.state with
	| Open_socket _wakener -> Lwt.return ()
	| Wait_for_alive2_resp (_, socket)
	| Connection_established socket ->
	    Socket.close socket
    in
      Lwt.return ()


end

module ErlEPMDServer = GenServer.Make(ErlEPMD)

let start name =
  nodename := name;
  let (waiter, wakener) = Lwt.wait () in
    ignore (ErlEPMDServer.start wakener);
    waiter

let get_addr_exn ascii_addr =
  lwt h = Lwt_lib.gethostbyname ascii_addr in
    match Array.to_list h.Unix.h_addr_list with
      | [] -> raise Not_found
      | addr :: _ -> Lwt.return addr

let open_socket addr port f =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    try_lwt
      let addr = Unix.ADDR_INET (addr, port) in
      lwt () = Lwt_unix.connect socket addr in
      lwt res = f socket in
      lwt () = Lwt_unix.close socket in
	Lwt.return res
    with
      | exn ->
	  ignore (Lwt_unix.close socket);
	  Lwt.fail exn

let open_socket' addr port =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    try_lwt
      let addr = Unix.ADDR_INET (addr, port) in
      lwt () = Lwt_unix.connect socket addr in
	Lwt.return (Some socket)
    with
      | _exn ->
	  ignore (Lwt_unix.close socket);
	  Lwt.return None

let node_to_namehost node =
    let idx = String.index node '@' in
    let nodename = String.sub node 0 idx in
    let hostname = String.sub node (idx + 1) (String.length node - idx - 1) in
      (nodename, hostname)

let lookup_node node =
  try_lwt
    let (nodename, hostname) = node_to_namehost node in
    lwt addr = get_addr_exn hostname in
      open_socket addr epmd_port
	(fun socket ->
	   let packet = Packet.decorate `BE2 ("\122" ^ nodename) in
	   lwt _ = Lwt_unix.write socket packet 0 (String.length packet) in
	   lwt () = Lwt_unix.wait_read socket in
	   let buf = String.make 64 '\000' in
	   lwt c = Lwt_unix.read socket buf 0 64 in
	     if c >= 4 && buf.[1] = '\000' then (
	       Lwt.return
		 (Some ((Char.code buf.[2] lsl 8) lor Char.code buf.[3]))
	     ) else raise Not_found
	)
  with
    | _ -> Lwt.return None

(*
let _ =
  lwt Some port = lookup_node "asd@localhost" in
    Printf.printf "asd: %d\n%!" port;
    Lwt.return ()
*)

let dflag_published = 1
let dflag_atom_cache = 2
let dflag_extended_references = 4
let dflag_dist_monitor = 8
let dflag_fun_tags = 0x10
let dflag_dist_monitor_name = 0x20
let dflag_hidden_atom_cache = 0x40
let dflag_new_fun_tags = 0x80
let dflag_extended_pids_ports = 0x100
let dflag_export_ptr_tag = 0x200
let dflag_bit_binaries = 0x400
let dflag_new_floats = 0x800
let dflag_unicode_io = 0x1000
let dflag_dist_hdr_atom_cache = 0x2000
let dflag_small_atom_tags = 0x4000


type node_connection_msg = [ `TODO ]
let nodes = Hashtbl.create 100
let add_node_connection node pid =
  Hashtbl.replace nodes node pid
let remove_node_connection node =
  Hashtbl.remove nodes node
let find_node_connection node =
  try
    Some (Hashtbl.find nodes node)
  with
    | Not_found -> None

module ErlNodeOut :
sig
  include GenServer.Type with
    type msg =
        [ Socket.msg | packet_msg | `Parse | GenServer.msg ]
    and type init_data = string
    and type stop_reason = GenServer.reason
end =
struct
  type msg =
      [ Socket.msg | packet_msg | `Parse | GenServer.msg ]

  type init_data = string

  type stop_reason = GenServer.reason

  type conn_state =
    | Recv_status
    | Recv_challenge
    | Recv_challenge_ack of int
    | Connection_established

  type state =
      {pid : msg pid;
       node : string;
       socket : Socket.socket;
       state: conn_state;
       p : Packet.t;
      }

  let send_text socket text =
    Socket.send_async socket text

  let send_packet state packet =
    send_text state (Packet.decorate `BE2 packet)

  let make_send_name_req () =
    let node = !nodename ^ "@" ^ !nodehost in
    let flags =
      dflag_published lor
	dflag_extended_references lor
	dflag_extended_pids_ports
    in
    let b = Buffer.create 20 in
      Buffer.add_char b 'n';
      add_int16_be b 5;
      add_int32_be b flags;
      Buffer.add_string b node;
      Buffer.contents b

  let make_challenge_reply challenge digest =
    let b = Buffer.create 20 in
      Buffer.add_char b 'r';
      add_int32_be b challenge;
      Buffer.add_string b digest;
      Buffer.contents b

  let init node self =
    add_node_connection node self;
    try_lwt
      (match_lwt lookup_node node with
	 | Some port -> (
	     let (_nodename, hostname) = node_to_namehost node in
	     lwt addr = get_addr_exn hostname in
	       match_lwt open_socket' addr port with
		 | Some socket -> (
		     let socket = Socket.of_fd socket self in
		       ignore (Socket.activate socket self);
		       send_packet socket (make_send_name_req ());
		       let state = Recv_status in
			 Lwt.return
			   (`Continue
			      {pid = self;
			       node;
			       socket;
			       state;
			       p = Packet.create `BE2;
			      })
		   )
		 | None -> raise_lwt Not_found
	   )
	 | None -> raise_lwt Not_found
      )
    with
      | _exn ->
	  remove_node_connection node;
	  Lwt.return `Init_failed

  let open_socket addr port self =
    (*let timeout = 1.0 in*)
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let addr = Unix.ADDR_INET (addr, port) in
    lwt () = Lwt_unix.connect socket addr in
    let tcpsock = Socket.of_fd socket self in
      ignore (Socket.activate tcpsock self);
      Lwt.return tcpsock

  let close_socket socket =
    Socket.close socket

  let handle_msg msg state =
    match state.state, msg with
      | Recv_status, `Packet data ->
	  if data.[0] = 's' then (
	    match data with
	      | "sok"
	      | "sok_simultaneous" ->
		  Lwt.return (`Continue {state with state = Recv_challenge})
	      | "salive" ->
		  send_packet state.socket "strue";
		  Lwt.return (`Continue {state with state = Recv_challenge})
	      | "snok"
	      | "snot_allowed"
	      | _ ->
		  lwt () = Lwt_log.error_f "Can't connect to %s" state.node in
		    Lwt.return (`Stop state)
	  ) else (
	    lwt () = Lwt_log.error_f "Can't connect to %s" state.node in
	      Lwt.return (`Stop state)
	  )
      | Recv_challenge, `Packet data ->
	  if data.[0] = 'n' then (
	    let schallenge = String.sub data 7 4 in
	    let challenge =
	      Int64.logor
		(Int64.shift_left (Int64.of_int (Char.code schallenge.[0])) 24)
		(Int64.of_int
		   ((Char.code schallenge.[1] lsl 16) lor
		      (Char.code schallenge.[2] lsl 8) lor
		      (Char.code schallenge.[3])))
	    in
	    let digest = Jlib.md5 (!cookie ^ Int64.to_string challenge) in
	    let mychallege = Random.int 1000000000 in
	      send_packet state.socket (make_challenge_reply mychallege digest);
	      Lwt.return (`Continue {state with
				       state = Recv_challenge_ack mychallege})
	  ) else (
	    lwt () = Lwt_log.error_f "Can't connect to %s" state.node in
	      Lwt.return (`Stop state)
	  )
      | Recv_challenge_ack mychallenge, `Packet data ->
	  if data.[0] = 'a' then (
	    let digest = Jlib.md5 (!cookie ^ string_of_int mychallenge) in
	    let digest' = String.sub data 1 (String.length data - 1) in
	      if digest = digest' then (
		Packet.change state.p Packet.BE4;
		Lwt.return (`Continue {state with
					 state = Connection_established})
	      ) else (
		lwt () = Lwt_log.error_f "Can't connect to %s" state.node in
		  Lwt.return (`Stop state)
	      )
	  ) else (
	    lwt () = Lwt_log.error_f "Can't connect to %s" state.node in
	      Lwt.return (`Stop state)
	  )
      | Connection_established, `Packet data ->
	  if String.length data = 0 then (
	    send_packet state.socket "";
	    Lwt.return (`Continue state)
	  ) else if data.[0] = 'p' then (
	    let (control, pos) = Erlang.binary_to_term data 1 in
	    lwt () =
	      Lwt_log.notice_f ~section
		"control message %s" (Erlang.term_to_string control)
	    in
	    let open Erlang in
	      match control with
		| ErlTuple [| ErlInt 2; _; _ |] ->
		    let (message, pos) = Erlang.binary_to_term data pos in
		    lwt () =
		      Lwt_log.notice_f ~section
			"message %s" (Erlang.term_to_string message)
		    in
		      Lwt.return (`Continue state)
		| ErlTuple [| ErlInt 6; _; _; _ |] ->
		    let (message, pos) = Erlang.binary_to_term data pos in
		    lwt () =
		      Lwt_log.notice_f ~section
			"message %s" (Erlang.term_to_string message)
		    in
		      Lwt.return (`Continue state)
		| _ ->
		    Lwt.return (`Continue state)
	  ) else (
	    lwt () = Lwt_log.error_f "Protocol error from %s" state.node in
	      Lwt.return (`Stop state)
	  )

  let parse state data =
    match Packet.parse state.p data with
      | Some packet ->
	  state.pid $! `Packet packet;
	  state.pid $! `Parse
      | None ->
	  ()

  let handle (msg : msg) state =
    match msg with
      | `Tcp_data (socket, data) -> (
	  lwt () =
	    Lwt_log.notice_f ~section
	      "tcp data %d %S" (String.length data) data
	  in
	    parse state data;
	    ignore (Socket.activate socket state.pid);
	    Lwt.return (`Continue state)
	)
      | `Tcp_close _socket -> (
	  lwt () = Lwt_log.debug ~section "tcp close" in
	    Lwt.return (`Stop state)
	)
      | #packet_msg as m ->
	  lwt () =
	    let `Packet data = m in
	    Lwt_log.notice_f ~section
	      "packet %S" data
	  in
	  handle_msg m state
      | `Parse ->
	  parse state "";
	  Lwt.return (`Continue state)
      | #GenServer.msg -> assert false

  let terminate state _reason =
    remove_node_connection state.node;
    lwt () = Socket.close state.socket in
      Lwt.return ()


end

module ErlNodeOutServer = GenServer.Make(ErlNodeOut)

let _ =
  ErlNodeOutServer.start "asd@localhost"
