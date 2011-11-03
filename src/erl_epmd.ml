open Process

let section = Jamler_log.new_section "erl_epmd"

module Packet = Jamler_packet
module GenServer = Gen_server

let full_nodename = ref "jamler@localhost" (* TODO *)
let nodename = ref "jamler"		(* TODO *)
let nodehost = ref "localhost"		(* TODO *)
let node_creation = ref 0
let node_port = ref 0
let epmd_port = 4369
let cookie = ref "YDZZQPNLWAMUSODVCMLA"	(* TODO *)

let node () = !full_nodename

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
      add_int16_be b 0;
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
	    send_packet socket (make_alive2_req !node_port !nodename);
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

let start node =
  let idx = String.index node '@' in
  let nodename' = String.sub node 0 idx in
  let hostname' = String.sub node (idx + 1) (String.length node - idx - 1) in
    nodename := nodename';
    nodehost := hostname';
    full_nodename := node;
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


type node_connection_msg =
    [ `Send of Erlang.pid * Erlang.erl_term
    | `SendName of string * Erlang.erl_term
    ]
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
let get_nodes () =
  Hashtbl.fold (fun n _ acc -> n :: acc) nodes []

module ErlNodeConnection :
sig
  include GenServer.Type with
    type msg =
        [ Socket.msg | packet_msg | `Parse | node_connection_msg
	| GenServer.msg ]
    and type init_data = [ `Out of string | `In of Lwt_unix.file_descr ]
    and type stop_reason = GenServer.reason
end =
struct
  type msg =
      [ Socket.msg | packet_msg | `Parse | node_connection_msg
      | GenServer.msg ]

  type init_data = [ `Out of string | `In of Lwt_unix.file_descr ]

  type stop_reason = GenServer.reason

  type conn_state =
    | Recv_name
    | Recv_status
    | Recv_challenge
    | Recv_challenge_reply of int
    | Recv_challenge_ack of int
    | Connection_established

  type state =
      {pid : msg pid;
       node : string;
       socket : Socket.socket;
       state: conn_state;
       p : Packet.t;
       queue : node_connection_msg Queue.t;
      }

  let send_text socket text =
    Socket.send_async socket text

  let send_packet state packet =
    let packet_type =
      match state.state with
	| Recv_name
	| Recv_status
	| Recv_challenge
	| Recv_challenge_reply _
	| Recv_challenge_ack _ -> `BE2
	| Connection_established -> `BE4
    in
      send_text state.socket (Packet.decorate packet_type packet)

  let make_send_name_req () =
    let node = !nodename ^ "@" ^ !nodehost in
    let flags =
      dflag_published lor
	dflag_extended_references lor
	dflag_extended_pids_ports lor
	dflag_new_floats
    in
    let b = Buffer.create 20 in
      Buffer.add_char b 'n';
      add_int16_be b 5;
      add_int32_be b flags;
      Buffer.add_string b node;
      Buffer.contents b

  let make_challenge_req challenge =
    let node = !nodename ^ "@" ^ !nodehost in
    let flags =
      dflag_published lor
	dflag_extended_references lor
	dflag_extended_pids_ports lor
	dflag_new_floats
    in
    let b = Buffer.create 20 in
      Buffer.add_char b 'n';
      add_int16_be b 5;
      add_int32_be b flags;
      add_int32_be b challenge;
      Buffer.add_string b node;
      Buffer.contents b

  let make_challenge_reply challenge digest =
    let b = Buffer.create 20 in
      Buffer.add_char b 'r';
      add_int32_be b challenge;
      Buffer.add_string b digest;
      Buffer.contents b

  let make_challenge_ack digest =
    let b = Buffer.create 20 in
      Buffer.add_char b 'a';
      Buffer.add_string b digest;
      Buffer.contents b

  let init node self =
    match node with
      | `Out node -> (
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
			     let state = Recv_status in
			     let state =
			       {pid = self;
				node;
				socket;
				state;
				p = Packet.create `BE2;
				queue = Queue.create ();
			       }
			     in
			       send_packet state (make_send_name_req ());
			       Lwt.return (`Continue state)
			 )
		       | None -> raise_lwt Not_found
		 )
	       | None -> raise_lwt Not_found
	    )
	  with
	    | _exn ->
		remove_node_connection node;
		Lwt.return `Init_failed
	)
      | `In socket ->
	  let socket = Socket.of_fd socket self in
	    ignore (Socket.activate socket self);
	    let state = Recv_name in
	      Lwt.return
		(`Continue
		   {pid = self;
		    node = "";
		    socket;
		    state;
		    p = Packet.create `BE2;
		    queue = Queue.create ();
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

  let switch_to_connection_established state =
    monitor_nodes_iter (fun pid -> pid $! `Node_up state.node);
    Packet.change state.p Packet.BE4;
    Lwt.return (`Continue {state with
			     state = Connection_established})

  let handle_msg msg state =
    match state.state, msg with
      | Recv_name, `Packet data ->
	  if data.[0] = 'n' && String.length data > 7 then (
	    let node = String.sub data 7 (String.length data - 7) in
	      send_packet state "sok";
	      let challege = Random.int 1000000000 in
		send_packet state (make_challenge_req challege);
		Lwt.return
		  (`Continue
		     {state with
			state = Recv_challenge_reply challege;
			node})
	  ) else (
	    Lwt.return (`Stop state)
	  )
      | Recv_status, `Packet data ->
	  if data.[0] = 's' then (
	    match data with
	      | "sok"
	      | "sok_simultaneous" ->
		  Lwt.return (`Continue {state with state = Recv_challenge})
	      | "salive" ->
		  send_packet state "strue";
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
	    let mychallenge = Random.int 1000000000 in
	      send_packet state
		(make_challenge_reply mychallenge digest);
	      Lwt.return (`Continue {state with
				       state = Recv_challenge_ack mychallenge})
	  ) else (
	    lwt () = Lwt_log.error_f "Can't connect to %s" state.node in
	      Lwt.return (`Stop state)
	  )
      | Recv_challenge_reply mychallenge, `Packet data ->
	  if data.[0] = 'r' && String.length data >= 5 then (
	    let digest = Jlib.md5 (!cookie ^ string_of_int mychallenge) in
	    let digest' = String.sub data 5 (String.length data - 5) in
	      if digest = digest' then (
		let schallenge = String.sub data 1 4 in
		let challenge =
		  Int64.logor
		    (Int64.shift_left
		       (Int64.of_int (Char.code schallenge.[0])) 24)
		    (Int64.of_int
		       ((Char.code schallenge.[1] lsl 16) lor
			  (Char.code schallenge.[2] lsl 8) lor
			  (Char.code schallenge.[3])))
		in
		let digest = Jlib.md5 (!cookie ^ Int64.to_string challenge) in
		  send_packet state (make_challenge_ack digest);
		  add_node_connection state.node state.pid;
		  switch_to_connection_established state
	      ) else (
		lwt () = Lwt_log.error_f "Can't connect to %s" state.node in
		  Lwt.return (`Stop state)
	      )
	  ) else (
	    lwt () = Lwt_log.error_f "Can't connect to %s" state.node in
	      Lwt.return (`Stop state)
	  )
      | Recv_challenge_ack mychallenge, `Packet data ->
	  if data.[0] = 'a' then (
	    let digest = Jlib.md5 (!cookie ^ string_of_int mychallenge) in
	    let digest' = String.sub data 1 (String.length data - 1) in
	      if digest = digest' then (
		switch_to_connection_established state
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
	    send_packet state "";
	    Lwt.return (`Continue state)
	  ) else if data.[0] = 'p' then (
	    let (control, pos) = Erlang.binary_to_term data 1 in
	    lwt () =
	      Lwt_log.notice_f ~section
		"%a control message %s"
		format_pid state.pid (Erlang.term_to_string control)
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
		| ErlTuple [| ErlInt 6; _; _; ErlAtom name |] ->
		    let (message, pos) = Erlang.binary_to_term data pos in
		    lwt () =
		      Lwt_log.notice_f ~section
			"message %s" (Erlang.term_to_string message)
		    in
		      (try
			 name $!! `Erl message
		       with
			 | _ -> ()
		      );
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

  let handle_send msg state =
    match msg, state.state with
    | _, Recv_name
    | _, Recv_status
    | _, Recv_challenge
    | _, Recv_challenge_reply _
    | _, Recv_challenge_ack _ ->
	Queue.add msg state.queue;
	Lwt.return (`Continue state)
	  (* TODO: send queue *)
    | `Send (pid, term), Connection_established ->
	let open Erlang in
	let control = ErlTuple [| ErlInt 2; ErlAtom ""; ErlPid pid |] in
	let b = Buffer.create 10 in
	  Buffer.add_char b 'p';
	  term_to_buffer b control;
	  term_to_buffer b term;
	  lwt () =
	    Lwt_log.notice_f ~section
	      "%a send %S"
	      format_pid state.pid
	      (Buffer.contents b)
	  in
	    send_packet state (Buffer.contents b);
	    Lwt.return (`Continue state)
    | `SendName (name, term), Connection_established ->
	let open Erlang in
	let pid = make_pid (node ()) 0 0 in
	let control =
	  ErlTuple [| ErlInt 6; ErlPid pid; ErlAtom ""; ErlAtom name |]
	in
	let b = Buffer.create 10 in
	  Buffer.add_char b 'p';
	  term_to_buffer b control;
	  term_to_buffer b term;
	  lwt () =
	    Lwt_log.notice_f ~section
	      "%a send %S"
	      format_pid state.pid
	      (Buffer.contents b)
	  in
	    send_packet state (Buffer.contents b);
	    Lwt.return (`Continue state)

  let handle (msg : msg) state =
    match msg with
      | `Tcp_data (socket, data) -> (
	  lwt () =
	    Lwt_log.notice_f ~section
	      "%a tcp data %d %S"
	      format_pid state.pid
	      (String.length data) data
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
      | #node_connection_msg as m ->
	  handle_send m state
      | #GenServer.msg -> assert false

  let terminate state _reason =
    lwt () =
      Lwt_log.notice_f ~section
	"%a terminated connection to %S" format_pid state.pid state.node
    in
      remove_node_connection state.node;
      (match state.state with
	 | Connection_established ->
	     monitor_nodes_iter (fun pid -> pid $! `Node_down state.node);
	 | _ ->
	     ()
      );
      lwt () = Socket.close state.socket in
	Lwt.return ()


end

module ErlNodeConnectionServer = GenServer.Make(ErlNodeConnection)

let try_connect node =
  match find_node_connection node with
    | Some conn ->
	()
    | None ->
	let _conn = ErlNodeConnectionServer.start (`Out node) in
	  ()

let dist_send pid term =
  let node = Erlang.node_of_pid pid in
    match find_node_connection node with
      | Some conn ->
	  conn $! `Send (pid, term)
      | None ->
	  let conn = ErlNodeConnectionServer.start (`Out node) in
	    conn $! `Send (pid, term)

let dist_send_by_name name node term =
  match find_node_connection node with
    | Some conn ->
	conn $! `SendName (name, term)
    | None ->
	let conn = ErlNodeConnectionServer.start (`Out node) in
	  conn $! `SendName (name, term)

module ErlListener =
struct
  let sockaddr_to_string addr =
    let nameinfo =
      Unix.getnameinfo addr [Unix.NI_NUMERICHOST; Unix.NI_NUMERICSERV]
    in
      nameinfo.Unix.ni_hostname ^ ":" ^ nameinfo.Unix.ni_service

  let rec accept start listen_socket =
    lwt (socket, _) = Lwt_unix.accept listen_socket in
    let peername = Lwt_unix.getpeername socket in
    let sockname = Lwt_unix.getsockname socket in
    lwt () =
      Lwt_log.notice_f
	~section
	"accepted connection %s -> %s"
	(sockaddr_to_string peername)
	(sockaddr_to_string sockname)
    in
    let pid = ErlNodeConnectionServer.start (`In socket) in
    lwt () =
      Lwt_log.notice_f
	~section
	"%a is handling connection %s -> %s"
	format_pid pid
	(sockaddr_to_string peername)
	(sockaddr_to_string sockname)
    in
      accept start listen_socket

  let set_port socket =
    let addr = Lwt_unix.getsockname socket in
    let nameinfo =
      Unix.getnameinfo addr [Unix.NI_NUMERICHOST; Unix.NI_NUMERICSERV]
    in
      node_port := int_of_string nameinfo.Unix.ni_service

  let start _self =
    Process.dist_send_ref := dist_send;
    Process.dist_send_by_name_ref := dist_send_by_name;
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let addr = Unix.ADDR_INET (Unix.inet_addr_any, 0) in
      Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
      Lwt_unix.bind socket addr;
      set_port socket;
      Lwt_unix.listen socket 1024;
      accept start socket
end

module ErlNetKernel :
sig
  include GenServer.Type with
    type msg = [ univ_msg | GenServer.msg ]
    and type init_data = unit
    and type stop_reason = GenServer.reason
end =
struct
  type msg = [ univ_msg | GenServer.msg ]

  type init_data = unit

  type stop_reason = GenServer.reason

  type state =
      {pid : msg pid;
      }

  let init () self =
    register (self :> univ_msg pid) "net_kernel";
    Lwt.return (`Continue {pid = self})

  open Erlang

  let handle_call request from state =
    match request, from with
      | ErlTuple [| ErlAtom "is_auth"; _ |], ErlTuple [| ErlPid pid; tag |] ->
	  let m = ErlTuple [| tag; ErlAtom "yes" |] in
	    pid $!!! m;
	    Lwt.return (`Continue state)
      | _ ->
	  Lwt.return (`Continue state)

  let handle (msg : msg) state =
    match msg with
      (*| #packet_msg as m ->
	  lwt () =
	    let `Packet data = m in
	      Lwt_log.notice_f ~section
		"packet %S" data
	  in
	    handle_call m state*)
      | `Erl (ErlTuple [| ErlAtom "$gen_call"; from; request |]) ->
	  handle_call request from state
      | `Erl term ->
	  lwt () =
	    Lwt_log.notice_f ~section
	      "unexpected packet %s" (Erlang.term_to_string term)
	  in
	    Lwt.return (`Continue state)
      | #GenServer.msg -> assert false

  let terminate state _reason =
    Lwt.return ()

end

module ErlNetKernelServer = GenServer.Make(ErlNetKernel)

(* Doesn't work *)
(*
module ErlGlobal :
sig
  include GenServer.Type with
    type msg = [ univ_msg | monitor_nodes_msg | GenServer.msg ]
    and type init_data = unit
    and type stop_reason = GenServer.reason
end =
struct
  type msg = [ univ_msg | monitor_nodes_msg | GenServer.msg ]

  type init_data = unit

  type stop_reason = GenServer.reason

  type state =
      {pid : msg pid;
       known : (string, unit) Hashtbl.t;
       resolvers : (string, unit) Hashtbl.t;
       sync_tag_my : (string, Erlang.erl_term) Hashtbl.t;
       the_locker : [ `Asd ] pid;
      }

  let vsn = 5

  let init () self =
    register (self :> univ_msg pid) "global_name_server";
    monitor_nodes (self :> monitor_nodes_msg pid) true;
    Lwt.return (`Continue {pid = self;
			   known = Hashtbl.create 10;
			   resolvers = Hashtbl.create 10;
			   sync_tag_my = Hashtbl.create 10;
			   the_locker;
			  })

  open Erlang

  let handle_call request from state =
    match request, from with
      (*| ErlTuple [| ErlAtom "is_auth"; _ |], ErlTuple [| ErlPid pid; tag |] ->
	  let m = ErlTuple [| tag; ErlAtom "yes" |] in
	    pid $!!! m;
	    Lwt.return (`Continue state)
      *)
      | _ ->
	  Lwt.return (`Continue state)

  let init_connect vsn node init_msg his_tag state =
    (* TODO *)
    Lwt.return (`Continue state)

  let handle_cast request state =
    match request with
      | ErlTuple [| ErlAtom "init_connect"; version;
		    ErlAtom node; init_msg |] -> (
	  match version with
	    | ErlTuple [| ErlInt his_vsn; his_tag |] when his_vsn > vsn ->
		init_connect vsn node init_msg his_tag state
	    | ErlTuple [| ErlInt his_vsn; his_tag |] ->
		init_connect his_vsn node init_msg his_tag state
	    | ErlTuple tuple -> (
		match Array.to_list tuple with
		  | ErlInt _his_vsn :: his_tag :: _ ->
		      init_connect vsn node init_msg his_tag state
		  | _ ->
		      assert false
	      )
	    | _ ->
		lwt () =
		  Lwt_log.notice_f ~section
		    "illegal global protocol version %s node: %S\n"
		    (term_to_string version) node
		in
		  Lwt.return (`Continue state)
	)
      | _ ->
	  Lwt.return (`Continue state)

  let handle (msg : msg) state =
    match msg with
      (*| #packet_msg as m ->
	  lwt () =
	    let `Packet data = m in
	      Lwt_log.notice_f ~section
		"packet %S" data
	  in
	    handle_call m state*)
      | `Node_up node ->
	  lwt () =
	    Lwt_log.notice_f ~section "node up %S" node
	  in
	  let is_known =
	    Hashtbl.mem state.known node ||
              Hashtbl.mem state.resolvers node
	  in
	    if is_known then (
	      Lwt.return (`Continue state)
	    ) else (
	      let mytag = ErlFloat (Unix.gettimeofday ()) in
		Hashtbl.replace state.sync_tag_my node mytag;
		state.the_locker $! `Node_up (node, mytag);
		let not_a_pid = ErlAtom "no_longer_a_pid" in
		let known =
		  Hashtbl.fold
		    (fun node () acc -> ErlCons (ErlString node, acc))
		    state.known ErlNil
		in
		let locker =
		  ErlTuple [| ErlAtom "locker"; not_a_pid;
			      S1#state.known;
			      ErlAtom "S1#state.the_locker TODO"
			   |]
		in
		let init_c =
		  ErlTuple [| ErlAtom "init_connect";
			      ErlTuple [| ErlInt vsn; mytag |];
			      !full_nodename;
			      locker |]
		in
		  dist_send_by_name node "global_name_server"
		    (ErlTuple [| ErlAtom "$gen_cast"; init_c |]);
		  let resolver = start_resolver node mytag in
		    Hashtbl.replace state.resolver node (mytag, resolver);
		    Lwt.return (`Continue state)
	    )
      | `Node_down node ->
	  lwt () =
	    Lwt_log.notice_f ~section "node down %S" node
	  in
	    Lwt.return (`Continue state)
      | `Erl (ErlTuple [| ErlAtom "$gen_call"; from; request |]) ->
	  handle_call request from state
      | `Erl (ErlTuple [| ErlAtom "$gen_cast"; request |]) ->
	  handle_cast request state
      | `Erl term ->
	  lwt () =
	    Lwt_log.notice_f ~section
	      "unexpected packet %s" (Erlang.term_to_string term)
	  in
	    Lwt.return (`Continue state)
      | #GenServer.msg -> assert false

  let terminate state _reason =
    Lwt.return ()

end

module ErlGlobalServer = GenServer.Make(ErlGlobal)
*)


let _ =
  ErlNetKernelServer.start ();
  (*ErlGlobalServer.start ();*)
  spawn ErlListener.start
  (*ErlNodeConnectionServer.start (`Out "asd@localhost")*)
