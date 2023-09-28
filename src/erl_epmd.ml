open Process

let src = Jamler_log.new_src "erl_epmd"

module Packet = Jamler_packet
module GenServer = Gen_server

let full_nodename = ref "jamler@localhost"
let nodename = ref "jamler"
let nodehost = ref "localhost"
let node_creation = ref 0
let node_port = ref 0
let epmd_port = 4369
let cookie = ref ""

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

type msg += Packet of string
type unit_promise = (unit, exn) result Eio.Promise.u

module ErlEPMD :
sig
  include GenServer.Type with
    type init_data = unit_promise
    and type stop_reason = GenServer.reason
end =
struct
  type msg += Init

  type init_data = unit_promise

  type stop_reason = GenServer.reason

  type conn_state =
    | Open_socket of unit_promise
    | Wait_for_alive2_resp of unit_promise * Socket.socket
    | Connection_established of Socket.socket

  type state =
      {pid : pid;
       state: conn_state;
       p : Packet.t;
      }

  let send_text socket text =
    Socket.send socket text

  let send_packet state packet =
    send_text state (Packet.decorate `BE2 packet)

  let init wakener self =
    let state = Open_socket wakener in
    self $! Init;
    `Continue
      {pid = self;
       state;
       p = Packet.create `BE2;
      }

  let open_socket addr port self =
    (*let timeout = 1.0 in*)
    let socket =
      Eio.Net.connect
        ~sw:(Process.get_global_switch ())
        (Process.get_global_env ())#net
        (`Tcp (Eio_unix__Net.Ipaddr.of_unix addr, port))
    in
    let tcpsock = Socket.of_fd socket self in
    ignore (Socket.activate tcpsock self);
    tcpsock

  let _close_socket socket =
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
      Buffer.to_bytes b

  let handle_msg msg state =
    match state.state, msg with
    | Open_socket wakener, Init ->
       let socket =
	 try
	   open_socket Unix.inet_addr_loopback epmd_port state.pid
	 with
	 | exn ->
            Logs.err ~src
	      (fun m ->
                m "failed to open epmd connection: %a"
                  Jamler_log.pp_exn exn);
	    raise exn
       in
       send_packet socket (make_alive2_req !node_port !nodename);
       `Continue
	 {state with
	   state = Wait_for_alive2_resp (wakener, socket)}
    | Open_socket _, _ -> assert false
    | Wait_for_alive2_resp (wakener, socket), Packet data ->
       if String.length data >= 4 &&
	    data.[0] = '\121' &&
	      data.[1] = '\000'
       then (
	 let creation = Char.code data.[3] land 3 in
	 node_creation := creation;
	 Eio.Promise.resolve_ok wakener ();
	 `Continue
	   {state with
	     state = Connection_established socket}
       ) else (
	 Eio.Promise.resolve_error wakener (Failure "can't register nodename");
	 `Stop state
       )
    | Wait_for_alive2_resp (_wakener, _socket), _ -> assert false
    | Connection_established _socket, _ -> assert false

  let handle (msg : msg) state =
    match msg with
      | Socket.Tcp_data (socket, data) -> (
        Logs.debug ~src
	  (fun m ->
            m "tcp data %d %S" (String.length data) data);
	(*Packet.parse state.p data;*)
	state.pid $! Packet data;
	ignore (Socket.activate socket state.pid);
	`Continue state
      )
      | Socket.Tcp_close _socket -> (
        Logs.debug ~src (fun m -> m "tcp close");
	`Stop state
      )
      | Packet _
      | Init as m ->
	 handle_msg m state
      | _ ->
         (* TODO: add a warning *)
	 `Continue state

  let terminate state _reason =
    let () =
      match state.state with
      | Open_socket wakener
      | Wait_for_alive2_resp (wakener, _) ->
	 Eio.Promise.resolve_error wakener (Failure "can't register nodename");
      | Connection_established _ -> ()
    in
    match state.state with
    | Open_socket _wakener -> ()
    | Wait_for_alive2_resp (_, socket)
    | Connection_established socket ->
       Socket.close socket


end

module ErlEPMDServer = GenServer.Make(ErlEPMD)

let start node cookie' =
  let idx = String.index node '@' in
  let nodename' = String.sub node 0 idx in
  let hostname' = String.sub node (idx + 1) (String.length node - idx - 1) in
    nodename := nodename';
    nodehost := hostname';
    full_nodename := node;
    cookie := cookie';
    let (waiter, wakener) = Eio.Promise.create () in
      ignore (ErlEPMDServer.start wakener);
      Eio.Promise.await_exn waiter

let get_addr_exn ascii_addr =
  let h =
    Eio_unix.run_in_systhread (fun () -> Unix.gethostbyname ascii_addr)
  in
  match Array.to_list h.Unix.h_addr_list with
  | [] -> raise Not_found
  | addr :: _ -> addr

let open_socket addr port f =
  Eio.Net.with_tcp_connect
    ~host:(Unix.string_of_inet_addr addr)
    ~service:(string_of_int port)
    (Process.get_global_env ())#net
    f

let open_socket' addr port =
  try
    let socket =
      Eio.Net.connect
        ~sw:(Process.get_global_switch ())
        (Process.get_global_env ())#net
        (`Tcp (Eio_unix__Net.Ipaddr.of_unix addr, port))
    in
    Some socket
  with
  | _exn ->
     None

let node_to_namehost node =
    let idx = String.index node '@' in
    let nodename = String.sub node 0 idx in
    let hostname = String.sub node (idx + 1) (String.length node - idx - 1) in
      (nodename, hostname)

let lookup_node node =
  try
    let (nodename, hostname) = node_to_namehost node in
    let addr = get_addr_exn hostname in
    open_socket addr epmd_port
      (fun socket ->
	let packet = Packet.decorate `BE2 (Bytes.of_string ("\122" ^ nodename)) in
        Eio.Flow.copy_string (Bytes.to_string packet) socket;
        let buf = Cstruct.create 64 in
        let c = Eio.Flow.single_read socket buf in
	if c >= 4 && Cstruct.get_byte buf 1 = 0 then (
	  Some (((Cstruct.get_byte buf 2) lsl 8) lor
                  (Cstruct.get_byte buf 3))
	) else raise Not_found
      )
  with
  | _ -> None

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

type msg += Node_connection of node_connection_msg

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
    type init_data = [ `Out of string | `In of Eio_unix.Net.stream_socket_ty Eio.Std.r ]
    and type stop_reason = GenServer.reason
end =
struct
  type msg += Parse

  type init_data = [ `Out of string | `In of Eio_unix.Net.stream_socket_ty Eio.Std.r ]

  type stop_reason = GenServer.reason

  type conn_state =
    | Recv_name
    | Recv_status
    | Recv_challenge
    | Recv_challenge_reply of int
    | Recv_challenge_ack of int
    | Connection_established

  type state =
      {pid : pid;
       node : string;
       socket : Socket.socket;
       state: conn_state;
       p : Packet.t;
       queue : node_connection_msg Queue.t;
      }

  let send_text socket text =
    Socket.send socket text

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
      Buffer.to_bytes b

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
      Buffer.to_bytes b

  let make_challenge_reply challenge digest =
    let b = Buffer.create 20 in
      Buffer.add_char b 'r';
      add_int32_be b challenge;
      Buffer.add_string b digest;
      Buffer.to_bytes b

  let make_challenge_ack digest =
    let b = Buffer.create 20 in
      Buffer.add_char b 'a';
      Buffer.add_string b digest;
      Buffer.to_bytes b

  let init node self =
    match node with
    | `Out node -> (
      add_node_connection node self;
      try
	(match lookup_node node with
	 | Some port -> (
	   let (_nodename, hostname) = node_to_namehost node in
	   let addr = get_addr_exn hostname in
	   match open_socket' addr port with
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
	     `Continue state
	   )
	   | None -> raise Not_found
	 )
	 | None -> raise Not_found
	)
      with
      | _exn ->
	 remove_node_connection node;
	 `Init_failed
    )
    | `In socket ->
       let socket = Socket.of_fd socket self in
       ignore (Socket.activate socket self);
       let state = Recv_name in
       `Continue
	 {pid = self;
	  node = "";
	  socket;
	  state;
	  p = Packet.create `BE2;
	  queue = Queue.create ();
	 }

(*
  let _open_socket addr port self =
    (*let timeout = 1.0 in*)
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let addr = Unix.ADDR_INET (addr, port) in
    let%lwt () = Lwt_unix.connect socket addr in
    let tcpsock = Socket.of_fd socket self in
      ignore (Socket.activate tcpsock self);
      Lwt.return tcpsock

  let _close_socket socket =
    Socket.close socket
 *)

  let switch_to_connection_established state =
    monitor_nodes_iter (fun pid -> pid $! Node_up state.node);
    Packet.change state.p Packet.BE4;
    `Continue {state with
	state = Connection_established}

  let handle_msg msg state =
    match state.state, msg with
    | Recv_name, `Packet data ->
       if data.[0] = 'n' && String.length data > 7 then (
	 let node = String.sub data 7 (String.length data - 7) in
	 send_packet state (Bytes.of_string "sok");
	 let challege = Random.int 1000000000 in
	 send_packet state (make_challenge_req challege);
	 `Continue
	   {state with
	     state = Recv_challenge_reply challege;
	     node}
       ) else (
	 `Stop state
       )
    | Recv_status, `Packet data ->
       if data.[0] = 's' then (
	 match data with
	 | "sok"
	 | "sok_simultaneous" ->
	    `Continue {state with state = Recv_challenge}
	 | "salive" ->
	    send_packet state (Bytes.of_string "strue");
	    `Continue {state with state = Recv_challenge}
	 | "snok"
	 | "snot_allowed"
	 | _ ->
            Logs.err ~src
	      (fun m ->
                m "Can't connect to %s" state.node);
	    `Stop state
       ) else (
         Logs.err ~src
	   (fun m ->
             m "Can't connect to %s" state.node);
	 `Stop state
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
	 `Continue {state with
	     state = Recv_challenge_ack mychallenge}
       ) else (
         Logs.err ~src
	   (fun m ->
             m "Can't connect to %s" state.node);
	 `Stop state
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
           Logs.err ~src
	     (fun m ->
               m "Can't connect to %s" state.node);
	   `Stop state
	 )
       ) else (
         Logs.err ~src
	   (fun m ->
             m "Can't connect to %s" state.node);
	 `Stop state
       )
    | Recv_challenge_ack mychallenge, `Packet data ->
       if data.[0] = 'a' then (
	 let digest = Jlib.md5 (!cookie ^ string_of_int mychallenge) in
	 let digest' = String.sub data 1 (String.length data - 1) in
	 if digest = digest' then (
	   switch_to_connection_established state
	 ) else (
           Logs.err ~src
	     (fun m ->
               m "Can't connect to %s" state.node);
	   `Stop state
	 )
       ) else (
         Logs.err ~src
	   (fun m ->
             m "Can't connect to %s" state.node);
	 `Stop state
       )
    | Connection_established, `Packet data ->
       if String.length data = 0 then (
	 send_packet state Bytes.empty;
	 `Continue state
       ) else if data.[0] = 'p' then (
	 let (control, pos) = Erlang.binary_to_term data 1 in
         Logs.debug ~src
	   (fun m ->
             m "%a control message %s"
	       pp_pid state.pid (Erlang.term_to_string control));
	 let open Erlang in
	 match control with
	 | ErlTuple [| ErlInt 2; _; _ |] ->
	    let (message, _pos) = Erlang.binary_to_term data pos in
            Logs.debug ~src
	      (fun m ->
                m "message %s" (Erlang.term_to_string message));
	    `Continue state
	 | ErlTuple [| ErlInt 6; _; _; ErlAtom name |] ->
	    let (message, _pos) = Erlang.binary_to_term data pos in
            Logs.debug ~src
	      (fun m ->
                m "message %s" (Erlang.term_to_string message));
	    (try
	       name $!! Erl message
	     with
	     | _ -> ()
	    );
	    `Continue state
	 | _ ->
	    `Continue state
       ) else (
         Logs.err ~src
	   (fun m ->
             m "Protocol error from %s" state.node);
	 `Stop state
       )

  let parse state data =
    match Packet.parse state.p data with
      | Some packet ->
	  state.pid $! Packet packet;
	  state.pid $! Parse
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
       `Continue state
    (* TODO: send queue *)
    | `Send (pid, term), Connection_established ->
       let open Erlang in
       let control = ErlTuple [| ErlInt 2; ErlAtom ""; ErlPid pid |] in
       let b = Buffer.create 10 in
       Buffer.add_char b 'p';
       term_to_buffer b control;
       term_to_buffer b term;
       Logs.debug ~src
	 (fun m ->
           m "%a send %S"
	     pp_pid state.pid
	     (Buffer.contents b));
       send_packet state (Buffer.to_bytes b);
       `Continue state
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
       Logs.debug ~src
	 (fun m ->
           m "%a send %S"
	     pp_pid state.pid
	     (Buffer.contents b));
       send_packet state (Buffer.to_bytes b);
       `Continue state

  let handle (msg : msg) state =
    match msg with
    | Socket.Tcp_data (socket, data) -> (
      Logs.debug ~src
	(fun m ->
          m "%a tcp data %d %S"
	    pp_pid state.pid
	    (String.length data) data);
      parse state data;
      ignore (Socket.activate socket state.pid);
      `Continue state
    )
    | Socket.Tcp_close _socket -> (
      Logs.debug ~src (fun m -> m "tcp close");
      `Stop state
    )
    | Packet data ->
       Logs.debug ~src (fun m -> m "packet %S" data);
       handle_msg (`Packet data) state
    | Parse ->
       parse state "";
       `Continue state
    | Node_connection m ->
       handle_send m state
    | _ ->
       (* TODO: add a warning *)
       `Continue state

  let terminate state _reason =
    Logs.info ~src
      (fun m ->
        m "%a terminated connection to %S" pp_pid state.pid state.node);
    remove_node_connection state.node;
    (match state.state with
     | Connection_established ->
	monitor_nodes_iter (fun pid -> pid $! Node_down state.node);
     | _ ->
	()
    );
    Socket.close state.socket;
    ()


end

module ErlNodeConnectionServer = GenServer.Make(ErlNodeConnection)

let try_connect node =
  match find_node_connection node with
    | Some _conn ->
	()
    | None ->
	let _conn = ErlNodeConnectionServer.start (`Out node) in
	  ()

let dist_send pid term =
  let node = Erlang.node_of_pid pid in
  match find_node_connection node with
  | Some conn ->
     conn $! Node_connection (`Send (pid, term))
  | None ->
     let conn = ErlNodeConnectionServer.start (`Out node) in
     conn $! Node_connection (`Send (pid, term))

let dist_send_by_name name node term =
  match find_node_connection node with
  | Some conn ->
     conn $! Node_connection (`SendName (name, term))
  | None ->
     let conn = ErlNodeConnectionServer.start (`Out node) in
     conn $! Node_connection (`SendName (name, term))

module ErlListener =
struct
  let sockaddr_to_string addr =
    let nameinfo =
      Unix.getnameinfo addr [Unix.NI_NUMERICHOST; Unix.NI_NUMERICSERV]
    in
      nameinfo.Unix.ni_hostname ^ ":" ^ nameinfo.Unix.ni_service

  let rec accept start listen_socket =
    let (socket, _peer) =
      Eio.Net.accept
        ~sw:(Process.get_global_switch ())
        listen_socket
    in
    let fd = Eio_unix.Net.fd socket in
    let peername = Eio_unix.Fd.use_exn "getpeername" fd Unix.getpeername in
    let sockname = Eio_unix.Fd.use_exn "getsockname" fd Unix.getsockname in
    Logs.info ~src
      (fun m ->
        m "accepted connection %s -> %s"
	  (sockaddr_to_string peername)
	  (sockaddr_to_string sockname));
    let pid = ErlNodeConnectionServer.start (`In socket) in
    Logs.info ~src
      (fun m ->
        m "%a is handling connection %s -> %s"
	  pp_pid pid
	  (sockaddr_to_string peername)
	  (sockaddr_to_string sockname));
    accept start listen_socket

  let set_port socket =
    let addr = Unix.getsockname socket in
    let nameinfo =
      Unix.getnameinfo addr [Unix.NI_NUMERICHOST; Unix.NI_NUMERICSERV]
    in
      node_port := int_of_string nameinfo.Unix.ni_service

  let start _self =
    Process.dist_send_ref := dist_send;
    Process.dist_send_by_name_ref := dist_send_by_name;
    let addr = `Tcp (Eio.Net.Ipaddr.V4.any, 0) in
    let socket =
      Eio.Net.listen ~reuse_addr:true ~backlog:1024
        ~sw:(Process.get_global_switch ())
        (Process.get_global_env ())#net
        addr
    in
    let fd = Eio_unix.Net.fd socket in
    Eio_unix.Fd.use_exn "set_port" fd set_port;
    accept start socket
end

module ErlNetKernel :
sig
  include GenServer.Type with
    type init_data = unit
    and type stop_reason = GenServer.reason
end =
struct
  type init_data = unit

  type stop_reason = GenServer.reason

  type state =
      {pid : pid;
      }

  let init () self =
    register self "net_kernel";
    `Continue {pid = self}

  open Erlang

  let handle_call request from state =
    match request, from with
    | ErlTuple [| ErlAtom "is_auth"; _ |], ErlTuple [| ErlPid pid; tag |] ->
       let m = ErlTuple [| tag; ErlAtom "yes" |] in
       pid $!!! m;
       `Continue state
    | _ ->
       `Continue state

  let handle (msg : msg) state =
    match msg with
      (*| #packet_msg as m ->
	  lwt () =
	    let `Packet data = m in
	      Lwt_log.notice_f ~section
		"packet %S" data
	  in
	    handle_call m state*)
    | Erl (ErlTuple [| ErlAtom "$gen_call"; from; request |]) ->
       handle_call request from state
    | Erl term ->
       Logs.info ~src
	 (fun m ->
           m "unexpected packet %s" (Erlang.term_to_string term));
       `Continue state
    | _ -> assert false

  let terminate _state _reason =
    ()

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


let start_net () =
  ignore (ErlNetKernelServer.start ());
  (*ErlGlobalServer.start ();*)
  ignore (spawn ErlListener.start);
  (*ErlNodeConnectionServer.start (`Out "asd@localhost")*)
  ()
