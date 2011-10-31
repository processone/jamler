open Process
module GenServer = Gen_server

(* Wont' work on 32-bit arch *)
let hash s =
  let h = ref 0 in
    for i = 0 to String.length s - 1 do
      h := !h + Char.code s.[i];
      h := !h + (!h lsl 10);
      h := !h land 0xffffffff;
      h := !h lxor (!h lsr 6);
    done;
    h := !h + (!h lsl 3);
    h := !h land 0xffffffff;
    h := !h lxor (!h lsr 11);
    h := !h + (!h lsl 15);
    h := !h land 0x3fffffff;
    !h

let hash_user (luser : Jlib.nodepreped) (lserver : Jlib.namepreped) =
  let luser = (luser :> string) in
  let lserver = (lserver :> string) in
  let h = ref 0 in
    for i = 0 to String.length luser - 1 do
      h := !h + Char.code luser.[i];
      h := !h + (!h lsl 10);
      h := !h land 0xffffffff;
      h := !h lxor (!h lsr 6);
    done;
    h := !h + Char.code '@';
    h := !h + (!h lsl 10);
    h := !h land 0xffffffff;
    h := !h lxor (!h lsr 6);
    for i = 0 to String.length lserver - 1 do
      h := !h + Char.code lserver.[i];
      h := !h + (!h lsl 10);
      h := !h land 0xffffffff;
      h := !h lxor (!h lsr 6);
    done;
    h := !h + (!h lsl 3);
    h := !h land 0xffffffff;
    h := !h lxor (!h lsr 11);
    h := !h + (!h lsl 15);
    h := !h land 0x3fffffff;
    !h


module JamlerCluster :
sig
  include GenServer.Type with
    type msg = [ univ_msg | monitor_nodes_msg | GenServer.msg ]
    and type init_data = unit
    and type stop_reason = GenServer.reason

  val get_nodes_by_hash : int -> int -> string list
end =
struct
  type msg = [ univ_msg | monitor_nodes_msg | GenServer.msg ]

  type init_data = unit

  type stop_reason = GenServer.reason

  type state =
      {pid : msg pid;
      }

  let section = Jamler_log.new_section "jamler_cluster"

  let name = "ejabberd_cluster"

  module OrderedInt =
  struct
    type t = int
    let compare (x : int) (y : int) =
      if x < y
      then -1
      else if x = y
      then 0
      else 1
  end
  module IntMap = Treap_map.Make(OrderedInt)

  let global_host = Jlib.nameprep_exn ""

  let cluster_nodes = Hashtbl.create 10
  let nodes_hash = ref IntMap.empty

  let dump_nodes () =
    IntMap.iter
      (fun h node ->
	 Printf.printf "%d\t%s\n%!" h node
      ) !nodes_hash

  let add_node node =
    if not (Hashtbl.mem cluster_nodes node) then (
      Hashtbl.replace cluster_nodes node ();
      nodes_hash := IntMap.add (hash node) node !nodes_hash;
      Jamler_hooks.run_plain Jamler_sm.node_up_hook global_host node
; dump_nodes ()
    )

  let delete_node node =
    if Hashtbl.mem cluster_nodes node then (
      Hashtbl.remove cluster_nodes node;
      nodes_hash := IntMap.remove (hash node) !nodes_hash;
      Jamler_hooks.run_plain Jamler_sm.node_down_hook global_host node
; dump_nodes ()
    )

  let get_nodes () =
    Hashtbl.fold (fun n _ acc -> n :: acc) cluster_nodes []

  let get_next_node hash =
    match IntMap.next hash !nodes_hash with
      | None -> (
	  try
	    IntMap.min_elt !nodes_hash
	  with
	    | Not_found ->
		assert false
	)
      | Some nh ->
	  nh

  let rec get_nodes_by_hash hash k =
    if k = 0
    then []
    else (
      let (hash, node) = get_next_node hash in
	if node <> Erl_epmd.node ()
	then node :: get_nodes_by_hash hash (k - 1)
        else []
    )

  open Erlang

  let store nodes user server resource ts priority owner =
    let module SM = Jamler_sm in
      match owner, nodes with
	| SM.External _, _
	| SM.Local _, [] ->
	    ()
	| SM.Local _pid, _ ->
	    let user = (user : Jlib.nodepreped :> string) in
	    let server = (server : Jlib.namepreped :> string) in
	    let resource = (resource : Jlib.resourcepreped :> string) in
	    let usr =
	      ErlTuple [| ErlString user;
			  ErlString server;
			  ErlString resource;
		       |]
	    in
	    let priority =
	      if priority >= -1
	      then ErlInt priority
	      else ErlAtom "undefined"
	    in
	    let owner =
	      ErlTuple [| ErlAtom (Erl_epmd.node ());
			  ErlTuple [| ErlFloat ts;
				      ErlBinary user;
				      ErlBinary server;
				      ErlBinary resource;
				   |]
		       |]
	    in
	      List.iter
		(fun node ->
		   dist_send_by_name name node
		     (ErlTuple [| ErlAtom "store";
				  usr;
				  ErlFloat ts;
				  priority;
				  owner |])
		) nodes

  let remove nodes user server resource ts owner =
    let module SM = Jamler_sm in
      match owner, nodes with
	| SM.External _, _
	| SM.Local _, [] ->
	    ()
	| SM.Local _pid, _ ->
	    let user = (user : Jlib.nodepreped :> string) in
	    let server = (server : Jlib.namepreped :> string) in
	    let resource = (resource : Jlib.resourcepreped :> string) in
	    let usr =
	      ErlTuple [| ErlString user;
			  ErlString server;
			  ErlString resource;
		       |]
	    in
	    let owner =
	      ErlTuple [| ErlAtom (Erl_epmd.node ());
			  ErlTuple [| ErlFloat ts;
				      ErlBinary user;
				      ErlBinary server;
				      ErlBinary resource;
				   |]
		       |]
	    in
	      List.iter
		(fun node ->
		   dist_send_by_name name node
		     (ErlTuple [| ErlAtom "remove";
				  usr;
				  ErlFloat ts;
				  owner |])
		) nodes

  let init () self =
    register (self :> univ_msg Process.pid) name;
    Jamler_sm.cluster_store := store;
    Jamler_sm.cluster_remove := remove;
    add_node (Erl_epmd.node ());
    monitor_nodes (self :> monitor_nodes_msg Process.pid) true;
    List.iter
      (fun node ->
	 dist_send_by_name name node
	   (ErlTuple [| ErlAtom "ready";
			ErlAtom (Erl_epmd.node ()) |]);
	 dist_send_by_name name node
	   (ErlTuple [| ErlAtom "node_up";
			ErlAtom (Erl_epmd.node ());
			ErlCons (ErlAtom (Erl_epmd.node ()), ErlNil) |])
      ) (Erl_epmd.get_nodes ());
    Lwt.return (`Continue {pid = self})

(*
  let handle_call request from state =
    match request, from with
      | ErlTuple [| ErlAtom "is_auth"; _ |], ErlTuple [| ErlPid pid; tag |] ->
	  let m = ErlTuple [| tag; ErlAtom "yes" |] in
	    pid $!!! m;
	    Lwt.return (`Continue state)
      | _ ->
	  Lwt.return (`Continue state)
*)

  let handle (msg : msg) state =
    match msg with
      (*| #packet_msg as m ->
	  lwt () =
	    let `Packet data = m in
	      Lwt_log.notice_f ~section
		"packet %S" data
	  in
	    handle_call m state*)
      (*| `Erl (ErlTuple [| ErlAtom "$gen_call"; from; request |]) ->
	  handle_call request from state*)
      | `Erl (ErlTuple [| ErlAtom "node_up"; ErlAtom node;
			  nodes |] as term) -> (
	  lwt () =
	    Lwt_log.notice_f ~section
	      "node_up from %s" node
	  in
	    try
	      let nodes = ErlType.(from_term (list atom) nodes) in
		add_node node;
		List.iter
		  (fun n ->
		     Erl_epmd.try_connect n
		       (*add_node n*)
		  ) nodes;
		Lwt.return (`Continue state)
	    with
	      | Invalid_argument "from_term" ->
		  lwt () =
		    Lwt_log.notice_f ~section
		      "invalid node_up packet from %s %s"
		      node
		      (Erlang.term_to_string term)
		  in
		    Lwt.return (`Continue state)
	)
      | `Erl (ErlTuple
		[| ErlAtom "store";
		   ErlTuple [| ErlString u; ErlString s; ErlString r |];
		   ErlFloat ts;
		   priority;
		   ErlPid pid (* TODO *) |] as term) -> (
	  lwt () =
	    Lwt_log.notice_f ~section
	      "store %s" (Erlang.term_to_string term)
	  in
	  let module SM = Jamler_sm in
	  let sid = (ts, SM.External pid) in
	  let user = Jlib.nodeprep_exn u in
	  let server = Jlib.nameprep_exn s in
	  let resource = Jlib.resourceprep_exn r in
	  let priority =
	    match priority with
	      | ErlInt p when p >= 0 -> p
	      | ErlInt _ -> -1
	      | _ -> -2
	  in
	  let info = [] in
	    SM.open_session sid user server resource priority info [];
	    Lwt.return (`Continue state)
	)
      | `Erl (ErlTuple
		[| ErlAtom "remove";
		   ErlTuple [| ErlString u; ErlString s; ErlString r |];
		   ErlFloat ts;
		   ErlPid pid (* TODO *) |] as term) -> (
	  lwt () =
	    Lwt_log.notice_f ~section
	      "remove %s" (Erlang.term_to_string term)
	  in
	  let module SM = Jamler_sm in
	  let sid = (ts, SM.External pid) in
	  let user = Jlib.nodeprep_exn u in
	  let server = Jlib.nameprep_exn s in
	  let resource = Jlib.resourceprep_exn r in
	  let info = [] in
	    SM.close_session sid user server resource info [];
	    Lwt.return (`Continue state)
	)
      | `Node_up node ->
	  lwt () =
	    Lwt_log.notice_f ~section
	      "node %s goes up" node
	  in
	  let nodes = get_nodes () in
	    dist_send_by_name name node
	      (ErlTuple [| ErlAtom "ready";
			   ErlAtom (Erl_epmd.node ()) |]);
	    dist_send_by_name name node
	      (ErlTuple [| ErlAtom "node_up";
			   ErlAtom (Erl_epmd.node ());
			   ErlType.(to_term (list atom) nodes) |]);
	    Lwt.return (`Continue state)
      | `Erl (ErlTuple [| ErlAtom "ready"; ErlAtom node |]) ->
	  lwt () =
	    Lwt_log.notice_f ~section
	      "node %s is ready" node
	  in
	  let nodes = get_nodes () in
	    dist_send_by_name name node
	      (ErlTuple [| ErlAtom "node_up";
			   ErlAtom (Erl_epmd.node ());
			   ErlType.(to_term (list atom) nodes) |]);
	    Lwt.return (`Continue state)
      | `Node_down node ->
	  lwt () =
	    Lwt_log.notice_f ~section
	      "node %s goes down" node
	  in
	    delete_node node;
	    Lwt.return (`Continue state)
      | `Erl term ->
	  lwt () =
	    Lwt_log.notice_f ~section
	      "unexpected packet %s" (Erlang.term_to_string term)
	  in
	    Lwt.return (`Continue state)
      | #GenServer.msg -> assert false

  let terminate _state _reason =
    Lwt.return ()

end

module JamlerClusterServer = GenServer.Make(JamlerCluster)


let start () =
  JamlerClusterServer.start ()
