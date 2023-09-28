open Process
module GenServer = Gen_server
module Hooks = Jamler_hooks

[@@@warning "-52"]

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

let node_up_hook : string Hooks.hook = Hooks.create ()
let node_down_hook : string Hooks.hook = Hooks.create ()

let sm_store = ref (fun _u _s _r _ts _priority _pid -> ())
let sm_remove = ref (fun _u _s _r _ts _pid -> ())


module JamlerCluster :
sig
  include GenServer.Type with
    type init_data = unit
    and type stop_reason = GenServer.reason

  val get_nodes_by_hash : int -> int -> string list
  val get_node_by_hash : int -> string
end =
struct
  type init_data = unit

  type stop_reason = GenServer.reason

  type state =
      {pid : pid;
      }

  let src = Jamler_log.new_src "jamler_cluster"

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
      Jamler_hooks.run node_up_hook global_host node
; dump_nodes ()
    )

  let delete_node node =
    if Hashtbl.mem cluster_nodes node then (
      Hashtbl.remove cluster_nodes node;
      nodes_hash := IntMap.remove (hash node) !nodes_hash;
      Jamler_hooks.run node_down_hook global_host node
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

  let get_node_by_hash hash =
    snd (get_next_node hash)

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

  let init () self =
    register self name;
    add_node (Erl_epmd.node ());
    monitor_nodes self true;
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
    `Continue {pid = self}

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
    | Erl (ErlTuple [| ErlAtom "node_up"; ErlAtom node;
		       nodes |] as term) -> (
      Logs.info ~src
	(fun m ->
          m "node_up from %s" node);
      try
	let nodes = ErlType.(from_term (list atom) nodes) in
	add_node node;
	List.iter
	  (fun n ->
	    Erl_epmd.try_connect n
	  (*add_node n*)
	  ) nodes;
	`Continue state
      with
      | Invalid_argument "from_term" ->
         Logs.info ~src
	   (fun m ->
             m "invalid node_up packet from %s %s"
	       node
	       (Erlang.term_to_string term));
	 `Continue state
    )
    | Erl (ErlTuple
	     [| ErlAtom "store";
		ErlTuple [| ErlBinary u; ErlBinary s; ErlBinary r |];
		ErlFloat ts;
		priority;
		(ErlPid _ | ErlTuple [| ErlAtom _; _ |] ) as owner
	     |] as term) -> (
      Logs.info ~src
	(fun m ->
          m "store %s" (Erlang.term_to_string term));
      !sm_store u s r ts priority owner;
      `Continue state
    )
    | Erl (ErlTuple
	     [| ErlAtom "remove";
		ErlTuple [| ErlBinary u; ErlBinary s; ErlBinary r |];
		ErlFloat ts;
		(ErlPid _ | ErlTuple [| ErlAtom _; _ |] ) as owner
	     |] as term) -> (
      Logs.info ~src
	(fun m ->
          m "remove %s" (Erlang.term_to_string term));
      !sm_remove u s r ts owner;
      `Continue state
    )
    | Node_up node ->
       Logs.info ~src
	 (fun m ->
           m "node %s goes up" node);
       let nodes = get_nodes () in
       dist_send_by_name name node
	 (ErlTuple [| ErlAtom "ready";
		      ErlAtom (Erl_epmd.node ()) |]);
       dist_send_by_name name node
	 (ErlTuple [| ErlAtom "node_up";
		      ErlAtom (Erl_epmd.node ());
		      ErlType.(to_term (list atom) nodes) |]);
       `Continue state
    | Erl (ErlTuple [| ErlAtom "ready"; ErlAtom node |]) ->
       Logs.info ~src
	 (fun m ->
           m "node %s is ready" node);
       let nodes = get_nodes () in
       dist_send_by_name name node
	 (ErlTuple [| ErlAtom "node_up";
		      ErlAtom (Erl_epmd.node ());
		      ErlType.(to_term (list atom) nodes) |]);
       `Continue state
    | Node_down node ->
       Logs.info ~src
	 (fun m ->
           m "node %s goes down" node);
       delete_node node;
       `Continue state
    | Erl term ->
       Logs.info ~src
	 (fun m ->
           m "unexpected packet %s" (Erlang.term_to_string term));
       `Continue state
    | _ -> assert false

  let terminate _state _reason =
    ()

end

module JamlerClusterServer = GenServer.Make(JamlerCluster)


let start () =
  JamlerClusterServer.start ()
