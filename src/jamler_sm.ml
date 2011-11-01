open Process

let section = Jamler_log.new_section "sm"

module LJID = Jlib.LJID
module Hooks = Jamler_hooks
module Auth = Jamler_auth
module Router = Jamler_router
module GenIQHandler = Jamler_gen_iq_handler
module GenServer = Gen_server

type broadcast =
    [ `RosterItem of LJID.t * [ `None | `From | `To | `Both | `Remove ] ]
type msg =
    [ Router.msg | `Broadcast of broadcast | `Replaced
    | `Node_up of string
    | `Node_down of string ]
type info = [ `TODO ] list

module Session :
sig
  type owner =
    | Local of msg pid
    | External of Erlang.pid
  type sid = float * owner

  type session =
      {usr : LJID.t;
       priority : int;
       info : info}

  val add : sid -> session -> unit
  val remove : sid -> unit
  val find_exn : sid -> session
  val find : sid -> session option
  val find_sids_by_usr :
    Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped -> sid list
  val find_sids_by_us : Jlib.nodepreped -> Jlib.namepreped -> sid list

  val dump_tables : unit -> unit
end
  =
struct
  type owner =
    | Local of msg pid
    | External of Erlang.pid
  type sid = float * owner

  module HashedSID =
  struct
    type t = sid
    let equal ((t1 : float), p1) (t2, p2) =
      t1 = t2 &&
      (match p1, p2 with
	 | Local p1, Local p2 ->
             Pid.equal p1 p2
	 | External p1, External p2 ->
	     p1 = p2
	 | Local _, External _
	 | External _, Local _ ->
	     false
      )
    let hash (t, p) =
      Hashtbl.hash t lxor
	(match p with
	   | Local p -> Pid.hash p
	   | External p -> Hashtbl.hash p
	)
  end
  module HashtblSID = Hashtbl.Make(HashedSID)

  type session =
      {usr : LJID.t;
       priority : int;
       info : info}

  let sessions = HashtblSID.create 100
  let usr_idx = Hashtbl.create 10

  let add_r_idx r_idx r sid =
    let sids =
      try
        Hashtbl.find r_idx r
      with
        | Not_found -> []
    in
    let sids = List.filter (fun s -> not (HashedSID.equal s sid)) sids in
      Hashtbl.replace r_idx r (sid :: sids)

  let add_ur_idx ur_idx u r sid =
    let r_idx =
      try
        Hashtbl.find ur_idx u
      with
        | Not_found ->
            let r_idx = Hashtbl.create 1 in
      	Hashtbl.add ur_idx u r_idx;
      	r_idx
    in
      add_r_idx r_idx r sid

  let add_usr_idx (u, s, r) sid =
    let ur_idx =
      try
        Hashtbl.find usr_idx s
      with
        | Not_found ->
            let ur_idx = Hashtbl.create 10 in
      	Hashtbl.add usr_idx s ur_idx;
      	ur_idx
    in
      add_ur_idx ur_idx u r sid

  let add sid session =
    HashtblSID.replace sessions sid session;
    add_usr_idx session.usr sid


  let remove_r_idx r_idx r sid =
    let sids =
      try
        Hashtbl.find r_idx r
      with
        | Not_found -> []
    in
    let sids = List.filter (fun s -> not (HashedSID.equal s sid)) sids in
      match sids with
        | [] ->
            Hashtbl.remove r_idx r
        | _ ->
            Hashtbl.replace r_idx r sids

  let remove_ur_idx ur_idx u r sid =
    let r_idx =
      try
        Hashtbl.find ur_idx u
      with
        | Not_found -> assert false
    in
      remove_r_idx r_idx r sid;
      if Hashtbl.length r_idx = 0
      then Hashtbl.remove ur_idx u

  let remove_usr_idx (u, s, r) sid =
    let ur_idx =
      try
        Hashtbl.find usr_idx s
      with
        | Not_found -> assert false
    in
      remove_ur_idx ur_idx u r sid;
      if Hashtbl.length ur_idx = 0
      then Hashtbl.remove usr_idx s

  let remove sid =
    try
      let session = HashtblSID.find sessions sid in
        HashtblSID.remove sessions sid;
        remove_usr_idx session.usr sid
    with
      | Not_found -> ()

  let find_exn sid =
    HashtblSID.find sessions sid

  let find sid =
    try
      Some (find_exn sid)
    with
      | Not_found -> None

  let find_sids_by_usr u s r =
    try
      let ur_idx = Hashtbl.find usr_idx s in
      let r_idx = Hashtbl.find ur_idx u in
      let sids = Hashtbl.find r_idx r in
        sids
    with
      | Not_found -> []

  let find_sids_by_us u s =
    try
      let ur_idx = Hashtbl.find usr_idx s in
      let r_idx = Hashtbl.find ur_idx u in
      let sids = Hashtbl.fold (fun _r sids acc -> sids @ acc) r_idx [] in
        sids
    with
      | Not_found -> []

  let node_up node =
    HashtblSID.iter
      (fun (_ts, owner) _ ->
	 match owner with
	   | Local pid ->
               pid $! `Node_up node
	   | External _ ->
	       ()
      ) sessions;
    Jamler_hooks.OK

  let node_down node =
    HashtblSID.iter
      (fun ((_ts, owner) as sid) _ ->
	 match owner with
           | Local pid ->
               pid $! `Node_down node
	   | External _ ->
	       remove sid
      ) sessions;
    Jamler_hooks.OK

  let () =
    let global_host = Jlib.nameprep_exn "" in
      Jamler_hooks.add_plain
	Jamler_cluster.node_up_hook global_host node_up 50;
      Jamler_hooks.add_plain
	Jamler_cluster.node_down_hook global_host node_down 50

  let dump_tables () =
    let string_of_sid (f, p) = Printf.sprintf "(%f, %d)" f (Obj.magic p) in
      HashtblSID.iter
        (fun sid session ->
           let (u, s, r) = session.usr in
             Printf.eprintf "sid:%s %s@%s/%s prio:%d\n" (string_of_sid sid)
      	 (u :> string) (s :> string) (r :> string) session.priority
        ) sessions;
      Hashtbl.iter
        (fun (s : Jlib.namepreped) ur_idx ->
           Printf.eprintf "%s:\n" (s :> string);
           Hashtbl.iter
             (fun (u : Jlib.nodepreped) r_idx ->
      		Printf.eprintf "  %s:\n" (u :> string);
      		Hashtbl.iter
      		  (fun (r : Jlib.resourcepreped) sids ->
      		     Printf.eprintf "    %s:\n" (r :> string);
      		     List.iter
      		       (fun sid ->
      			  Printf.eprintf "      sid:%s\n" (string_of_sid sid)
      		       ) sids
      		  ) r_idx;
             ) ur_idx;
        ) usr_idx;
      flush stderr;

end
include Session


let jid_to_term jid =
  let open Erlang in
    ErlTuple [| ErlAtom "jid";
		ErlString jid.Jlib.user;
		ErlString jid.Jlib.server;
		ErlString jid.Jlib.resource;
		ErlString (jid.Jlib.luser :> string);
		ErlString (jid.Jlib.lserver :> string);
		ErlString (jid.Jlib.lresource :> string);
	     |]

let send_owner owner msg =
  match owner with
    | Local pid ->
	pid $! msg
    | External pid ->
	let open Erlang in
	let msg =
	  match msg with
	    | `Route (from, to', packet) ->
		ErlTuple [| ErlAtom "route";
			    jid_to_term from;
			    jid_to_term to';
			    ErlType.(to_term xml packet)
			 |]
	    | `Broadcast _->
		ErlNil			(* TODO *)
	    | `Node_down _
	    | `Node_up _ ->
		ErlNil
	    | `Replaced ->
		ErlAtom "replaced"
	in
	  match msg with
	    | ErlNil -> ()
	    | _ -> pid $!!! msg

let cluster_store nodes user server resource ts priority owner =
  let open Erlang in
  let name = "ejabberd_cluster" in
    match owner, nodes with
      | External _, _
      | Local _, [] ->
	  ()
      | Local _pid, _ ->
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

let cluster_remove nodes user server resource ts owner =
  let open Erlang in
  let name = "ejabberd_cluster" in
    match owner, nodes with
      | External _, _
      | Local _, [] ->
	  ()
      | Local _pid, _ ->
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


let set_session sid user server resource priority info nodes =
  let usr = (user, server, resource) in
    add sid {usr; priority; info};
    let (ts, owner) = sid in
      cluster_store nodes user server resource ts priority owner
;dump_tables ()

let check_existing_resources user server resource =
  (* A connection exist with the same resource. We replace it: *)
  let sids = find_sids_by_usr user server resource in
    match sids with
      | [] -> ()
      | s :: sids' ->
          let max_sid = List.fold_left max s sids' in
            List.iter
      	(fun ((_, owner) as s) ->
      	   if s != max_sid then (
      	     send_owner owner `Replaced
      	   )
      	) sids


let max_user_sessions_access =
  Jamler_acl.get_rule "max_user_sessions" Jamler_config.int

(* default value for the maximum number of user connections *)
let max_user_sessions = max_int

(* Get the user_max_session setting
   This option defines the max number of time a given users are allowed to
   log in
   Defaults to infinity *)
let get_max_user_sessions user host =
  Jamler_acl.match_rule
    host max_user_sessions_access
    (Jlib.make_jid' user host (Jlib.resourceprep_exn ""))
    max_user_sessions


let check_max_sessions user server =
  (* If the max number of sessions for a given is reached, we replace the
     first one *)
  let sids = find_sids_by_us user server in
  let max_sessions = get_max_user_sessions user server in
    match sids with
      | s :: sids' when List.length sids > max_sessions ->
          let min_sid = List.fold_left min s sids' in
          let (_, pid) = min_sid in
            (* TODO *)
            ()
            (*Pid ! replaced*)
      | _ -> ()


(* On new session, check if some existing connections need to be replace *)
let check_for_sessions_to_replace user server resource =
  (* TODO: Depending on how this is executed, there could be an unneeded
     replacement for max_sessions. We need to check this at some point. *)
  check_existing_resources user server resource;
  check_max_sessions user server

let open_session sid user server resource priority info nodes =
  set_session sid user server resource priority info nodes;
  check_for_sessions_to_replace user server resource
  (*JID = jlib:make_jid(User, Server, Resource),
  ejabberd_hooks:run(sm_register_connection_hook, JID#jid.lserver,
      	       [SID, JID, Info]).*)

let do_close_session sid user server resource nodes =
  remove sid;
  let (ts, owner) = sid in
    cluster_remove nodes user server resource ts owner
;dump_tables ()

let close_session sid user server resource _info nodes =
  do_close_session sid user server resource nodes
  (*Info = do_close_session(SID),
  US = {jlib:nodeprep(User), jlib:nameprep(Server)},
  case ejabberd_cluster:get_node_new(US) of
      Node when Node /= node() ->
          rpc:cast(Node, ?MODULE, drop_session, [SID]);
      _ ->
          ok
  end,
  JID = jlib:make_jid(User, Server, Resource),
  ejabberd_hooks:run(sm_remove_connection_hook, JID#jid.lserver,
      	       [SID, JID, Info]).
  *)

let close_session_unset_presence sid user server resource _status info nodes =
  close_session sid user server resource info nodes
  (*ejabberd_hooks:run(unset_presence_hook, jlib:nameprep(Server),
      	       [User, Server, Resource, Status]).*)

let set_presence sid user server resource priority _presence info nodes =
  set_session sid user server resource priority info nodes
  (*ejabberd_hooks:run(set_presence_hook, jlib:nameprep(Server),
      	       [User, Server, Resource, Presence]).*)

let unset_presence sid user server resource status info nodes =
  set_session sid user server resource (-2) info nodes
  (*ejabberd_hooks:run(unset_presence_hook, jlib:nameprep(Server),
      	       [User, Server, Resource, Status]).*)

let clean_session_list sessions =
  let rec clean_session_list ss res =
    match ss with
      | [] -> res
      | [s] -> s :: res
      | ((sid1, ses1) as s1) :: ((((sid2, ses2) as s2) :: rest) as rest') ->
          if ses1.usr = ses2.usr then (
            if sid1 > sid2
            then clean_session_list (s1 :: rest) res
            else clean_session_list (s2 :: rest) res
          ) else clean_session_list rest' (s1 :: res)
  in
    clean_session_list
      (List.sort (fun (_, x) (_, y) -> compare x.usr y.usr) sessions) []

let get_user_resources luser lserver =
  let sids = find_sids_by_us luser lserver in
  let sessions = List.map (fun s -> (s, find_exn s)) sids in
  let sessions = clean_session_list sessions in
    List.map
      (fun (_sid, {usr = (_, _, r); _}) -> r)
      sessions

let get_user_present_resources luser lserver =
  let sids = find_sids_by_us luser lserver in
  let sessions = List.map (fun s -> (s, find_exn s)) sids in
  let sessions = clean_session_list sessions in
    List.fold_left
      (fun acc (sid, {priority; usr = (_, _, r); _}) ->
	 if priority >= -1
	 then (priority, r, sid) :: acc
	 else acc
      ) [] sessions

let bounce_offline_message from to' packet =
  let err = Jlib.make_error_reply packet Jlib.err_service_unavailable in
    Router.route to' from err
      (*stop.*)


let route_message from to' packet =
  let luser = to'.Jlib.luser in
  let lserver = to'.Jlib.lserver in
  let prio_res = get_user_present_resources luser lserver in
  let priority =
    match prio_res with
      | [] -> None
      | p :: prio_res' ->
          let (max_p, _, _) = List.fold_left max p prio_res' in
            if max_p >= 0
            then Some max_p
            else None
  in
    match priority with
      | Some priority ->
          (* Route messages to all priority that equals the max, if
             positive *)
          List.iter
            (fun (p, _r, sid) ->
      	       if p = priority then (
      		 let (_, owner) = sid in
      		   (*?DEBUG("sending to process ~p~n", [Pid]),*)
      		   send_owner owner (`Route (from, to', packet))
      	       )
            ) prio_res
      | _ -> (
          match Xml.get_tag_attr_s "type" packet with
            | "error" ->
      		()
            | "groupchat"
            | "headline" ->
      		bounce_offline_message from to' packet
            | _ -> (
      		match_lwt Auth.does_user_exist luser lserver with
      		  | true -> (
      		(* TODO *) Lwt.return ()
      		    (*case is_privacy_allow(From, To, Packet) of
      			true ->
      			    ejabberd_hooks:run(offline_message_hook,
      					       LServer,
      					       [From, To, Packet]);
      			false ->
      			    ok
      		    end;*)
      		    )
      		  | _ ->
      		      let err =
      			Jlib.make_error_reply
      			  packet Jlib.err_service_unavailable
      		      in
      			Router.route to' from err;
			Lwt.return ()
      	      );
		()
        )

let process_iq from to' packet =
  match Jlib.iq_query_info packet with
    | `IQ ({Jlib.iq_xmlns = xmlns; _} as iq) -> (
        let host = to'.Jlib.lserver in
	lwt handle_res =
	  GenIQHandler.handle `SM host xmlns from to' iq
	in
          if not handle_res then (
            let err =
      	      Jlib.make_error_reply packet Jlib.err_service_unavailable
            in
      	      Router.route to' from err
          );
	  Lwt.return ()
      )
    | `Reply -> Lwt.return ()
    | _ ->
        let err =
          Jlib.make_error_reply packet Jlib.err_bad_request
        in
          Router.route to' from err;
	  Lwt.return ()


(* The default list applies to the user as a whole,
   and is processed if there is no active list set
   for the target session/resource to which a stanza is addressed,
   or if there are no current sessions for the user.
*)
let is_privacy_allow from to' packet =
  true				(* TODO *)
(*
    User = To#jid.user,
    Server = To#jid.server,
    PrivacyList = ejabberd_hooks:run_fold(privacy_get_user_list, Server,
					  #userlist{}, [User, Server]),
    is_privacy_allow(From, To, Packet, PrivacyList).

%% Check if privacy rules allow this delivery
%% Function copied from ejabberd_c2s.erl
is_privacy_allow(From, To, Packet, PrivacyList) ->
    User = To#jid.user,
    Server = To#jid.server,
    allow == ejabberd_hooks:run_fold(
	       privacy_check_packet, Server,
	       allow,
	       [User,
		Server,
		PrivacyList,
		{From, To, Packet},
		in]).
*)

let roster_in_subscription = Hooks.create_fold ()

let rec do_route1 from to' packet =
  let {Jlib.luser = luser;
       Jlib.lserver = lserver;
       Jlib.lresource = lresource; _} = to' in
  let `XmlElement (name, attrs, _els) = packet in
    match (lresource :> string) with
      | "" -> (
	  match name with
	    | "presence" -> (
		ignore (
		  lwt pass =
		    match Xml.get_attr_s "type" attrs with
		      | "subscribe" ->
			  let reason =
			    Xml.get_path_s packet [`Elem "status"; `Cdata]
			  in
			    if is_privacy_allow from to' packet
			    then
			      (Hooks.run_fold
				 roster_in_subscription
				 lserver
				 false
				 (luser, lserver, from, `Subscribe, reason))
			    else Lwt.return false
		      | "subscribed" ->
			  if is_privacy_allow from to' packet
			  then
			    (Hooks.run_fold
			       roster_in_subscription
			       lserver
			       false
			       (luser, lserver, from, `Subscribed, ""))
			  else Lwt.return false
		      | "unsubscribe" ->
			  if is_privacy_allow from to' packet
			  then
			    (Hooks.run_fold
			       roster_in_subscription
			       lserver
			       false
			       (luser, lserver, from, `Unsubscribe, ""))
			  else Lwt.return false
		      | "unsubscribed" ->
			  if is_privacy_allow from to' packet
			  then
			    (Hooks.run_fold
			       roster_in_subscription
			       lserver
			       false
			       (luser, lserver, from, `Unsubscribed, ""))
			  else Lwt.return false
		      | _ -> Lwt.return true
		  in
		    Lwt.return (
		      if pass then (
			let presources =
			  get_user_present_resources luser lserver
			in
			  List.iter
			    (fun (_, r, _sid) ->
			       do_route1
				 from (Jlib.jid_replace_resource' to' r) packet
			    ) presources
		      )
		    )
		)
	      )
	    | "message" ->
		route_message from to' packet
	    | "iq" ->
		ignore (process_iq from to' packet)
	    | "broadcast" ->
		List.iter
		  (fun r ->
		     do_route1 from (Jlib.jid_replace_resource' to' r) packet
		  ) (get_user_resources luser lserver)
	    | _ ->
		()
	)
      | _ -> (
	  match find_sids_by_usr luser lserver lresource with
	    | [] -> (
		match name with
		  | "message" ->
		      route_message from to' packet
		  | "iq" -> (
		      match Xml.get_attr_s "type" attrs with
			| "error"
			| "result" -> ()
			| _ ->
			    let err =
			      Jlib.make_error_reply
				packet Jlib.err_service_unavailable
			    in
			      Router.route to' from err
		    )
		  | _ ->
		      (*?DEBUG("packet droped~n", [])*)
		      ()
	      )
	    | s :: sids ->
		let sid = List.fold_left max s sids in
		let (_, owner) = sid in
		  (*?DEBUG("sending to process ~p~n", [Pid]),*)
		  send_owner owner (`Route (from, to', packet))
	)

let do_route from to' packet =
  let {Jlib.luser = luser;
       Jlib.lserver = lserver;
       Jlib.lresource = lresource; _} = to' in
  let hash = Jamler_cluster.hash_user luser lserver in
  let node = Jamler_cluster.JamlerCluster.get_node_by_hash hash in
  let open Erlang in
    if node <> Erl_epmd.node () then (
      match (lresource :> string) with
	| "" -> (
	    let `XmlElement (name, attrs, _els) = packet in
	      match name with
		| "iq" ->
		    ignore (process_iq from to' packet)
		| _ ->
		    dist_send_by_name "ejabberd_sm" node
		      (ErlTuple [| ErlAtom "route";
				   jid_to_term from;
				   jid_to_term to';
				   ErlType.(to_term xml packet)
				|])
	  )
	| _ -> (
	    match find_sids_by_usr luser lserver lresource with
	      | [] -> (
		  dist_send_by_name "ejabberd_sm" node
		    (ErlTuple [| ErlAtom "route";
				 jid_to_term from;
				 jid_to_term to';
				 ErlType.(to_term xml packet)
			      |])
		)
	      | s :: sids ->
		  let sid = List.fold_left max s sids in
		  let (_, owner) = sid in
		    (*?DEBUG("sending to process ~p~n", [Pid]),*)
		    send_owner owner (`Route (from, to', packet))
	  )
    ) else (
      do_route1 from to' packet
    )


let route from to' packet =
  try
    do_route from to' packet
  with
    | exn ->
	ignore (
	  Lwt_log.error_f
	    ~section
	    ~exn:exn
	    "exception when processing packet\nfrom: %s\nto: %s\npacket: %s\nexception"
            (Jlib.jid_to_string from)
            (Jlib.jid_to_string to')
            (Xml.element_to_string packet)
	)

let broadcast to' data =
  let {Jlib.luser = luser;
       Jlib.lserver = lserver; _} = to' in
    List.iter
      (fun r ->
	 let to' = Jlib.jid_replace_resource' to' r in
	 let packet = `Broadcast data in
	   match find_sids_by_usr luser lserver r with
	    | [] -> ()
	    | s :: sids ->
		let sid = List.fold_left max s sids in
		let (_, owner) = sid in
		  send_owner owner packet
      ) (get_user_resources luser lserver)

let get_sid_by_usr luser lserver lresource =
  match find_sids_by_usr luser lserver lresource with
    | [] -> None
    | s :: sids ->
	Some (List.fold_left max s sids)

let sm_store u s r ts priority pid =
  let open Erlang in
  let sid = (ts, External pid) in
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
    open_session sid user server resource priority info []

let sm_remove u s r ts pid =
  let open Erlang in
  let sid = (ts, External pid) in
  let user = Jlib.nodeprep_exn u in
  let server = Jlib.nameprep_exn s in
  let resource = Jlib.resourceprep_exn r in
  let info = [] in
    close_session sid user server resource info []

let _ =
  Jamler_cluster.sm_store := sm_store;
  Jamler_cluster.sm_remove := sm_remove

module JamlerSM :
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
    register (self :> univ_msg pid) "ejabberd_sm";
    Lwt.return (`Continue {pid = self})

  open Erlang

  let route_from_term =
    function
      | ErlTuple [| ErlAtom "route";
		    ErlTuple [| ErlAtom "jid";
				from_user; from_server; from_resource;
				_; _; _ |];
		    ErlTuple [| ErlAtom "jid";
				to_user; to_server; to_resource;
				_; _; _ |];
		    msg |] -> (
	  let from_user = ErlType.(from_term string from_user) in
	  let from_server = ErlType.(from_term string from_server) in
	  let from_resource = ErlType.(from_term string from_resource) in
	  let to_user = ErlType.(from_term string to_user) in
	  let to_server = ErlType.(from_term string to_server) in
	  let to_resource = ErlType.(from_term string to_resource) in
	  let from = Jlib.make_jid_exn from_user from_server from_resource in
	  let to' = Jlib.make_jid_exn to_user to_server to_resource in
	  let msg = ErlType.(from_term xml msg) in
	    `Route (from, to', msg)
	)
      | _ -> invalid_arg "route_from_term"

  let handle (msg : msg) state =
    match msg with
      | `Erl (ErlTuple [| ErlAtom "route"; _; _; _ |] as term) -> (
	  try
	    let `Route (from, to', msg) = route_from_term term in
	      route from to' msg;
	      Lwt.return (`Continue state)
	  with
	    | exn ->
		lwt () =
		  Lwt_log.error_f
		    ~section
		    ~exn:exn
		    "exception on processing packet\npacket: %s\nexception"
		    (Erlang.term_to_string term)
		in
		  Lwt.return (`Continue state)
	)
      | `Erl (ErlTuple
		[| ErlAtom "send";
		   ErlTuple [| ErlFloat ts;
			       ErlBinary user;
			       ErlBinary server;
			       ErlBinary resource |];
		   msg |] as term) -> (
	  try
	    let user = Jlib.nodeprep_exn user in
	    let server = Jlib.nameprep_exn server in
	    let resource = Jlib.resourceprep_exn resource in
	      (match get_sid_by_usr user server resource with
		 | None ->
		     ()
		 | Some (ts', owner) when ts' <> ts ->
		     ()
		 | Some (_ts, External _) ->
		     ()
		 | Some (_ts, Local pid) -> (
		     match msg with
		       | ErlTuple [| ErlAtom "route"; _; _; _ |] ->
			   let route_msg = route_from_term msg in
			     pid $! route_msg;
		       | _ -> invalid_arg "unknown message"
		   )
	      );
	      Lwt.return (`Continue state)
	  with
	    | exn ->
		lwt () =
		  Lwt_log.error_f
		    ~section
		    ~exn:exn
		    "exception on processing packet\npacket: %s\nexception"
		    (Erlang.term_to_string term)
		in
		  Lwt.return (`Continue state)
	)
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

module JamlerSMServer = GenServer.Make(JamlerSM)

let _ =
  JamlerSMServer.start ();

