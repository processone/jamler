open Process

let section = Jamler_log.new_section "sm"

module LJID = Jlib.LJID
module Hooks = Jamler_hooks
module Auth = Jamler_auth
module Router = Jamler_router
module GenIQHandler = Jamler_gen_iq_handler

type msg = Router.msg
type info = [ `TODO ] list

module Session :
sig
  type sid = float * msg pid

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
  type sid = float * msg pid

  module HashedSID =
  struct
    type t = sid
    let equal (t1, p1) (t2, p2) =
      let p1 = pid_to_proc p1
      and p2 = pid_to_proc p2 in
        t1 = t2 && p1.id = p2.id
    let hash (t, p) =
      let p = pid_to_proc p in
        Hashtbl.hash t lxor Hashtbl.hash p.id
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


let set_session sid user server resource priority info =
  let usr = (user, server, resource) in
    add sid {usr; priority; info}

let check_existing_resources user server resource =
  (* A connection exist with the same resource. We replace it: *)
  let sids = find_sids_by_usr user server resource in
    match sids with
      | [] -> ()
      | s :: sids' ->
          let max_sid = List.fold_left max s sids' in
            List.iter
      	(fun ((_, pid) as s) ->
      	   if s != max_sid then (
      	     (* TODO *)
      	     ()
      	     (* Pid ! replaced; *)
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

let open_session sid user server resource priority info =
  set_session sid user server resource priority info;
  check_for_sessions_to_replace user server resource
  (*JID = jlib:make_jid(User, Server, Resource),
  ejabberd_hooks:run(sm_register_connection_hook, JID#jid.lserver,
      	       [SID, JID, Info]).*)

let do_close_session sid =
  remove sid
  (*
  Info = case mnesia:dirty_read({session, SID}) of
             [] -> [];
             [#session{info=I}] -> I
         end,
  drop_session(SID),
  Info.*)

let close_session sid _user _server _resource =
  do_close_session sid
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

let close_session_unset_presence sid user server resource _status =
  close_session sid user server resource
  (*ejabberd_hooks:run(unset_presence_hook, jlib:nameprep(Server),
      	       [User, Server, Resource, Status]).*)

let set_presence sid user server resource priority _presence info =
  set_session sid user server resource priority info
  (*ejabberd_hooks:run(set_presence_hook, jlib:nameprep(Server),
      	       [User, Server, Resource, Presence]).*)

let unset_presence sid user server resource status info =
  set_session sid user server resource (-1) info
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
    List.map
      (fun (sid, {priority; usr = (_, _, r); _}) -> (priority, r, sid))
      sessions

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
      		 let (_, pid) = sid in
      		   (*?DEBUG("sending to process ~p~n", [Pid]),*)
      		   pid $! `Route (from, to', packet)
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

let rec do_route from to' packet =
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
			       do_route
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
		     do_route from (Jlib.jid_replace_resource' to' r) packet
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
		let (_, pid) = sid in
		  (*?DEBUG("sending to process ~p~n", [Pid]),*)
		  pid $! `Route (from, to', packet)
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

