open Gen_roster

module RosterSQLStorage : RosterStorage =
struct
  let name = "mod_roster_sql"

  let rosters = (Hashtbl.create 100
		   : (Jlib.nodepreped * Jlib.namepreped,
		      (LJID.t, subscription roster_item) Hashtbl.t) Hashtbl.t)

  let raw_to_record (sjid, nick, ssubscription, sask, saskmessage) =
    let jid = Jlib.string_to_jid_exn sjid in
    let ljid = Jlib.jid_tolower jid in
    let subscription =
      match ssubscription, sask with
	| "B", _ -> `Both
	| "T", "I" -> `To `In
	| "T", _ -> `To `None
	| "F", "O" -> `From `Out
	| "F", _ -> `From `None
	| _, "B" -> `None `Both
	| _, "I" -> `None `In
	| _, "O" -> `None `Out
	| _, _ -> `None `None
    in
      {jid = (ljid :> string * string * string);
       name = nick;
       subscription;
       askmessage = saskmessage;
       groups = []}


  let get_roster_query server username =
    let username = (username : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	select @(jid)s, @(nick)s, @(subscription)s, @(ask)s, @(askmessage)s
        from rosterusers
        where username=%(username)s
      |}]
    in
      Sql.query server query

  let get_roster_jid_groups_query server username =
    let username = (username : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	select @(jid)s, @(grp)s from rostergroups
	where username=%(username)s
      |}]
    in
      Sql.query server query

  let get_roster_groups_query_t username ljid =
    let username = (username : Jlib.nodepreped :> string) in
    let sjid = Jlib.LJID.to_string ljid in
    let query =
      [%sql {|
	select @(grp)s from rostergroups
	where username=%(username)s and jid=%(sjid)s
      |}]
    in
      Sql.query_t query

  let read_roster user server =
    try%lwt
      let%lwt items = get_roster_query server user in
      let%lwt jid_groups = get_roster_jid_groups_query server user in
      let groups_ht = Hashtbl.create (List.length jid_groups) in
      let _ =
	List.iter
	  (fun (j, g) ->
	     Hashtbl.add groups_ht j g
	  ) jid_groups
      in
      let ritems =
	List.map
	  (fun item ->
	     let r = raw_to_record item in
	     let (u, s, res) = r.jid in
	     let jid = Jlib.make_jid_exn u s res in
	     let ljid = Jlib.jid_tolower jid in
	     let sjid = Jlib.jid_to_string' u s res in
	     let groups = Hashtbl.find_all groups_ht sjid in
	       (ljid, {r with groups})
	  ) items
      in
	Lwt.return ritems
    with
      | _ -> Lwt.return []

  let delete_roster' u s =
    Hashtbl.remove rosters (u, s)

  let _delete_roster u s =
    Lwt.return (delete_roster' u s)

  let read_roster_item_t username ljid =
    let username = (username : Jlib.nodepreped :> string) in
    let sjid = Jlib.LJID.to_string ljid in
    let query =
      [%sql {|
	select @(jid)s, @(nick)s, @(subscription)s, @(ask)s, @(askmessage)s
        from rosterusers
        where username=%(username)s and jid=%(sjid)s
        for update
      |}]
    in
      Sql.query_t query

  let _write_roster_item' u s jid item =
    let us = (u, s) in
    let roster =
      try
	Hashtbl.find rosters us
      with
	| Not_found ->
	    let roster = Hashtbl.create 1 in
	      Hashtbl.add rosters us roster;
	      roster
    in
      Hashtbl.replace roster jid item



  let write_roster_item_t username ljid item =
    let username = (username : Jlib.nodepreped :> string) in
    let sjid = Jlib.LJID.to_string ljid in
    let name = item.name in
    let ssubscription =
      match item.subscription with
	| `None _ -> "N"
	| `From _ -> "F"
	| `To _ -> "T"
	| `Both -> "B"
    in
    let sask =
      let pending =
	match item.subscription with
	  | `None pending -> (pending :> [ `Both | `In | `None | `Out ])
	  | `From pending -> (pending :> [ `Both | `In | `None | `Out ])
	  | `To pending -> (pending :> [ `Both | `In | `None | `Out ])
	  | `Both
	  (*| `Remove*) -> `None
      in
	match pending with
	  | `None -> "N"
	  | `In -> "I"
	  | `Out -> "O"
	  | `Both -> "B"
    in
    let askmessage = item.askmessage in
    let insert_user =
      [%sql {|
	insert into rosterusers(username, jid, nick, subscription, ask,
				askmessage)
	values (%(username)s, %(sjid)s, %(name)s, %(ssubscription)s, %(sask)s,
                %(askmessage)s)
      |}]
    in
    let update_user =
      [%sql {|
	update rosterusers
	set nick = %(name)s,
            subscription = %(ssubscription)s,
            ask = %(sask)s,
            askmessage = %(askmessage)s
	where username = %(username)s and jid = %(sjid)s
      |}]
    in
    let%lwt () = Sql.update_t insert_user update_user in
    let delete_groups =
      [%sql {|
	delete from rostergroups
        where username=%(username)s and jid=%(sjid)s
      |}]
    in
    let%lwt _ = Sql.query_t delete_groups in
      Lwt_list.iter_s
	(fun group ->
	   let insert_group =
	     [%sql {|
	       insert into rostergroups(username, jid, grp)
	       values (%(username)s, %(sjid)s, %(group)s)
	     |}]
	   in
	   let%lwt _ = Sql.query_t insert_group in
	     Lwt.return ()
	) item.groups

  let delete_roster_item_t username ljid =
    let username = (username : Jlib.nodepreped :> string) in
    let sjid = Jlib.LJID.to_string ljid in
    let query1 =
      [%sql {|
	delete from rosterusers
        where username=%(username)s and jid=%(sjid)s
      |}]
    in
    let query2 =
      [%sql {|
	delete from rostergroups
        where username=%(username)s and jid=%(sjid)s
      |}]
    in
    let%lwt _ = Sql.query_t query1 in
    let%lwt _ = Sql.query_t query2 in
      Lwt.return ()

  let item_set_transaction' luser _lserver jid1 attrs els =
    let jid = (jid1.Jlib.user, jid1.Jlib.server, jid1.Jlib.resource) in
    let ljid = Jlib.jid_tolower jid1 in
    let%lwt res = read_roster_item_t luser ljid in
    let item' =
      match res with
	| [] ->
	    {jid = jid;
	     name = "";
	     subscription = `None `None;
	     groups = [];
	     askmessage = "";
	    }
	| [item] ->
	    let r = raw_to_record item in
	      {r with
		 jid = jid;
		 name = "";
		 groups = []}
	| _ -> assert false
    in
    let item =
      (item' :> [ subscription | `Remove] roster_item)
    in
    let item = process_item_attrs item attrs in
    let item = process_item_els item els in
    let%lwt () =
      match item with
	| {subscription = `Remove; _} ->
	    delete_roster_item_t luser ljid
	| {subscription = #subscription; _} as item ->
	    write_roster_item_t luser ljid item
    in
      (*
	%% If the item exist in shared roster, take the
	%% subscription information from there:
	Item3 = ejabberd_hooks:run_fold(roster_process_item,
	LServer, Item2, [LServer]),
	case roster_version_on_db(LServer) of
	true -> mnesia:write(#roster_version{us = {LUser, LServer}, version = sha:sha(term_to_binary(now()))});
	false -> ok
	end,
      *)
      Lwt.return (item', item, ljid)

  let item_set_transaction luser lserver jid1 attrs els =
    Sql.transaction lserver
      (fun () -> item_set_transaction' luser lserver jid1 attrs els)

  let subscription_transaction' direction luser _lserver jid1 type' reason =
    let ljid = Jlib.jid_tolower jid1 in
    let%lwt item =
      match%lwt read_roster_item_t luser ljid with
	| [] ->
	    let jid =
	      (jid1.Jlib.user, jid1.Jlib.server, jid1.Jlib.resource)
	    in
	      Lwt.return
		{jid;
		 name = "";
		 subscription = `None `None;
		 groups = [];
		 askmessage = "";
		}
	| [item] ->
	    let r = raw_to_record item in
	    let%lwt groups = get_roster_groups_query_t luser ljid in
	      Lwt.return {r with groups}
	| _ -> assert false
    in
    let new_state =
      match direction with
	| `Out -> Gen_roster.out_state_change item.subscription type'
	| `In -> Gen_roster.in_state_change item.subscription type'
    in
    let autoreply =
      match direction with
	| `Out -> None
	| `In -> Gen_roster.in_auto_reply item.subscription type'
    in
    let ask_message =
      match new_state with
	| Some (`None `Both)
	| Some (`None `In)
	| Some (`To `In) -> reason
	| _ -> ""
    in
      match new_state with
	| None ->
	    Lwt.return (None, autoreply)
	| Some (`None `None) when item.subscription = `None `In ->
	    let%lwt () = delete_roster_item_t luser ljid in
	      Lwt.return (None, autoreply)
	| Some subscription ->
	    let new_item =
	      {item with
		 subscription = subscription;
		 askmessage = ask_message}
	    in
	    let%lwt () = write_roster_item_t luser ljid new_item in
	      (*case roster_version_on_db(LServer) of
		true -> mnesia:write(#roster_version{us = {LUser, LServer}, version = sha:sha(term_to_binary(now()))});
		false -> ok
		end,*)
	      Lwt.return (Some new_item, autoreply)

  let subscription_transaction direction luser lserver jid1 type' reason =
    Sql.transaction lserver
      (fun () ->
	 subscription_transaction' direction luser lserver jid1 type' reason
      )

end

module ModRosterSql = Gen_roster.Make(RosterSQLStorage)

let () = Gen_mod.register_mod (module ModRosterSql : Gen_mod.Module)
