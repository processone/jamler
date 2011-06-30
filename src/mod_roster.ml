open Gen_roster

module RosterMemoryStorage : RosterStorage =
struct
  let name = "mod_roster"

  let rosters = (Hashtbl.create 100
		   : (Jlib.nodepreped * Jlib.namepreped,
		      (LJID.t, subscription roster_item) Hashtbl.t) Hashtbl.t)

  let read_roster' u s =
    try
      Some (Hashtbl.find rosters (u, s))
    with
      | Not_found -> None

  let read_roster' u s =
    try
      let roster = Hashtbl.find rosters (u, s) in
	Hashtbl.fold
	  (fun jid item acc -> (jid, item) :: acc) roster []
    with
      | Not_found -> []

  let read_roster u s =
    Lwt.return (read_roster' u s)

  let delete_roster' u s =
    Hashtbl.remove rosters (u, s)

  let delete_roster u s =
    Lwt.return (delete_roster' u s)

  let read_roster_item' u s jid =
    try
      let roster = Hashtbl.find rosters (u, s) in
	Some (Hashtbl.find roster jid)
    with
      | Not_found -> None

  let read_roster_item u s jid =
    Lwt.return (read_roster_item' u s jid)

  let write_roster_item' u s jid item =
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

  let write_roster_item u s jid item =
    Lwt.return (write_roster_item' u s jid item)

  let delete_roster_item' u s jid =
    let us = (u, s) in
      try
	let roster = Hashtbl.find rosters us in
	  Hashtbl.remove roster jid;
	  if Hashtbl.length roster = 0
	  then Hashtbl.remove rosters us
      with
	| Not_found -> ()

  let delete_roster_item u s jid =
    Lwt.return (delete_roster_item' u s jid)

  let item_set_transaction luser lserver jid1 attrs els =
    let jid = (jid1.Jlib.user, jid1.Jlib.server, jid1.Jlib.resource) in
    let ljid = Jlib.jid_tolower jid1 in
    let res = read_roster_item' luser lserver ljid in
    let item' =
      match res with
	| None ->
	    {jid = jid;
	     name = "";
	     subscription = `None `None;
	     groups = [];
	     askmessage = "";
	    }
	| Some item ->
	    {item with
	       jid = jid;
	       name = "";
	       groups = []}
    in
    let item =
      (item' :> [ subscription | `Remove] roster_item)
    in
    let item = process_item_attrs item attrs in
    let item = process_item_els item els in
      (match item with
	 | {subscription = `Remove; _} ->
	     delete_roster_item' luser lserver ljid
	 | {subscription = #subscription; _} as item ->
	     write_roster_item' luser lserver ljid item
      );
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

  let subscription_transaction direction luser lserver jid1 type' reason =
    let ljid = Jlib.jid_tolower jid1 in
    let item =
      match read_roster_item' luser lserver ljid with
	| None ->
	    let jid =
	      (jid1.Jlib.user, jid1.Jlib.server, jid1.Jlib.resource)
	    in
	      {jid;
	       name = "";
	       subscription = `None `None;
	       groups = [];
	       askmessage = "";
	      }
	| Some i -> i
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
	    delete_roster_item' luser lserver ljid;
	    Lwt.return (None, autoreply)
	| Some subscription ->
	    let new_item =
	      {item with
		 subscription = subscription;
		 askmessage = ask_message}
	    in
	      write_roster_item' luser lserver ljid new_item;
	      (*case roster_version_on_db(LServer) of
		true -> mnesia:write(#roster_version{us = {LUser, LServer}, version = sha:sha(term_to_binary(now()))});
		false -> ok
		end,*)
	      Lwt.return (Some new_item, autoreply)

  let _ =
    let test = Jlib.string_to_jid_exn "test@e.localhost" in
    let test10 = Jlib.string_to_jid_exn "test10@e.localhost" in
    let i1 = {jid = ("test10", "e.localhost", "");
	      name = "test10__";
	      subscription = `Both;
	      groups = ["asd"; "qwe"];
	      askmessage = "";
	     }
    in
    let i2 = {jid = ("test", "e.localhost", "");
	      name = "test";
	      subscription = `Both;
	      groups = ["test"];
	      askmessage = "";
	     }
    in
    lwt () =
      write_roster_item test.Jlib.luser test.Jlib.lserver
	(test10.Jlib.luser, test10.Jlib.lserver, test10.Jlib.lresource)
	i1
    in
    lwt () =
      write_roster_item test10.Jlib.luser test10.Jlib.lserver
	(test.Jlib.luser, test.Jlib.lserver, test.Jlib.lresource)
	i2
    in
      Lwt.return ()

end

module ModRoster = Gen_roster.Make(RosterMemoryStorage)

let () = Gen_mod.register_mod (module ModRoster : Gen_mod.Module)
