module GenIQHandler = Jamler_gen_iq_handler
module SM = Jamler_sm
module Hooks = Jamler_hooks

module ModLastSQL :
sig
  include Gen_mod.Module

  val get_last_info : Jlib.nodepreped -> Jlib.namepreped -> (int * string) Lwt.t
  val roster_get_jid_info :
    (Jlib.nodepreped * Jlib.namepreped * Jlib.jid,
     Gen_roster.subscription * string list)
    Hooks.fold_hook
end
  =
struct
  let name = "mod_last_sql"
  let section = Jamler_log.new_section name
  let roster_get_jid_info = Hooks.create_fold ()
  let remove_user = Hooks.create ()
  let unset_presence_hook = Hooks.create ()

  let get_last luser lserver = 
    let username = (luser : Jlib.nodepreped :> string) in
    let query =
      <:sql<
	select @(seconds)d, @(state)s
	  from last
	  where username=%(username)s
      >>
    in
      Sql.query lserver query

  (* @spec (LUser::string(), LServer::string()) ->
     {ok, TimeStamp::integer(), Status::string()} | not_found
   get_last_info(LUser, LServer) ->
    case get_last(LUser, LServer) of
	{error, _Reason} ->
	    not_found;
	Res ->
	    Res
    end. *)

  let get_last_info luser lserver =
    try_lwt
      lwt res = get_last luser lserver in
	match res with
	  | [(ts, status)] -> Lwt.return (ts, status)
	  | _ -> Lwt.fail Not_found
    with
      | _ -> Lwt.fail Not_found

  let get_last_iq iq subel luser lserver =
    match SM.get_user_resources luser lserver with
      | [] -> (
	  try_lwt
	    lwt last = get_last luser lserver in (
		match last with
		  | [(ts, status)] ->
		      let ts' = (int_of_float (Unix.time ())) - ts in
			Lwt.return
			  (`IQ {iq with
				  Jlib.iq_type =
			       `Result
				 (Some (`XmlElement
					  ("query",
					   [("xmlns", <:ns<LAST>>);
					    ("seconds", string_of_int ts')],
					   [`XmlCdata status])))})
		  | _ ->
		      Lwt.return
			(`IQ {iq with
				Jlib.iq_type =
                             `Error (Jlib.err_service_unavailable, Some subel)})
	      )
	  with
	    | _ ->
		Lwt.return
		  (`IQ {iq with
			  Jlib.iq_type =
                       `Error (Jlib.err_internal_server_error, Some subel)})
	)
      | _ ->
	  Lwt.return
	    (`IQ {iq with
		    Jlib.iq_type =
		 `Result
		   (Some (`XmlElement
			    ("query",
			     [("xmlns", <:ns<LAST>>);
			      ("seconds", "0")],
			     [])))})

  let process_local_iq _from _to = function
    | {Jlib.iq_type = `Get _subel; iq_xmlns = <:ns<LAST>>; _} as iq ->
	let secs = (int_of_float (Jlib.uptime ())) in
	  Lwt.return
	    (`IQ {iq with
		    Jlib.iq_type =
		 `Result
		   (Some (`XmlElement
			    ("query",
			     [("xmlns", <:ns<LAST>>);
			      ("seconds", string_of_int secs)],
			     [])))})
    | {Jlib.iq_type = `Get subel; _} as iq ->
        Lwt.return (`IQ {iq with
                           Jlib.iq_type =
                        `Error (Jlib.err_service_unavailable, Some subel)})
    | {Jlib.iq_type = `Set subel; _} as iq ->
        Lwt.return (`IQ {iq with
                           Jlib.iq_type =
                        `Error (Jlib.err_not_allowed, Some subel)})

  let process_sm_iq from to' = function
    | {Jlib.iq_type = `Set subel; _} as iq ->
	Lwt.return (`IQ {iq with
                           Jlib.iq_type =
                        `Error (Jlib.err_not_allowed, Some subel)})
    | {Jlib.iq_type = `Get subel; iq_xmlns = <:ns<LAST>>; _} as iq ->
	let user = to'.Jlib.luser in
	let server = to'.Jlib.lserver in
	lwt (subscription, _groups) =
	  Hooks.run_fold roster_get_jid_info server
	    (`None `None, []) (user, server, from) in (
	    match subscription with
	      | `Both | `From _ -> (
		  (* TODO: privacy required
		    UserListRecord = ejabberd_hooks:run_fold(
				       privacy_get_user_list, Server,
				       #userlist{},
				       [User, Server]),
		    case ejabberd_hooks:run_fold(
			   privacy_check_packet, Server,
			                    allow,
			   [User, Server, UserListRecord,
			    {To, From,
			     {xmlelement, "presence", [], []}},
		   out]) of *)
		  match true with
		    | true ->
			get_last_iq iq subel user server
		    | false ->
			Lwt.return (`IQ {iq with
					   Jlib.iq_type =
					`Error (Jlib.err_forbidden, Some subel)}
				   ))
	      | _ ->
		  Lwt.return (`IQ {iq with
				     Jlib.iq_type =
				  `Error (Jlib.err_forbidden, Some subel)}))
    | {Jlib.iq_type = `Get subel; _} as iq ->
        Lwt.return (`IQ {iq with
                           Jlib.iq_type =
                        `Error (Jlib.err_service_unavailable, Some subel)})

  let remove_user_h (luser, lserver) =
    let username = (luser : Jlib.nodepreped :> string) in
    let delete_last =
      <:sql<
	delete from last
	where username=%(username)s
      >>
    in
    lwt _ = Sql.query lserver delete_last in
      Lwt.return (Hooks.OK)

  let store_last_info _luser _lserver _timestamp _status =
    (* odbc_queries:set_last_t(LServer, Username, Seconds, State). *)
    Lwt.return (Hooks.OK)

  let on_presence_update_h (luser, lserver, _resrouce, status) =
    let timestamp = int_of_float (Unix.time ()) in
      store_last_info luser lserver timestamp status

  let start host =
    Hooks.add remove_user host remove_user_h 50;
    Hooks.add unset_presence_hook host on_presence_update_h 50;
    Lwt.return (
      [Gen_mod.iq_handler `Local host <:ns<LAST>> process_local_iq ();
       Gen_mod.iq_handler `SM host <:ns<LAST>> process_sm_iq ();
      ]
    )

  let stop _host =
    (* ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host,
			  ?MODULE, on_presence_update, 50), *)
    Lwt.return ()

end

let () = Gen_mod.register_mod (module ModLastSQL : Gen_mod.Module)
