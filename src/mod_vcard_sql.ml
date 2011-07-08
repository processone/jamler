module GenIQHandler = Jamler_gen_iq_handler
module Hooks = Jamler_hooks
module Router = Jamler_router
module Translate = Jamler_translate
module Config = Jamler_config

module ModVCardSQL :
sig
  include Gen_mod.Module
  type hook_acc

end
  =
struct
  type hook_acc = | Result of (string list)
                  | Error of Xml.element
                  | Empty

  let name = "mod_vcard_sql"
  let remove_user = Hooks.create ()
  let disco_sm_features = Hooks.create_fold ()

  let get_sm_features acc (_from, _to, node, _lang) =
    match acc with
      | Result features when node = "" ->
	  Lwt.return (Hooks.OK, Result (<:ns<VCARD>> :: features))
      | Empty when node = "" ->
	  Lwt.return (Hooks.OK, Result [ <:ns<VCARD>>])
      | _ ->
	  Lwt.return (Hooks.OK, acc)

  let remove_user_h (luser, lserver) =
    let username = (luser : Jlib.nodepreped :> string) in
    lwt _ = Sql.transaction lserver
      (fun () ->
	 let delete_vcard =
	   <:sql<delete from vcard where username=%(username)s>>
	 in
	 let delete_vcard_search =
	   <:sql<delete from vcard_search where lusername=%(username)s>>
	 in
	 lwt _ = Sql.query_t delete_vcard in
	 lwt _ = Sql.query_t delete_vcard_search in
	   Lwt.return ()) in
      Lwt.return (Hooks.OK)

  let process_local_iq _from _to iq =
    match iq.Jlib.iq_type with
      | `Set subel ->
	  Lwt.return (`IQ {iq with
                             Jlib.iq_type =
                          `Error (Jlib.err_not_allowed, Some subel)})
      | `Get _ ->
	  let desc = (Translate.translate iq.Jlib.iq_lang Cfg.synopsis)
	    ^ "\n" ^ Cfg.copyright in
	  Lwt.return
	    (`IQ {iq with
                    Jlib.iq_type =
                 `Result
                   (Some (`XmlElement
                            ("vCard",
			     [("xmlns", <:ns<VCARD>>)],
			     [`XmlElement ("FN", [],
					   [`XmlCdata Cfg.name]);
			      `XmlElement ("URL", [],
					   [`XmlCdata Cfg.uri]);
			      `XmlElement ("DESC", [],
					   [`XmlCdata desc]);
			      `XmlElement ("BDAY", [],
					   [`XmlCdata Cfg.bday])])))})

  let set_vcard user luser lserver vcard =
    let fn       = Xml.get_path_s vcard [`Elem "FN";                   `Cdata] in
    let family   = Xml.get_path_s vcard [`Elem "N"; `Elem "FAMILY";    `Cdata] in
    let given    = Xml.get_path_s vcard [`Elem "N"; `Elem "GIVEN";     `Cdata] in
    let middle   = Xml.get_path_s vcard [`Elem "N"; `Elem "MIDDLE";    `Cdata] in
    let nickname = Xml.get_path_s vcard [`Elem "NICKNAME";             `Cdata] in
    let bday     = Xml.get_path_s vcard [`Elem "BDAY";                 `Cdata] in
    let ctry     = Xml.get_path_s vcard [`Elem "ADR"; `Elem "CTRY";    `Cdata] in
    let locality = Xml.get_path_s vcard [`Elem "ADR"; `Elem "LOCALITY";`Cdata] in
    let email1   = Xml.get_path_s vcard [`Elem "EMAIL"; `Elem "USERID";`Cdata] in
    let email2   = Xml.get_path_s vcard [`Elem "EMAIL";                `Cdata] in
    let orgname  = Xml.get_path_s vcard [`Elem "ORG"; `Elem "ORGNAME"; `Cdata] in
    let orgunit  = Xml.get_path_s vcard [`Elem "ORG"; `Elem "ORGUNIT"; `Cdata] in
    let email = match email1 with
      | "" -> email2
      | _ -> email1
    in try
	let lfn       = Stringprep.tolower fn in
	let lfamily   = Stringprep.tolower family in
	let lgiven    = Stringprep.tolower given in
	let lmiddle   = Stringprep.tolower middle in
	let lnickname = Stringprep.tolower nickname in
	let lbday     = Stringprep.tolower bday in
	let lctry     = Stringprep.tolower ctry in
	let llocality = Stringprep.tolower locality in
	let lemail    = Stringprep.tolower email in
	let lorgname  = Stringprep.tolower orgname in
	let lorgunit  = Stringprep.tolower orgunit in

	let username = user in
	let lusername = (luser : Jlib.nodepreped :> string) in
	let svcard = Xml.element_to_string vcard in
	let vcard_insert = <:sql<
	  insert into vcard(username, vcard)
	  values (%(lusername)s, %(svcard)s)
	  >> in
	let vcard_update = <:sql<
	  update vcard set vcard = %(svcard)s
	  where username = %(lusername)s
	  >> in
	let vcard_search_insert = <:sql<
	  insert into vcard_search(
	    username, lusername, fn, lfn, family,
            lfamily, given, lgiven, middle, lmiddle,
            nickname, lnickname, bday, lbday, ctry,
            lctry, locality, llocality, email, lemail,
            orgname, lorgname, orgunit, lorgunit)
	  values (
              %(username)s, %(lusername)s, %(fn)s, %(lfn)s, %(family)s,
              %(lfamily)s, %(given)s, %(lgiven)s, %(middle)s, %(lmiddle)s,
              %(nickname)s, %(lnickname)s, %(bday)s, %(lbday)s, %(ctry)s,
              %(lctry)s, %(locality)s, %(llocality)s, %(email)s, %(lemail)s,
              %(orgname)s, %(lorgname)s, %(orgunit)s, %(lorgunit)s)
	  >> in
	let vcard_search_update = <:sql<
	  update vcard_search
	  set
	    username = %(username)s,
	    fn = %(fn)s,
	    lfn = %(lfn)s,
	    family = %(family)s,
	    lfamily = %(lfamily)s,
	    given = %(given)s,
	    lgiven = %(lgiven)s,
	    middle = %(middle)s,
	    lmiddle = %(lmiddle)s,
	    nickname = %(nickname)s,
	    lnickname = %(lnickname)s,
	    bday = %(bday)s,
	    lbday = %(lbday)s,
	    ctry = %(ctry)s,
	    lctry = %(lctry)s,
	    locality = %(locality)s,
	    llocality = %(llocality)s,
	    email = %(email)s,
	    lemail = %(lemail)s,
	    orgname = %(orgname)s,
	    lorgname = %(lorgname)s,
	    orgunit = %(orgunit)s,
	    lorgunit = %(lorgunit)s
	  where lusername = %(lusername)s
	  >> in
	  try_lwt
	    Sql.transaction lserver
	      (fun () ->
		 lwt _ = Sql.update_t vcard_insert vcard_update in
		   Sql.update_t vcard_search_insert vcard_search_update)
	  with
	    | _ ->
		Lwt.return ()
      with
	| _ ->
	    Lwt.return ()

  let get_vcard luser lserver =
    let username = (luser : Jlib.nodepreped :> string) in
    let get_vcard =
      <:sql<select @(vcard)s from vcard where username=%(username)s>>
    in
      Sql.query lserver get_vcard

  let process_sm_iq from to' iq =
    match iq.Jlib.iq_type with
      | `Set subel ->
	  let {Jlib.user = user;
	       Jlib.luser = luser;
	       Jlib.lserver = lserver; _} = from in
	    if Config.is_my_host lserver then (
	      (* TODO: check set_vcard's result and report error if needed *)
	      lwt _ = set_vcard user luser lserver subel in
		Lwt.return (`IQ {iq with Jlib.iq_type = `Result None})
	    ) else (
	      Lwt.return
		(`IQ {iq with
                        Jlib.iq_type =
                     `Error (Jlib.err_not_allowed, Some subel)}))
      | `Get subel ->
	  let {Jlib.luser = luser; Jlib.lserver = lserver; _} = to' in
	    try_lwt (
	      match_lwt get_vcard luser lserver with
		| svcard :: _ -> (
		    try
		      let vcard = Xml.parse_element svcard in
			Lwt.return
			  (`IQ {iq with Jlib.iq_type = `Result (Some vcard)})
		    with
		      | _ ->
			  Lwt.return
			    (`IQ {iq with
				    Jlib.iq_type =
				 `Error (Jlib.err_service_unavailable,
					 Some subel)}))
		| _ ->
		    Lwt.return (`IQ {iq with Jlib.iq_type = `Result None}))
	    with
	      | _ ->
		  Lwt.return
		    (`IQ {iq with
                            Jlib.iq_type =
			 `Error (Jlib.err_internal_server_error, Some subel)})

  let start host =
    (* TODO: make search *)
    (* MyHost = gen_mod:get_opt_host(Host, Opts, "vjud.@HOST@"),
    Search = gen_mod:get_opt(search, Opts, true),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, init, [MyHost, Host, Search])). *)
    Lwt.return (
      [Gen_mod.hook remove_user host remove_user_h 50;
       Gen_mod.fold_hook disco_sm_features host get_sm_features 50;
       Gen_mod.iq_handler `Local host <:ns<VCARD>> process_local_iq ();
       Gen_mod.iq_handler `SM host <:ns<VCARD>> process_sm_iq ();
      ]
    )

  let stop _host =
    Lwt.return ()

end

let () = Gen_mod.register_mod (module ModVCardSQL : Gen_mod.Module)
