module GenIQHandler = Jamler_gen_iq_handler
module Hooks = Jamler_hooks
module Router = Jamler_router
module Translate = Jamler_translate
module Config = Jamler_config
module Auth = Jamler_auth

open Mod_disco

module ModVCardSQL :
sig
  include Gen_mod.Module

end
  =
struct
  let name = "mod_vcard_sql"

  let get_sm_features acc (_from, _to, node, _lang) =
    match acc with
    | Features features when node = "" ->
       (Hooks.OK, Features ([%xmlns "VCARD"] :: features))
    | FEmpty when node = "" ->
       (Hooks.OK, Features [ [%xmlns "VCARD"]])
    | _ ->
       (Hooks.OK, acc)

  let remove_user (luser, lserver) =
    let username = (luser : Jlib.nodepreped :> string) in
    Sql.transaction lserver
      (fun () ->
	let delete_vcard =
	  [%sql {|delete from vcard where username=%(username)s|}]
	in
	let delete_vcard_search =
	  [%sql {|delete from vcard_search where lusername=%(username)s|}]
	in
	let _ = Sql.query_t delete_vcard in
	let _ = Sql.query_t delete_vcard_search in
	());
    Hooks.OK

  let process_local_iq _from _to iq =
    match iq.Jlib.iq_type with
    | `Set subel ->
       `IQ {iq with
           Jlib.iq_type =
             `Error (Jlib.err_not_allowed, Some subel)}
    | `Get _ ->
       let desc = (Translate.translate iq.Jlib.iq_lang Cfg.synopsis)
	          ^ "\n" ^ Cfg.copyright in
       `IQ {iq with
           Jlib.iq_type =
             `Result
               (Some (`XmlElement
                        ("vCard",
			 [("xmlns", [%xmlns "VCARD"])],
			 [`XmlElement ("FN", [],
				       [`XmlCdata Cfg.name]);
			  `XmlElement ("URL", [],
				       [`XmlCdata Cfg.uri]);
			  `XmlElement ("DESC", [],
				       [`XmlCdata desc]);
			  `XmlElement ("BDAY", [],
				       [`XmlCdata Cfg.bday])])))}

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
      let vcard_insert = [%sql {|
	                        insert into vcard(username, vcard)
	                        values (%(lusername)s, %(svcard)s)
	                        |}] in
      let vcard_update = [%sql {|
	                        update vcard set vcard = %(svcard)s
	                        where username = %(lusername)s
	                        |}] in
      let vcard_search_insert = [%sql {|
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
	  |}] in
      let vcard_search_update = [%sql {|
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
	  |}] in
      try
	Sql.transaction lserver
	  (fun () ->
	    Sql.update_t vcard_insert vcard_update;
	    Sql.update_t vcard_search_insert vcard_search_update)
      with
      | _ ->
	 ()
    with
    | _ ->
       ()

  let get_vcard luser lserver =
    let username = (luser : Jlib.nodepreped :> string) in
    let get_vcard =
      [%sql {|select @(vcard)s from vcard where username=%(username)s|}]
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
	 set_vcard user luser lserver subel;
	 `IQ {iq with Jlib.iq_type = `Result None}
       ) else (
         `IQ {iq with
             Jlib.iq_type =
               `Error (Jlib.err_not_allowed, Some subel)})
    | `Get subel ->
       let {Jlib.luser = luser; Jlib.lserver = lserver; _} = to' in
       try (
	 match get_vcard luser lserver with
	 | svcard :: _ -> (
	   try
	     let vcard = Xml.parse_element svcard in
	     `IQ {iq with Jlib.iq_type = `Result (Some vcard)}
	   with
	   | _ ->
	      `IQ {iq with
		  Jlib.iq_type =
		    `Error (Jlib.err_service_unavailable,
			    Some subel)})
	 | _ ->
	    `IQ {iq with Jlib.iq_type = `Result None})
       with
       | _ ->
          `IQ {iq with
              Jlib.iq_type =
		`Error (Jlib.err_internal_server_error, Some subel)}

  let start host =
    (* TODO: make search *)
    (* MyHost = gen_mod:get_opt_host(Host, Opts, "vjud.@HOST@"),
    Search = gen_mod:get_opt(search, Opts, true),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, init, [MyHost, Host, Search])). *)
    Mod_disco.register_feature host [%xmlns "VCARD"];
    [Gen_mod.hook Auth.remove_user host remove_user 50;
     Gen_mod.fold_hook disco_sm_features host get_sm_features 50;
     Gen_mod.iq_handler `Local host [%xmlns "VCARD"] process_local_iq ();
     Gen_mod.iq_handler `SM host [%xmlns "VCARD"] process_sm_iq ();
    ]

  let stop host =
    Mod_disco.unregister_feature host [%xmlns "VCARD"];
    ()

end

let () = Gen_mod.register_mod (module ModVCardSQL : Gen_mod.Module)
