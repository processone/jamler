module GenIQHandler = Jamler_gen_iq_handler
module Hooks = Jamler_hooks
module Auth = Jamler_auth
module Router = Jamler_router
module Translate = Jamler_translate
module Config = Jamler_config
module Captcha = Jamler_captcha

type source = [ `JID of Jlib.LJID.t | `IP of Unix.inet_addr ]

module ModRegister :
sig
  include Gen_mod.Module
end
  =
struct
  let name = "mod_register"
  let section = Jamler_log.new_section name

  let is_captcha_enabled =
    Config.(get_module_opt_with_default
	      name ["captcha_protected"] bool false)

  let password_strength =
    Config.(get_module_opt_with_default
	      name ["password_strength"] int 0)

  let ip_access =
    Config.(get_module_opt_with_default
	      name ["ip_access"] (list ip) [])

  let registration_watchers =
    Config.(get_module_opt_with_default
	      name ["registration_watchers"] (list jid) [])

  let welcome_message_subject =
    Config.(get_module_opt_with_default
	      name ["welcome_message"; "subject"] string "")

  let welcome_message_body = 
    Config.(get_module_opt_with_default
	      name ["welcom_message"; "body"] string "")

  let check_ip_access _ _ =
    (* TODO *)
    true

  let is_strong_password lserver password =
    let entropy = float_of_int (password_strength lserver) in
      if entropy > 0.0 then ((Auth.entropy password) >= entropy)
      else true

  let write_time tm =
    Printf.sprintf
      "%04d-%02d-%02d %02d:%02d:%02d"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
      tm.Unix.tm_mday tm.Unix.tm_hour
      tm.Unix.tm_min tm.Unix.tm_sec

  let get_time_string () =
    write_time (Unix.localtime (Unix.time ()))

  let process_xdata_submit el =
    match Xml.get_subtag el "x" with
      | None ->
	  None
      | Some xdata ->
	  let fields = Jlib.parse_xdata_submit xdata in
	    try (
	      match (List.assoc "username" fields,
		     List.assoc "password" fields) with
		| user :: _, pass :: _ -> (
		    match Jlib.nodeprep user with
		      | Some luser ->
			  Some (luser, pass)
		      | None ->
			  None
		  )
		| _ ->
		    None
	    ) with
	      | Not_found ->
		  None

  let source_to_string = function
    | `IP ip ->
	Unix.string_of_inet_addr ip
    | `JID ljid ->
	Jlib.LJID.to_string ljid

  let check_from jid _server =
    match jid with
      | {Jlib.user = ""; Jlib.server = ""; _} ->
	  true
      | _ ->
	  (* TODO
	     Access = gen_mod:get_module_opt(Server, ?MODULE, access_from, none),
	     acl:match_rule(Server, Access, JID) *)
	  false

  let check_timeout _source =
    (* TODO *)
    true

  let remove_timeout _source =
    (* TODO *)
    ()

  let may_remove_resource = function
    | `JID (u, s, _r) ->
	`JID (u, s, Jlib.resourceprep_exn "")
    | `IP ip ->
	`IP ip

  let send_welcome_message jid =
    let host = jid.Jlib.lserver in
      match (welcome_message_subject host, welcome_message_body host) with
	| "", "" ->
	    ()
	| subj, body ->
	    Router.route
	      (Jlib.make_jid_exn "" (host:>string) "")
	      jid
	      (`XmlElement
		 ("message", [("type", "normal")], 
		  [`XmlElement ("subject", [], [`XmlCdata subj]);
		   `XmlElement ("body", [], [`XmlCdata body])]))

  let send_registration_notifications ujid source =
    let host = ujid.Jlib.lserver in
      match registration_watchers host with
	| [] ->
	    ()
	| jids ->
	    let body = Printf.sprintf
	      ("[%s] The account %s was registered from IP address %s " ^^
		 "on node ~w using %s.")
	      (get_time_string ()) (Jlib.jid_to_string ujid)
	      (source_to_string source) (* node() *) name in
	      List.iter
		(fun jid ->
		   Router.route
		     (Jlib.make_jid_exn "" (host:>string) "")
		     jid
		     (`XmlElement ("message", [("type", "chat")],
				   [`XmlElement ("body", [],
						 [`XmlCdata body])])))
		jids

  let try_register luser (lserver:Jlib.namepreped)
      password source_raw lang =
    if luser = (Jlib.nodeprep_exn "") then
      Lwt.return (`Error Jlib.err_bad_request)
    else (
      let jid = Jlib.make_jid_exn (luser:>string) (lserver:>string) "" in
	(* TODO
	   Access = gen_mod:get_module_opt(Server, ?MODULE, access, all),
           IPAccess = get_ip_access(Server),
	   case {acl:match_rule(Server, Access, JID),
		  check_ip_access(SourceRaw, IPAccess)} of *)
	match (`Allow, `Allow) with
	  | `Deny, _ ->
	      Lwt.return (`Error Jlib.err_forbidden)
	  | _, `Deny ->
	      Lwt.return (`Error Jlib.err_forbidden)
	  | `Allow, `Allow -> (
	      let source = may_remove_resource source_raw in
		if check_timeout source then (
		  if is_strong_password lserver password then (
		    match_lwt Auth.try_register luser lserver password with
		      | Auth.OK ->
			  send_welcome_message jid;
			  send_registration_notifications jid source;
			  Lwt.return `OK
		      | err ->
			  remove_timeout source;
			  match err with
			    | Auth.Exists ->
				Lwt.return (`Error Jlib.err_conflict)
			    | Auth.Invalid_jid ->
				Lwt.return (`Error Jlib.err_jid_malformed)
			    | Auth.Not_allowed ->
				Lwt.return (`Error Jlib.err_not_allowed)
			    | _ ->
				Lwt.return (`Error Jlib.err_internal_server_error)
		  ) else
		    let errtxt = "The password is too weak" in
		      Lwt.return (`Error (Jlib.errt_not_acceptable lang errtxt))
		) else
		  let errtxt = "Users are not allowed to register " ^
		    "accounts so quickly" in
		    Lwt.return (`Error (Jlib.errt_resource_constraint lang errtxt))
	    )
    )

  let try_set_password luser lserver password iq subel lang =
    if is_strong_password lserver password then (
      match_lwt Auth.set_password luser lserver password with
	| Auth.OK ->
	    Lwt.return (`IQ {iq with Jlib.iq_type = `Result (Some subel)})
	| Auth.Empty_password ->
	    Lwt.return (`IQ {iq with Jlib.iq_type =
			    `Error (Jlib.err_bad_request, Some subel)})
	| Auth.Not_allowed ->
	    Lwt.return (`IQ {iq with Jlib.iq_type =
			    `Error (Jlib.err_not_allowed, Some subel)})
	| Auth.Invalid_jid ->
	    Lwt.return (`IQ {iq with Jlib.iq_type =
			    `Error (Jlib.err_item_not_found, Some subel)})
	| _ ->
	    Lwt.return (`IQ {iq with Jlib.iq_type =
			    `Error (Jlib.err_internal_server_error, Some subel)})
    ) else
      let errtxt = "The password is too weak" in
	Lwt.return (`IQ {iq with Jlib.iq_type =
			`Error (Jlib.errt_not_acceptable lang errtxt, Some subel)})

  let try_register_or_set_password
      luser lserver password from iq
      subel source lang is_captcha_succeed =
    match from with
      | {Jlib.luser = u; Jlib.lserver = s; _} when u = luser && s = lserver ->
	  try_set_password luser lserver password iq subel lang
      | _ when is_captcha_succeed -> (
	  match check_from from lserver with
	    | true -> (
		match_lwt try_register luser lserver password source lang with
		  | `OK ->
		      Lwt.return
			(`IQ {iq with Jlib.iq_type = 
			     `Result (Some subel)})
		  | `Error error ->
		      Lwt.return
			(`IQ {iq with
				Jlib.iq_type =
			     `Error (error, Some subel)}))
	    | false ->
		Lwt.return
		  (`IQ {iq with
			  Jlib.iq_type =
		       `Error (Jlib.err_forbidden, Some subel)}))
      | _ ->
	  Lwt.return
	    (`IQ {iq with
		    Jlib.iq_type =
		 `Error (Jlib.err_not_allowed, Some subel)})

  let process_iq' from to'
      ({Jlib.iq_type = type';
	Jlib.iq_lang = lang;
	Jlib.iq_id = id; _} as iq) source =
    match type' with
      | `Set subel -> (
	  let utag_opt = Xml.get_subtag subel "username" in
	  let ptag_opt = Xml.get_subtag subel "password" in
	  let rtag_opt = Xml.get_subtag subel "remove" in
	  let lserver = to'.Jlib.lserver in
	  (* Access = gen_mod:get_module_opt(Server, ?MODULE, access, all),
	     AllowRemove = (allow == acl:match_rule(Server, Access, From)), *)
	  let allow_remove = true in
	    match utag_opt, ptag_opt, rtag_opt, allow_remove with
	      | Some utag, _, Some _rtag, true -> (
		  match Jlib.nodeprep (Xml.get_tag_cdata utag) with
		    | Some luser -> (
			match from with
			  | {Jlib.luser = u; Jlib.lserver = s; _}
			      when u = luser && s = lserver ->
			      lwt _ = Auth.remove luser lserver in
				Lwt.return
				  (`IQ {iq with Jlib.iq_type =
				       `Result (Some subel)})
			  | _ -> (
			      match ptag_opt with
				| Some ptag ->
				    let password = Xml.get_tag_cdata ptag in
				    lwt _ = Auth.remove'
				      luser lserver password in
				      Lwt.return
					(`IQ {iq with Jlib.iq_type =
					     `Result (Some subel)})
				| _ ->
				    Lwt.return
				      (`IQ {iq with
					      Jlib.iq_type =
					   `Error (Jlib.err_bad_request,
						   Some subel)})))
		    | None ->
			Lwt.return
			  (`IQ {iq with
				  Jlib.iq_type =
			       `Error (Jlib.err_jid_malformed, Some subel)}))
	      | None, _, Some _rtag, true -> (
		  match from with
		    | {Jlib.luser = luser;
		       Jlib.lserver = s; _} when s = lserver ->
			let res_iq = {Jlib.iq_xmlns = <:ns<REGISTER>>;
				      Jlib.iq_id = id;
				      Jlib.iq_lang = "";
				      Jlib.iq_type = `Result (Some subel)} in
			  Router.route from from (Jlib.iq_to_xml res_iq);
			  lwt _ = Auth.remove luser lserver in
			    Lwt.return `Ignore
		    | _ ->
			Lwt.return
			  (`IQ {iq with
				  Jlib.iq_type =
			       `Error (Jlib.err_not_allowed,
				       Some subel)}))
	      | Some utag, Some ptag, _, _ -> (
		  match Jlib.nodeprep (Xml.get_tag_cdata utag) with
		    | Some luser ->
			let password = Xml.get_tag_cdata ptag in
			  try_register_or_set_password
			    luser lserver password from
			    iq subel source lang
			    (not (is_captcha_enabled lserver))
		    | None ->
			Lwt.return
			  (`IQ {iq with
				  Jlib.iq_type =
			       `Error (Jlib.err_jid_malformed, Some subel)}))
	      | _ ->
		  if is_captcha_enabled lserver then (
		    try (
		      let () = Captcha.process_reply_exn subel in
			match process_xdata_submit subel with
			  | Some (luser, password) ->
			      try_register_or_set_password
				luser lserver password from
				iq subel source lang true
			  | None ->
			      Lwt.return
				(`IQ {iq with
					Jlib.iq_type =
				     `Error (Jlib.err_bad_request, Some subel)})
		    ) with
		      | Captcha.Err_malformed ->
			  Lwt.return
			    (`IQ {iq with
				    Jlib.iq_type =
				 `Error (Jlib.err_bad_request, Some subel)})
		      | _ ->
			  let errtxt = "The CAPTCHA verification has failed" in
			    Lwt.return
			      (`IQ {iq with
				      Jlib.iq_type =
				   `Error ((Jlib.errt_not_allowed
					      lang errtxt), Some subel)})
		  ) else (
		    Lwt.return
		      (`IQ {iq with
			      Jlib.iq_type =
			   `Error (Jlib.err_bad_request, Some subel)})))
      | `Get subel ->
	  lwt is_registered, username_subels, query_subels =
	    let {Jlib.user = user;
		 Jlib.luser = luser;
		 Jlib.lserver = lserver; _} = from in
	      match_lwt Auth.does_user_exist luser lserver with
		| true ->
		    Lwt.return (true, [`XmlCdata user],
				[`XmlElement ("registered", [], [])])
		| false ->
		    Lwt.return (false, [`XmlCdata user], [])
	  in
	    if (is_captcha_enabled to'.Jlib.lserver) && is_registered then (
	      let top_instr_el =
		`XmlElement ("instructions", [],
			     [`XmlCdata
				(Translate.translate
				   lang
				   ("You need a client that supports x:data "
				    ^ "and CAPTCHA to register"))]) in
	      let instr_el =
		`XmlElement ("instructions", [],
			     [`XmlCdata
				(Translate.translate
				   lang
				   ("Choose a username and password "
				    ^ "to register with this server"))]) in
	      let ufield =
		`XmlElement ("field",
			     [("type", "text-single");
			      ("label", Translate.translate lang "User");
			      ("var", "username")],
			     [`XmlElement ("required", [], [])]) in
	      let pfield =
		`XmlElement ("field",
			     [("type", "text-private");
			      ("label", Translate.translate lang "Password");
			      ("var", "password")],
			     [`XmlElement ("required", [], [])]) in
		try_lwt (
		  lwt captcha_els = Captcha.create_captcha_x_exn
		    id to' lang (Some source) [instr_el; ufield; pfield] [] in
		    Lwt.return
		      (`IQ {iq with Jlib.iq_type =
			   `Result (Some (`XmlElement
					    ("query",
					     [("xmlns", "jabber:iq:register")],
					     top_instr_el :: captcha_els)))})
		) with
		  | Captcha.Err_limit ->
		      let errtext = "Too many CAPTCHA requests" in
			Lwt.return
			  (`IQ {iq with
				  Jlib.iq_type =
			       `Error ((Jlib.errt_resource_constraint
					  lang errtext),
				       Some subel)})
		  | _ ->
		      let errtext = "Unable to generate a CAPTCHA" in
			Lwt.return
			  (`IQ {iq with
				  Jlib.iq_type =
			       `Error ((Jlib.errt_internal_server_error
					  lang errtext),
				       Some subel)})
	    ) else (
	      Lwt.return
		(`IQ {iq with Jlib.iq_type =
		     `Result
		       (Some
			  (`XmlElement
			     ("query",
			      [("xmlns", "jabber:iq:register")],
			      [`XmlElement
				 ("instructions", [],
				  [`XmlCdata
				     (Translate.translate lang
					("Choose a username and password " ^
					   "to register with this server"))]);
			       `XmlElement ("username", [], username_subels);
			       `XmlElement ("password", [], [])]
			      @ query_subels)))}))

  let process_iq from to' iq =
    process_iq' from to' iq (`JID (Jlib.jid_tolower from))

  let stream_feature_register acc _host =
    Lwt.return
      (Hooks.OK, (`XmlElement ("register",
			       [("xmlns", <:ns<FEATURE_IQREGISTER>>)],
			       [])) :: acc)

  let unauthenticated_iq_register _acc (server, iq, ip) =
    let address = `IP ip in
      match_lwt process_iq' (Jlib.make_jid_exn "" "" "")
	(Jlib.make_jid_exn "" server "") iq address with
	  | `IQ res_iq ->
	      let res1 = Jlib.replace_from_to (Jlib.make_jid_exn "" server "")
		(Jlib.make_jid_exn "" "" "") (Jlib.iq_to_xml res_iq) in
		Lwt.return (Hooks.OK, Jlib.remove_attr "to" res1)
	  | `Ignore ->
	      assert false

  let start host =
    Mod_disco.register_feature host <:ns<REGISTER>>;
    Lwt.return (
      [Gen_mod.iq_handler `Local host <:ns<REGISTER>> process_iq ();
       Gen_mod.iq_handler `SM host <:ns<REGISTER>> process_iq ();
       (* Gen_mod.fold_hook c2s_stream_features host
 	  stream_feature_register 50;
	  Gen_mod.fold_hook c2s_unauthenticated_iq host
	  unauthenticated_iq_register 50; *)
      ]
    )

  let stop host =
    Mod_disco.unregister_feature host <:ns<REGISTER>>;
    Lwt.return ()

end

let () = Gen_mod.register_mod (module ModRegister : Gen_mod.Module)
