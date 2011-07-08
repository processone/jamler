module GenIQHandler = Jamler_gen_iq_handler
module Hooks = Jamler_hooks
module Auth = Jamler_auth
module Router = Jamler_router
module Translate = Jamler_translate

module ModRegister :
sig
  include Gen_mod.Module
end
  =
struct
  let name = "mod_register"
  let section = Jamler_log.new_section name

  let is_captcha_enabled =
    Jamler_config.(get_module_opt_with_default
		     name ["captcha_protected"] bool false)

  let try_register_or_set_password
      luser lserver password from
      iq subel source lang =
    (* TODO *)
    Lwt.return (`IQ {iq with
		       Jlib.iq_type =
		    `Error (Jlib.err_internal_server_error,
			    Some subel)})

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
	    match utag_opt, rtag_opt, allow_remove with
	      | Some utag, Some rtag, true -> (
		  match Jlib.nodeprep (Xml.get_tag_cdata utag) with
		    | Some luser -> (
			match from with
			  | {Jlib.luser = u; Jlib.lserver = s; _}
			      when u = luser && s = lserver ->
			      lwt _ = Auth.remove_user luser lserver in
				Lwt.return
				  (`IQ {iq with Jlib.iq_type =
				       `Result (Some subel)})
			  | _ -> (
			      match ptag_opt with
				| Some ptag ->
				    let password = Xml.get_tag_cdata ptag in
				    lwt _ = Auth.remove_user'
				      luser lserver password in
				      Lwt.return
					(`IQ {iq with Jlib.iq_type =
					     `Result (Some subel)})
				    (* ok ->
					    IQ#iq{type = result,
						  sub_el = [SubEl]};
					%% TODO FIXME: This piece of
					%% code does not work since
					%% the code have been changed
					%% to allow several auth
					%% modules.  lists:foreach can
					%% only return ok:
					not_allowed ->
					    IQ#iq{type = error,
						  sub_el =
						  [SubEl, ?ERR_NOT_ALLOWED]};
					not_exists ->
					    IQ#iq{type = error,
						  sub_el =
						  [SubEl, ?ERR_ITEM_NOT_FOUND]};
					_ ->
					    IQ#iq{type = error,
						  sub_el =
						  [SubEl,
						   ?ERR_INTERNAL_SERVER_ERROR]} *)
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
	      | None, Some rtag, true -> (
		  match from with
		    | {Jlib.luser = luser;
		       Jlib.lserver = s;
		       Jlib.lresource = lresource; _} when s = lserver ->
			let res_iq = {Jlib.iq_xmlns = <:ns<REGISTER>>;
				      Jlib.iq_id = id;
				      Jlib.iq_lang = "";
				      Jlib.iq_type = `Result (Some subel)} in
			  Router.route
			    (Jlib.make_jid' luser lserver lresource)
			    (Jlib.make_jid' luser lserver lresource)
			    (Jlib.iq_to_xml res_iq);
			  lwt _ = Auth.remove_user luser lserver in
			    Lwt.return `Ignore
		    | _ ->
			Lwt.return
			  (`IQ {iq with
				  Jlib.iq_type =
			       `Error (Jlib.err_not_allowed,
				       Some subel)}))
	      | Some utag, Some ptag, _ -> (
		  match Jlib.nodeprep (Xml.get_tag_cdata utag) with
		    | Some luser ->
			let password = Xml.get_tag_cdata ptag in
			  try_register_or_set_password
			    luser lserver password from
			    iq subel source lang
		    | None ->
			Lwt.return
			  (`IQ {iq with
				  Jlib.iq_type =
			       `Error (Jlib.err_jid_malformed, Some subel)}))
	      | _ ->
		  if is_captcha_enabled lserver then (
		    (* case ejabberd_captcha:process_reply(SubEl) of
			ok ->
			    case process_xdata_submit(SubEl) of
				{ok, User, Password} ->
				    try_register_or_set_password(
				      User, Server, Password, From,
				      IQ, SubEl, Source, Lang, true);
				_ ->
				    IQ#iq{type = error,
					  sub_el = [SubEl, ?ERR_BAD_REQUEST]}
			    end;
			{error, malformed} ->
			    IQ#iq{type = error,
				  sub_el = [SubEl, ?ERR_BAD_REQUEST]};
			_ ->
			    ErrText = "The CAPTCHA verification has failed",
			    IQ#iq{type = error,
				  sub_el = [SubEl,
					    ?ERRT_NOT_ALLOWED(Lang, ErrText)]}
		    end; *)
		    Lwt.return
		      (`IQ {iq with
			      Jlib.iq_type =
			   `Error (Jlib.err_internal_server_error,
				   Some subel)})
		  ) else (
		    Lwt.return
		      (`IQ {iq with
			      Jlib.iq_type =
			   `Error (Jlib.err_bad_request, Some subel)})))
      | `Get subel ->
	  lwt is_registered, username_subels, query_subels =
	    match from with
	      | {Jlib.user = user;
		 Jlib.luser = luser;
		 Jlib.lserver = lserver; _} ->
		  match_lwt Auth.does_user_exist luser lserver with
		    | true ->
			Lwt.return (true, [`XmlCdata user],
				    [`XmlElement ("registered", [], [])])
		    | false ->
			Lwt.return (false, [`XmlCdata user], [])
	      | _ ->
		  Lwt.return (false, [], [])
	  in
	    if (is_captcha_enabled to'.Jlib.lserver) && is_registered then (
	      (* TopInstrEl = {xmlelement, "instructions", [],
				  [{xmlcdata,
				    translate:translate(
				      Lang, "You need a client that supports x:data "
				      "and CAPTCHA to register")}]},
		    InstrEl = {xmlelement, "instructions", [],
			       [{xmlcdata,
				 translate:translate(
				   Lang,
				   "Choose a username and password "
				   "to register with this server")}]},
		    UField = {xmlelement, "field",
			      [{"type", "text-single"},
			       {"label", translate:translate(Lang, "User")},
			       {"var", "username"}],
			      [{xmlelement, "required", [], []}]},
		    PField = {xmlelement, "field",
			      [{"type", "text-private"},
			       {"label", translate:translate(Lang, "Password")},
			       {"var", "password"}],
			      [{xmlelement, "required", [], []}]},
		    case ejabberd_captcha:create_captcha_x(
			   ID, To, Lang, Source, [InstrEl, UField, PField]) of
			{ok, CaptchaEls} ->
			    IQ#iq{type = result,
				  sub_el = [{xmlelement, "query",
					     [{"xmlns", "jabber:iq:register"}],
					     [TopInstrEl | CaptchaEls]}]};
                        {error, limit} ->
                            ErrText = "Too many CAPTCHA requests",
                            IQ#iq{type = error,
				  sub_el = [SubEl, ?ERRT_RESOURCE_CONSTRAINT(
                                                      Lang, ErrText)]};
			_Err ->
			    ErrText = "Unable to generate a CAPTCHA",
			    IQ#iq{type = error,
				  sub_el = [SubEl, ?ERRT_INTERNAL_SERVER_ERROR(
						      Lang, ErrText)]}
		    end; *)
	      Lwt.return (`IQ {iq with
				 Jlib.iq_type =
			      `Error (Jlib.err_internal_server_error,
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
    process_iq' from to' iq (Jlib.jid_tolower from)

  let stream_feature_register acc host =
    Lwt.return
      (Hooks.OK, (`XmlElement ("register",
			       [("xmlns", <:ns<FEATURE_IQREGISTER>>)],
			       [])) :: acc)

  let unauthenticated_iq_register _acc (server, iq, ip) =
    let address = match ip with
      | Some (addr, port) -> Some addr
      | _ -> None
    in
      match_lwt process_iq' (Jlib.make_jid_exn "" "" "")
	(Jlib.make_jid_exn "" server "") iq address with
	  | `IQ res_iq ->
	      let res1 = Jlib.replace_from_to (Jlib.make_jid_exn "" server "")
		(Jlib.make_jid_exn "" "" "") (Jlib.iq_to_xml res_iq) in
		Lwt.return (Hooks.OK, Jlib.remove_attr "to" res1)
	  | `Ignore ->
	      assert false

  let start host =
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
    Lwt.return ()

end

let () = Gen_mod.register_mod (module ModRegister : Gen_mod.Module)
