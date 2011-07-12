module Translate = Jamler_translate
module Config = Jamler_config

let section = Jamler_log.new_section "captcha"

type result = | Valid
	      | Invalid
	      | No_found

exception Err_malformed
exception Err_limit
exception Err_nodata
exception Err_malformed_image
exception Err_fbig
exception Err_timeout
exception Err_bad_match
exception Err_not_found

type captcha = {key: string;
		timer: bool; (* TODO *)
		callback: (result -> unit) option}

type captcha_tbl = (string, captcha) Hashtbl.t
let captcha_tbl = Hashtbl.create 10

let vfield type' var value =
  `XmlElement ("field", [("type", type'); ("var", var)],
	       [`XmlElement ("value", [], [value])])

let captcha_text lang =
  Translate.translate lang "Enter the text you see"

let captcha_lifetime = 120000 (* two minutes *)
let limit_period = 60*1000*1000 (* one minute *)
(* -define(RPC_TIMEOUT, 5000). *)

let captcha_url = Config.(get_global_opt_with_default ["captcha_url"] string "")
let captcha_cmd = Config.(get_global_opt_with_default ["captcha_cmd"] string "")
let captcha_limit = Config.(get_global_opt ["captcha_limit"] int)

let is_feature_available () =
  match captcha_cmd () with
    | "" -> false
    | _ -> true

let get_url str =
  match captcha_url () with
    | "" -> (
	match (Config.myhosts () :> string list) with
	  | myhost :: _ ->
	      "http://" ^ myhost ^ "/captcha/" ^ str
	  | [] ->
	      "http://localhost/captcha/" ^ str
      )
    | url ->
	url ^ "/" ^ str

let is_limited = function
  | None ->
      false
  | Some limiter -> (
      match captcha_limit () with
	| Some int when int > 0 ->
	    (* TODO
	       case catch gen_server:call(?MODULE, {is_limited, Limiter, Int},
                                       5000) of
                true ->
                    true;
                false ->
                    false;
                Err ->
                    ?ERROR_MSG("Call failed: ~p", [Err]),
                    false
               end; *)
	    false
	| _ ->
	    false
    )

let cmd cmdline =
  (* TODO *)
  Lwt.fail Err_nodata

let do_create_image key =
  let filename = captcha_cmd () in
  let cmdline = filename ^ " " ^ key in
    match cmd cmdline with
	(* TODO *)
      | res ->
	  res

let create_image' limiter key =
  match is_limited limiter with
    | true ->
	Lwt.fail Err_limit
    | false ->
	do_create_image key

let create_image limiter =
  (* Six numbers from 1 to 9. *)
  let key = String.create 6 in
    for i=0 to 5 do (
      let tmp = Jlib.get_random_string () in
	key.[i] <- tmp.[0]
    ) done;
    create_image' limiter key

let create_captcha_exn sid from to' lang limiter callback =
  lwt type', key, image = create_image limiter in
    (* TODO:
       Id = randoms:get_string() ++ "-" ++ ejabberd_cluster:node_id(), *)
  let id = Jlib.get_random_string () in
  let b64image = Jlib.encode_base64 image in
  let jid = Jlib.jid_to_string from in
  let cid = "sha1+" ^ (Jlib.sha1 image) ^ "@bob.xmpp.org" in
  let data = `XmlElement ("data",
			  [("xmlns", <:ns<BOB>>); ("cid", cid);
			   ("max-age", "0"); ("type", type')],
			  [`XmlCdata b64image]) in
  let captcha =
    `XmlElement
      ("captcha", [("xmlns", <:ns<CAPTCHA>>)],
       [`XmlElement
	  ("x", [("xmlns", <:ns<XDATA>>); ("type", "form")],
	   [vfield "hidden" "FORM_TYPE" (`XmlCdata <:ns<CAPTCHA>>);
	    vfield "hidden" "from" (`XmlCdata (Jlib.jid_to_string to'));
	    vfield "hidden" "challenge" (`XmlCdata id);
	    vfield "hidden" "sid" (`XmlCdata sid);
	    `XmlElement
	      ("field", [("var", "ocr"); ("label", captcha_text lang)],
	       [`XmlElement ("required", [], []);
		`XmlElement
		  ("media", [("xmlns", <:ns<MEDIA>>)],
		   [`XmlElement
		      ("uri", [("type", type')],
		       [`XmlCdata ("cid:" ^ cid)])])])])]) in
  let body_string1 = Translate.translate lang
    ("Your messages to %s are being blocked. To unblock them, visit %s") in
  (* TODO:
     let body_string = Printf.sprintf body_string1 jid (get_url id) in *)
  let body_string = "Your messages are being blocked" in
  let body = `XmlElement ("body", [], [`XmlCdata body_string]) in
  let oob = `XmlElement ("x", [("xmlns", <:ns<OOB>>)],
			 [`XmlElement ("url", [],
				       [`XmlCdata (get_url id)])]) in
    (* TODO:
       Tref = erlang:send_after(?CAPTCHA_LIFETIME, ?MODULE, {remove_id, Id}), *)
  let timer = true in
  let _ = Hashtbl.replace captcha_tbl id
    {key; timer; callback = Some callback} in
    Lwt.return (id, [body; oob; captcha; data])

let create_captcha_x_exn sid to' lang limiter head_els tail_els =
  lwt type', key, image = create_image limiter in
    (* TODO:
       Id = randoms:get_string() ++ "-" ++ ejabberd_cluster:node_id(), *)
  let id = Jlib.get_random_string () in
  let b64image = Jlib.encode_base64 image in
  let cid = "sha1+" ^ (Jlib.sha1 image) ^ "@bob.xmpp.org" in
  let data = `XmlElement ("data",
			  [("xmlns", <:ns<BOB>>); ("cid", cid);
			   ("max-age", "0"); ("type", type')],
			  [`XmlCdata b64image]) in
  let help_txt = Translate.translate lang
    "If you don't see the CAPTCHA image here, visit the web page." in
  let image_url = get_url (id ^ "/image") in
  let captcha =
    `XmlElement
      ("x", [("xmlns", <:ns<XDATA>>); ("type", "form")],
       ((vfield "hidden" "FORM_TYPE" (`XmlCdata <:ns<CAPTCHA>>)) :: head_els @
	  [`XmlElement ("field", [("type", "fixed")],
			[`XmlElement ("value", [], [`XmlCdata help_txt])]);
	   `XmlElement ("field", [("type", "hidden");
				  ("var", "captchahidden")],
			[`XmlElement ("value", [],
				      [`XmlCdata "workaround-for-psi"])]);
	   `XmlElement ("field", [("type", "text-single");
				  ("label", Translate.translate
				     lang "CAPTCHA web page");
				  ("var", "url")],
			[`XmlElement ("value", [], [`XmlCdata image_url])]);
	   vfield "hidden" "from" (`XmlCdata (Jlib.jid_to_string to'));
	   vfield "hidden" "challenge" (`XmlCdata id);
	   vfield "hidden" "sid" (`XmlCdata sid);
	   `XmlElement
	     ("field", [("var", "ocr"); ("label", captcha_text lang)],
	      [`XmlElement ("required", [], []);
	       `XmlElement
		 ("media", [("xmlns", <:ns<MEDIA>>)],
		  [`XmlElement
		     ("uri", [("type", type')],
		      [`XmlCdata ("cid:" ^ cid)])])])] @ tail_els)) in
    (* TODO:
       Tref = erlang:send_after(?CAPTCHA_LIFETIME, ?MODULE, {remove_id, Id}), *)
  let timer = true in
  let _ = Hashtbl.replace captcha_tbl id {key; timer; callback = None} in
    Lwt.return [captcha; data]

let create_captcha_x_exn' sid to' lang limiter head_els =
  create_captcha_x_exn sid to' lang limiter head_els []

(* TODO
build_captcha_html(Id, Lang) ->
    case lookup_captcha(Id) of
	{ok, _} ->
	    ImgEl = {xmlelement, "img", [{"src", get_url(Id ++ "/image")}], []},
	    TextEl = {xmlcdata, ?CAPTCHA_TEXT(Lang)},
	    IdEl = {xmlelement, "input", [{"type", "hidden"},
					  {"name", "id"},
					  {"value", Id}], []},
	    KeyEl = {xmlelement, "input", [{"type", "text"},
					   {"name", "key"},
					   {"size", "10"}], []},
	    FormEl = {xmlelement, "form", [{"action", get_url(Id)},
					   {"name", "captcha"},
					   {"method", "POST"}],
		      [ImgEl,
		       {xmlelement, "br", [], []},
		       TextEl,
		       {xmlelement, "br", [], []},
		       IdEl,
		       KeyEl,
		       {xmlelement, "br", [], []},
		       {xmlelement, "input", [{"type", "submit"},
					      {"name", "enter"},
					      {"value", "OK"}], []}
		      ]},
	    {FormEl, {ImgEl, TextEl, IdEl, KeyEl}};
	_ ->
	    captcha_not_found
    end.
*)

(* TODO:
   check_captcha(Id, ProvidedKey) ->
    case string:tokens(Id, "-") of
	[_, NodeID] ->
	    case ejabberd_cluster:get_node_by_id(NodeID) of
		Node when Node == node() ->
		    do_check_captcha(Id, ProvidedKey);
		Node ->
		    case catch rpc:call(Node, ?MODULE, check_captcha,
					[Id, ProvidedKey], ?RPC_TIMEOUT) of
			{'EXIT', _} ->
			    captcha_not_found;
			{badrpc, _} ->
			    captcha_not_found;
			Res ->
			    Res
		    end
	    end;
	_ ->
	    captcha_not_found
    end. *)

(* TODO
   lookup_captcha(Id) ->
    case string:tokens(Id, "-") of
	[_, NodeID] ->
	    case ejabberd_cluster:get_node_by_id(NodeID) of
		Node when Node == node() ->
		    case ets:lookup(captcha, Id) of
			[C] ->
			    {ok, C};
			_ ->
			    {error, enoent}
		    end;
		Node ->
		    case catch rpc:call(Node, ets, lookup,
					[captcha, Id], ?RPC_TIMEOUT) of
			[C] ->
			    {ok, C};
			_ ->
			    {error, enoent}
		    end
	    end;
	_ ->
	    {error, enoent}
   end. *)

let lookup_captcha id =
  try
    Some (Hashtbl.find captcha_tbl id)
  with
    | Not_found ->
	None

(* this was do_check_captcha *)
let check_captcha id provided_key =
  match lookup_captcha id with
    | Some {key = valid_key;
	    timer = _timer;
	    callback = callback} ->
	Hashtbl.remove captcha_tbl id;
	(* TODO
	   erlang:cancel_timer(Tref), *)
	let result = if valid_key = provided_key then Valid else Invalid in
	let _ = match callback with
	  | Some f -> f result
	  | None -> ()
	in
	  result
    | None ->
	No_found

let process_reply_exn el =
  match el with
    | `XmlElement _ -> (
	match Xml.get_subtag el "x" with
	  | None ->
	      raise Err_malformed
	  | Some xdata ->
	      let fields = Jlib.parse_xdata_submit xdata in
		try (
		  match (List.assoc "challenge" fields,
			 List.assoc "ocr" fields) with
		    | id :: _, ocr :: _ -> (
			match check_captcha id ocr with
			  | Valid -> ()
			  | Invalid -> raise Err_bad_match
			  | No_found -> raise Err_not_found
		      )
		    | _ ->
			raise Err_malformed
		)
		with
		  | Not_found ->
		      raise Err_malformed
      )
    | `XmlCdata _ ->
	raise Err_malformed

(* TODO
process(_Handlers, #request{method='GET', lang=Lang, path=[_, Id]}) ->
    case build_captcha_html(Id, Lang) of
	{FormEl, _} when is_tuple(FormEl) ->
	    Form =
		{xmlelement, "div", [{"align", "center"}],
		 [FormEl]},
	    ejabberd_web:make_xhtml([Form]);
	captcha_not_found ->
	    ejabberd_web:error(not_found)
    end;

process(_Handlers, #request{method='GET', path=[_, Id, "image"], ip = IP}) ->
    {Addr, _Port} = IP,
    case lookup_captcha(Id) of
	{ok, #captcha{key=Key}} ->
	    case create_image(Addr, Key) of
		{ok, Type, _, Img} ->
		    {200,
		     [{"Content-Type", Type},
		      {"Cache-Control", "no-cache"},
		      {"Last-Modified", httpd_util:rfc1123_date()}],
		     Img};
                {error, limit} ->
                    ejabberd_web:error(not_allowed);
		_ ->
		    ejabberd_web:error(not_found)
	    end;
	_ ->
	    ejabberd_web:error(not_found)
    end;

process(_Handlers, #request{method='POST', q=Q, lang=Lang, path=[_, Id]}) ->
    ProvidedKey = proplists:get_value("key", Q, none),
    case check_captcha(Id, ProvidedKey) of
	captcha_valid ->
	    Form =
		{xmlelement, "p", [],
		 [{xmlcdata,
		   translate:translate(Lang, "The captcha is valid.")
		  }]},
	    ejabberd_web:make_xhtml([Form]);
	captcha_non_valid ->
	    ejabberd_web:error(not_allowed);
	captcha_not_found ->
	    ejabberd_web:error(not_found)
    end;

process(_Handlers, _Request) ->
    ejabberd_web:error(not_found).
*)

(* TODO
   handle_call({is_limited, Limiter, RateLimit}, _From, State) ->
    NowPriority = now_priority(),
    CleanPriority = NowPriority + ?LIMIT_PERIOD,
    Limits = clean_treap(State#state.limits, CleanPriority),
    case treap:lookup(Limiter, Limits) of
        {ok, _, Rate} when Rate >= RateLimit ->
            {reply, true, State#state{limits = Limits}};
        {ok, Priority, Rate} ->
            NewLimits = treap:insert(Limiter, Priority, Rate+1, Limits),
            {reply, false, State#state{limits = NewLimits}};
        _ ->
            NewLimits = treap:insert(Limiter, NowPriority, 1, Limits),
            {reply, false, State#state{limits = NewLimits}}
    end;

...........

handle_info({remove_id, Id}, State) ->
    ?DEBUG("captcha ~p timed out", [Id]),
    case ets:lookup(captcha, Id) of
	[#captcha{args=Args, pid=Pid}] ->
	    if is_pid(Pid) ->
		    Pid ! {captcha_failed, Args};
	       true ->
		    ok
	    end,
	    ets:delete(captcha, Id);
	_ ->
	    ok
    end,
    {noreply, State};
*)

let check_captcha_setup () =
  match is_feature_available () with
    | true -> (
	try_lwt
	  lwt _ = create_image None in
	    Lwt.return ()
	with
	  | _ ->
	      Lwt_log.warning ~section
		("captcha is enabled in the option captcha_cmd, " ^
		   "but it can't generate images")
      )
    | false ->
	Lwt.return ()
