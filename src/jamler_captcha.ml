module Translate = Jamler_translate
module Config = Jamler_config

let src = Jamler_log.new_src "captcha"

let prefix pref string =
  let len = String.length pref in
    try
      (Str.first_chars string len) = pref
    with
      | _ -> false

type limiter = [ `JID of Jlib.LJID.t | `IP of Unix.inet_addr ]

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
		timer: Process.timer;
		callback: (result -> unit) option}

type captcha_tbl = (string, captcha) Hashtbl.t
let captcha_tbl = Hashtbl.create 10

let vfield type' var value =
  `XmlElement ("field", [("type", type'); ("var", var)],
	       [`XmlElement ("value", [], [value])])

let captcha_text lang =
  Translate.translate lang "Enter the text you see"

let captcha_lifetime = 120.0 (* two minutes *)
let captcha_limit_period = 60.0 (* one minute *)
(* -define(RPC_TIMEOUT, 5000). *)
let cmd_timeout = 5.0 (* 5 seconds *)
(* -define(MAX_FILE_SIZE, 64*1024). *)

let captcha_url = Config.(get_global_opt_with_default ["captcha_url"] string "")
let captcha_cmd = Config.(get_global_opt_with_default ["captcha_cmd"] string "")
let captcha_limit = Config.(get_global_opt ["captcha_limit"] int)

module LimitTreap = Treap.Make
  (struct
     let compare = Stdlib.compare
     let hash = Hashtbl.hash
     let equal = (=)
     type t = limiter
     end)
  (struct
     let compare = Stdlib.compare
     type t = float
   end)

let limits_treap = ref LimitTreap.empty

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

let rec clean_treap treap clean_priority =
    match LimitTreap.is_empty treap with
      | true ->
	  treap
      | false ->
	  let _key, priority, _value = LimitTreap.get_root treap in
	    if priority > clean_priority then (
	      clean_treap (LimitTreap.delete_root treap) clean_priority
	    ) else
	      treap

let is_limited' limiter rate_limit =
  let now_priority = -1.0 *. (Unix.gettimeofday ()) in
  let clean_priority = now_priority +. captcha_limit_period in
  let limits = clean_treap !limits_treap clean_priority in
    match LimitTreap.lookup limiter limits with
      | Some (_, rate) when rate >= rate_limit ->
	  limits_treap := limits;
	  true
      | Some (priority, rate) ->
	  limits_treap := LimitTreap.insert limiter priority (rate+1) limits;
	  false
      | None ->
	  limits_treap := LimitTreap.insert limiter now_priority 1 limits;
	  false

let is_limited = function
  | None ->
      false
  | Some limiter -> (
      match captcha_limit () with
	| Some int when int > 0 ->
	    is_limited' limiter int
	| _ ->
	    false
    )

let cmd cmdline key =
  (*let args = [|cmdline; key|] in
  Lwt_process.pread ~timeout:cmd_timeout (cmdline, args)*)
  let args = [cmdline; key] in
  (* TODO: use timeout *)
  Eio.Process.parse_out (Process.get_global_env ())#process_mgr
    Eio.Buf_read.take_all args

let do_create_image key =
  let filename = captcha_cmd () in
  match cmd filename key with
  | "" ->
     Logs.err ~src
       (fun m ->
         m "failed to process output from '%s %s': no data"
	   filename key);
     raise Err_nodata
  | data ->
     if prefix "\x89PNG\r\n\x1a\n" data then
       ("image/png", key, data)
     else if prefix "\xff\xd8" data then
       ("image/jpeg", key, data)
     else if prefix "GIF87a" data then
       ("image/gif", key, data)
     else if prefix "GIF89a" data then
       ("image/gif", key, data)
     else (
       Logs.err ~src
	 (fun m ->
           m "failed to process output from '%s %s': malformed image"
	     filename key);
       raise Err_malformed_image
     )

let create_image' limiter key =
  match is_limited limiter with
  | true ->
     raise Err_limit
  | false ->
     do_create_image key

let create_image limiter =
  (* Six numbers from 1 to 9. *)
  let key =
    String.init 6
      (fun _ -> Char.chr (Random.int 9 + 1 + Char.code '0'))
  in
  create_image' limiter key

let lookup_captcha id =
  try
    Some (Hashtbl.find captcha_tbl id)
  with
    | Not_found ->
	None

let remove_id id () =
  Logs.debug ~src (fun m -> m "captcha %s timed out" id);
  match lookup_captcha id with
  | Some {callback = callback; _} -> (
    Hashtbl.remove captcha_tbl id;
    match callback with
    | Some f -> f Invalid
    | None -> ()
  )
  | _ ->
     ()

let create_captcha_exn sid _from to' lang limiter callback =
  let type', key, image = create_image limiter in
    (* TODO:
       Id = randoms:get_string() ++ "-" ++ ejabberd_cluster:node_id(), *)
  let id = Jlib.get_random_string () in
  let b64image = Jlib.encode_base64 image in
  (*let jid = Jlib.jid_to_string from in*)
  let cid = "sha1+" ^ (Jlib.sha1 image) ^ "@bob.xmpp.org" in
  let data = `XmlElement ("data",
			  [("xmlns", [%xmlns "BOB"]); ("cid", cid);
			   ("max-age", "0"); ("type", type')],
			  [`XmlCdata b64image]) in
  let captcha =
    `XmlElement
      ("captcha", [("xmlns", [%xmlns "CAPTCHA"])],
       [`XmlElement
	  ("x", [("xmlns", [%xmlns "XDATA"]); ("type", "form")],
	   [vfield "hidden" "FORM_TYPE" (`XmlCdata [%xmlns "CAPTCHA"]);
	    vfield "hidden" "from" (`XmlCdata (Jlib.jid_to_string to'));
	    vfield "hidden" "challenge" (`XmlCdata id);
	    vfield "hidden" "sid" (`XmlCdata sid);
	    `XmlElement
	      ("field", [("var", "ocr"); ("label", captcha_text lang)],
	       [`XmlElement ("required", [], []);
		`XmlElement
		  ("media", [("xmlns", [%xmlns "MEDIA"])],
		   [`XmlElement
		      ("uri", [("type", type')],
		       [`XmlCdata ("cid:" ^ cid)])])])])]) in
  (* TODO:
  let body_string1 = Translate.translate lang
    ("Your messages to %s are being blocked. To unblock them, visit %s") in
     let body_string = Printf.sprintf body_string1 jid (get_url id) in *)
  let body_string = "Your messages are being blocked" in
  let body = `XmlElement ("body", [], [`XmlCdata body_string]) in
  let oob = `XmlElement ("x", [("xmlns", [%xmlns "OOB"])],
			 [`XmlElement ("url", [],
				       [`XmlCdata (get_url id)])]) in
  let timer = Process.apply_after captcha_lifetime (remove_id id) in
  let _ = Hashtbl.replace captcha_tbl id
            {key; timer; callback = Some callback} in
  (id, [body; oob; captcha; data])

let create_captcha_x_exn sid to' lang limiter head_els tail_els =
  let type', key, image = create_image limiter in
    (* TODO:
       Id = randoms:get_string() ++ "-" ++ ejabberd_cluster:node_id(), *)
  let id = Jlib.get_random_string () in
  let b64image = Jlib.encode_base64 image in
  let cid = "sha1+" ^ (Jlib.sha1 image) ^ "@bob.xmpp.org" in
  let data = `XmlElement ("data",
			  [("xmlns", [%xmlns "BOB"]); ("cid", cid);
			   ("max-age", "0"); ("type", type')],
			  [`XmlCdata b64image]) in
  let help_txt = Translate.translate lang
    "If you don't see the CAPTCHA image here, visit the web page." in
  let image_url = get_url (id ^ "/image") in
  let captcha =
    `XmlElement
      ("x", [("xmlns", [%xmlns "XDATA"]); ("type", "form")],
       ((vfield "hidden" "FORM_TYPE" (`XmlCdata [%xmlns "CAPTCHA"])) :: head_els @
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
		 ("media", [("xmlns", [%xmlns "MEDIA"])],
		  [`XmlElement
		     ("uri", [("type", type')],
		      [`XmlCdata ("cid:" ^ cid)])])])] @ tail_els)) in
  let timer = Process.apply_after captcha_lifetime (remove_id id) in
  let _ = Hashtbl.replace captcha_tbl id {key; timer; callback = None} in
  [captcha; data]

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

(* this was do_check_captcha *)
let check_captcha id provided_key =
  match lookup_captcha id with
    | Some {key = valid_key;
	    timer = timer;
	    callback = callback} ->
	Hashtbl.remove captcha_tbl id;
	Process.cancel_timer timer;
	let result = if valid_key = provided_key then Valid else Invalid in
	let _ = match callback with
	  | Some f -> f result
	  | None -> ()
	in
	  result
    | None ->
	No_found

let process_reply_exn el =
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

let check_captcha_setup () =
  match is_feature_available () with
  | true -> (
    try
      let _ = create_image None in
      ()
    with
    | _ ->
       Logs.warn ~src
	 (fun m ->
           m "captcha is enabled in the option captcha_cmd, but it can't generate images");
       ()
  )
  | false ->
     ()
