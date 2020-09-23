type namepreped = string
type nodepreped = string
type resourcepreped = string

type jid = {user : string;
	    server : string;
	    resource : string;
	    luser : nodepreped;
	    lserver : namepreped;
	    lresource : resourcepreped;
	   }

let nameprep_exn = Stringprep.nameprep
let nodeprep_exn = Stringprep.nodeprep
let resourceprep_exn = Stringprep.resourceprep

let start_time = Unix.time ()

let nameprep s =
  try
    Some (nameprep_exn s)
  with
    | Invalid_argument "stringprep" -> None

let nodeprep s =
  try
    Some (nodeprep_exn s)
  with
    | Invalid_argument "stringprep" -> None

let resourceprep s =
  try
    Some (resourceprep_exn s)
  with
    | Invalid_argument "stringprep" -> None

exception Bad_jid

let make_jid_exn user server resource =
  let luser = nodeprep_exn user
  and lserver = nameprep_exn server
  and lresource = resourceprep_exn resource in
    {user; server; resource; luser; lserver; lresource}

let make_jid user server resource =
  try
    Some (make_jid_exn user server resource)
  with
    | Invalid_argument "stringprep" -> None

let make_jid' luser lserver lresource =
  {user = luser; server = lserver; resource = lresource;
   luser; lserver; lresource}

let ljid_to_jid (luser, lserver, lresource) =
  make_jid' luser lserver lresource

let string_to_jid_exn str =
  let rec parse1 str i =
    if i < String.length str then (
      match str.[i] with
	| '@' ->
	    if i = 0
	    then raise Bad_jid
	    else parse2 str (String.sub str 0 i) (i + 1) (i + 1)
	| '/' ->
	    if i = 0
	    then raise Bad_jid
	    else parse3 str "" (String.sub str 0 i) (i + 1)
	| _ -> parse1 str (i + 1)
    ) else (
      if i = 0
      then raise Bad_jid
      else make_jid_exn "" str ""
    )
  and parse2 str u p i =
    if i < String.length str then (
      match str.[i] with
	| '@' ->
	    raise Bad_jid
	| '/' ->
	    if i = p
	    then raise Bad_jid
	    else parse3 str u (String.sub str p (i - p)) (i + 1)
	| _ -> parse2 str u p (i + 1)
    ) else (
      if i = p
      then raise Bad_jid
      else make_jid_exn u (String.sub str p (i - p)) ""
    )
  and parse3 str u s i =
    make_jid_exn u s (String.sub str i (String.length str - i))
  in
    parse1 str 0

let string_to_jid str =
  try
    Some (string_to_jid_exn str)
  with
    | Invalid_argument "stringprep"
    | Bad_jid -> None

let jid_to_string' user server resource =
  let s1 =
    match user with
      | "" -> ""
      | _ -> user ^ "@"
  in
  let s2 = s1 ^ server in
  let s3 =
    match resource with
      | "" -> s2;
      | _ -> s2 ^ "/" ^ resource
  in
    s3

let jid_to_string {user; server; resource; _} =
  jid_to_string' user server resource

let jid_tolower {luser = u; lserver = s; lresource = r; _} =
  (u, s, r)

let jid_remove_resource jid =
  {jid with resource = ""; lresource = ""}

let jid_replace_resource' jid lresource =
  {jid with resource = (lresource :> string); lresource}


let stanza_error code type' condition =
  `XmlElement
    ("error",
     [("code", code); ("type", type')],
     [`XmlElement (condition, [("xmlns", [%ns:STANZAS])], [])])

let err_bad_request =
  stanza_error "400" "modify" "bad-request"
let err_conflict =
  stanza_error "409" "cancel" "conflict"
let err_feature_not_implemented =
  stanza_error "501" "cancel" "feature-not-implemented"
let err_forbidden =
  stanza_error "403" "auth"   "forbidden"
let err_gone =
  stanza_error "302" "modify" "gone"
let err_internal_server_error =
  stanza_error "500" "wait"   "internal-server-error"
let err_item_not_found =
  stanza_error "404" "cancel" "item-not-found"
let err_jid_malformed =
  stanza_error "400" "modify" "jid-malformed"
let err_not_acceptable =
  stanza_error "406" "modify" "not-acceptable"
let err_not_allowed =
  stanza_error "405" "cancel" "not-allowed"
let err_not_authorized =
  stanza_error "401" "auth"   "not-authorized"
let err_payment_required =
  stanza_error "402" "auth"   "payment-required"
let err_recipient_unavailable =
  stanza_error "404" "wait"   "recipient-unavailable"
let err_redirect =
  stanza_error "302" "modify" "redirect"
let err_registration_required =
  stanza_error "407" "auth"   "registration-required"
let err_remote_server_not_found =
  stanza_error "404" "cancel" "remote-server-not-found"
let err_remote_server_timeout =
  stanza_error "504" "wait"   "remote-server-timeout"
let err_resource_constraint =
  stanza_error "500" "wait"   "resource-constraint"
let err_service_unavailable =
  stanza_error "503" "cancel" "service-unavailable"
let err_subscription_required =
  stanza_error "407" "auth"   "subscription-required"
let err_unexpected_request =
  stanza_error "400" "wait"   "unexpected-request"
let err_unexpected_request_cancel =
  stanza_error "401" "cancel" "unexpected-request"

let translate _lang text = text	(* TODO *)

let stanza_errort code type' condition lang text =
  `XmlElement
    ("error",
     [("code", code); ("type", type')],
     [`XmlElement (condition, [("xmlns", [%ns:STANZAS])], []);
      `XmlElement ("text", [("xmlns", [%ns:STANZAS])],
		   [`XmlCdata (translate lang text)])])

let errt_bad_request lang text =
  stanza_errort "400" "modify" "bad-request" lang text
let errt_conflict lang text =
  stanza_errort "409" "cancel" "conflict" lang text
let errt_feature_not_implemented lang text =
  stanza_errort "501" "cancel" "feature-not-implemented" lang text
let errt_forbidden lang text =
  stanza_errort "403" "auth"   "forbidden" lang text
let errt_gone lang text =
  stanza_errort "302" "modify" "gone" lang text
let errt_internal_server_error lang text =
  stanza_errort "500" "wait"   "internal-server-error" lang text
let errt_item_not_found lang text =
  stanza_errort "404" "cancel" "item-not-found" lang text
let errt_jid_malformed lang text =
  stanza_errort "400" "modify" "jid-malformed" lang text
let errt_not_acceptable lang text =
  stanza_errort "406" "modify" "not-acceptable" lang text
let errt_not_allowed lang text =
  stanza_errort "405" "cancel" "not-allowed" lang text
let errt_not_authorized lang text =
  stanza_errort "401" "auth"   "not-authorized" lang text
let errt_payment_required lang text =
  stanza_errort "402" "auth"   "payment-required" lang text
let errt_recipient_unavailable lang text =
  stanza_errort "404" "wait"   "recipient-unavailable" lang text
let errt_redirect lang text =
  stanza_errort "302" "modify" "redirect" lang text
let errt_registration_required lang text =
  stanza_errort "407" "auth"   "registration-required" lang text
let errt_remote_server_not_found lang text =
  stanza_errort "404" "cancel" "remote-server-not-found" lang text
let errt_remote_server_timeout lang text =
  stanza_errort "504" "wait"   "remote-server-timeout" lang text
let errt_resource_constraint lang text =
  stanza_errort "500" "wait"   "resource-constraint" lang text
let errt_service_unavailable lang text =
  stanza_errort "503" "cancel" "service-unavailable" lang text
let errt_subscription_required lang text =
  stanza_errort "407" "auth"   "subscription-required" lang text
let errt_unexpected_request lang text =
  stanza_errort "400" "wait"   "unexpected-request" lang text

(* Auth stanza errors *)
let err_auth_no_resource_provided lang =
  errt_not_acceptable lang "No resource provided"
let err_auth_bad_resource_format lang =
  errt_not_acceptable lang "Illegal resource format"
let err_auth_resource_conflict lang =
  errt_conflict lang "Resource conflict"

let stream_error condition cdata =
  `XmlElement
    ("stream:error",
     [],
     [`XmlElement (condition, [("xmlns", [%ns:STREAMS])],
		   [`XmlCdata cdata])])

let serr_bad_format = stream_error "bad-format" ""
let serr_bad_namespace_prefix = stream_error "bad-namespace-prefix" ""
let serr_conflict = stream_error "conflict" ""
let serr_connection_timeout = stream_error "connection-timeout" ""
let serr_host_gone = stream_error "host-gone" ""
let serr_host_unknown = stream_error "host-unknown" ""
let serr_improper_addressing = stream_error "improper-addressing" ""
let serr_internal_server_error = stream_error "internal-server-error" ""
let serr_invalid_from = stream_error "invalid-from" ""
let serr_invalid_id = stream_error "invalid-id" ""
let serr_invalid_namespace = stream_error "invalid-namespace" ""
let serr_invalid_xml = stream_error "invalid-xml" ""
let serr_not_authorized = stream_error "not-authorized" ""
let serr_policy_violation = stream_error "policy-violation" ""
let serr_remote_connection_failed = stream_error "remote-connection-failed" ""
let serr_resourse_constraint = stream_error "resource-constraint" ""
let serr_restricted_xml = stream_error "restricted-xml" ""
let serr_see_other_host host = stream_error "see-other-host" host
let serr_system_shutdown = stream_error "system-shutdown" ""
let serr_unsupported_encoding = stream_error "unsupported-encoding" ""
let serr_unsupported_stanza_type = stream_error "unsupported-stanza-type" ""
let serr_unsupported_version = stream_error "unsupported-version" ""
let serr_xml_not_well_formed = stream_error "xml-not-well-formed" ""


let make_result_iq_reply_attrs attrs =
  let to' = Xml.get_attr "to" attrs in
  let from = Xml.get_attr "from" attrs in
  let attrs = List.remove_assoc "to" attrs in
  let attrs = List.remove_assoc "from" attrs in
  let attrs =
    match to' with
      | Some to_val ->
	  ("from", to_val) :: attrs;
      | _ ->
	  attrs
  in
  let attrs =
    match from with
      | Some from_val ->
	  ("to", from_val) :: attrs
      | _ ->
	  attrs
  in
  let attrs = List.remove_assoc "type" attrs in
  let attrs = ("type", "result") :: attrs in
    attrs

let make_result_iq_reply (`XmlElement (name, attrs, subtags)) =
  let attrs = make_result_iq_reply_attrs attrs in
    `XmlElement (name, attrs, subtags)


let make_error_reply_attrs attrs =
  let to' = Xml.get_attr "to" attrs in
  let from = Xml.get_attr "from" attrs in
  let attrs = List.remove_assoc "to" attrs in
  let attrs = List.remove_assoc "from" attrs in
  let attrs =
    match to' with
      | Some to_val ->
	  ("from", to_val) :: attrs
      | _ ->
	  attrs
  in
  let attrs =
    match from with
      | Some from_val ->
	  ("to", from_val) :: attrs
      | _ ->
	  attrs
  in
  let attrs = List.remove_assoc "type" attrs in
  let attrs = ("type", "error") :: attrs in
    attrs

let make_error_reply (`XmlElement (name, attrs, subtags)) error =
  let new_attrs = make_error_reply_attrs attrs in
    `XmlElement (name, new_attrs, subtags @ [(error :> Xml.element_cdata)])

let replace_from_to_attrs from to' attrs =
  let attrs = List.remove_assoc "to" attrs in
  let attrs = List.remove_assoc "from" attrs in
  let attrs = ("to", to') :: attrs in
  let attrs = ("from", from) :: attrs in
    attrs

let replace_from_to from to' (`XmlElement (name, attrs, els)) =
  let attrs =
    replace_from_to_attrs
      (jid_to_string from)
      (jid_to_string to')
      attrs
  in
    `XmlElement (name, attrs, els)

let remove_attr attr (`XmlElement (name, attrs, els)) =
  let attrs = List.remove_assoc attr attrs in
    `XmlElement (name, attrs, els)


type iq_query =
    [ `Get of Xml.element
    |  `Set of Xml.element ]
type iq_response =
    [ `Result of Xml.element option
    | `Error of Xml.element * Xml.element option ]
type iq_query_response = [ iq_query | iq_response ]

type +'a iq =
    {iq_id : string;
     iq_type : [< iq_query_response ] as 'a;
     iq_xmlns : string;
     iq_lang : string}

let iq_query_or_response_info el =
  match el with
    | `XmlElement ("iq", attrs, els) -> (
	let id = Xml.get_attr_s "id" attrs in
	let type' = Xml.get_attr_s "type" attrs in
	let lang = Xml.get_attr_s "xml:lang" attrs in
	let type' =
	  match type' with
	    | "set" -> `Set
	    | "get" -> `Get
	    | "result" -> `Result
	    | "error" -> `Error
	    | _ -> `Invalid
	in
	  match type' with
	    | `Invalid -> `Invalid
	    | (`Set | `Get) as type' -> (
		let filtered_els = Xml.remove_cdata els in
		  match filtered_els with
		    | [`XmlElement (_name, attrs, _els) as e] ->
			let xmlns = Xml.get_attr_s "xmlns" attrs in
			  if xmlns = ""
			  then `Invalid
			  else
			    let type' =
			      match type' with
				| `Set -> `Set e
				| `Get -> `Get e
			    in
			      `IQ {iq_id = id;
				   iq_type = type';
				   iq_xmlns = xmlns;
				   iq_lang = lang}
		    | _ -> `Invalid
	      )
	    | (`Result | `Error) as type' ->
		let filtered_els = Xml.remove_cdata els in
		let not_error (`XmlElement (name, _, _)) =
		  name <> "error"
		in
		  match List.partition not_error filtered_els with
		    | ([`XmlElement _ as non_error_el], error_els) -> (
			let xmlns = Xml.get_tag_attr_s "xmlns" non_error_el
			in
			  match type', error_els with
			    | `Result, [] ->
				`IQ {iq_id = id;
				     iq_type = `Result (Some non_error_el);
				     iq_xmlns = xmlns;
				     iq_lang = lang}
			    | `Error, [error_el] ->
				`IQ {iq_id = id;
				     iq_type = `Error (error_el,
						       Some non_error_el);
				     iq_xmlns = xmlns;
				     iq_lang = lang}
			    | _, _ -> `Invalid
		      )
		    | ([], error_els) -> (
			match type', error_els with
			  | `Result, [] ->
			      `IQ {iq_id = id;
				   iq_type = `Result None;
				   iq_xmlns = "";
				   iq_lang = lang}
			  | `Error, [error_el] ->
			      `IQ {iq_id = id;
				   iq_type = `Error (error_el, None);
				   iq_xmlns = "";
				   iq_lang = lang}
			  | _, _ -> `Invalid
		      )
		    | _ ->
			`Invalid
      )
    | _ -> `Not_iq

let iq_query_info el =
  match iq_query_or_response_info el with
    | `IQ {iq_type = `Set _ | `Get _; _} as res -> res
    | `IQ {iq_type = `Result _ | `Error _; _} -> `Reply
    | `Invalid -> `Invalid
    | `Not_iq -> `Not_iq

(*
  is_iq_request_type(set) -> true;
  is_iq_request_type(get) -> true;
  is_iq_request_type(_) -> false.
*)


let iq_to_xml {iq_id = id; iq_type = type'; _} =
  let (stype, els) =
    match type' with
      | `Set el -> ("set", [el])
      | `Get el -> ("get", [el])
      | `Result None -> ("result", [])
      | `Result (Some el) -> ("result", [el])
      | `Error (error_el, None) -> ("error", [error_el])
      | `Error (error_el, Some el) -> ("error", [el; error_el])
  in
    (`XmlElement
       ("iq",
	[("id", id); ("type", stype)],
	els) :> Xml.element)

let rec parse_xdata_values els' res =
  match els' with
    | [] -> res
    | (`XmlElement ("value", _attrs, subels)) :: els ->
	let val' = Xml.get_cdata subels in
	  parse_xdata_values els (val' :: res)
    | _ :: els ->
	parse_xdata_values els res

let rec parse_xdata_fields els' res =
  match els' with
    | [] -> res
    | (`XmlElement ("field", attrs, subels)) :: els -> (
	match Xml.get_attr_s "var" attrs with
	  | "" ->
	      parse_xdata_fields els res
	  | var ->
	      let field = (var, List.rev (parse_xdata_values subels [])) in
		parse_xdata_fields els (field :: res)
      )
    | _ :: els ->
	parse_xdata_fields els res

let parse_xdata_submit el =
  match el with
    | `XmlElement (_name, attrs, els) -> (
	match Xml.get_attr_s "type" attrs with
	  | "form" (* This is a workaround to accept Psi's wrong forms *)
	  | "submit" ->
	      List.rev (parse_xdata_fields els [])
	  | _ ->
	      []
      )
    | _ ->
	[]

let sha1 s =
  let h = Cryptokit.Hash.sha1 () in
  let res = Cryptokit.hash_string h s in
  let t = Cryptokit.Hexa.encode () in
    Cryptokit.transform_string t res

let md5 s =
  let h = Cryptokit.Hash.md5 () in
    Cryptokit.hash_string h s

let decode_base64 s =
  let t = Cryptokit.Base64.decode () in
    Cryptokit.transform_string t s

let encode_base64 s =
  let t = Cryptokit.Base64.encode_multiline () in
  let res = Cryptokit.transform_string t s in
  let buf = Buffer.create (String.length res) in
  String.iter (function
    | '\n' | '\r' | '\t' | ' ' -> ()
    | c -> Buffer.add_char buf c)
    res;
  Buffer.contents buf

let get_random_string () =		(* TODO *)
  string_of_int (Random.int 1000000000)

type timezone = | UTC
		| Shift of int * int

let get_tzo () =
  let tfloat = Unix.time () in
  let tm_local = Unix.localtime tfloat in
  let tm_utc = Unix.gmtime tfloat in
  let time_f_utc, _ = Unix.mktime tm_utc in
  let time_f_local, _ = Unix.mktime tm_local in
  let sec_diff = int_of_float (time_f_local -. time_f_utc) in
  let div = sec_diff / 3600 in
  let rem = abs(sec_diff) mod 3600 in
    Shift (div, rem)

let timestamp_to_iso' tm =
  Printf.sprintf "%04d%02d%02dT%02d:%02d:%02d"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let timestamp_to_iso tfloat timezone =
  let tm_utc = Unix.gmtime tfloat in
  let utc = Printf.sprintf
    "%04d-%02d-%02dT%02d:%02d:%02d"
    (tm_utc.Unix.tm_year + 1900) (tm_utc.Unix.tm_mon + 1)
    tm_utc.Unix.tm_mday tm_utc.Unix.tm_hour
    tm_utc.Unix.tm_min tm_utc.Unix.tm_sec in
  let tzo = match timezone with
    | UTC ->
	"Z"
    | Shift (tzh, tzm) ->
	Printf.sprintf "%+02d:%02d" tzh tzm
  in
    (utc, tzo)

let timestamp_to_xml tfloat tz from_jid desc =
  let (t_string, tz_string) = timestamp_to_iso tfloat tz in
  let text = [`XmlCdata desc] in
  let from = jid_to_string from_jid in
    `XmlElement ("delay", [("xmlns", [%ns:DELAY]);
			   ("from", from);
			   ("stamp", t_string ^ tz_string)],
		 text)

(* TODO: Remove this function once XEP-0091 is Obsolete *)
let timestamp_to_xml' tm =
  let stamp = Printf.sprintf
    "%04d%02d%02dT%02d:%02d:%02d"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday tm.Unix.tm_hour
    tm.Unix.tm_min tm.Unix.tm_sec in
    `XmlElement ("x", [("xmlns", [%ns:DELAY91]);
		       ("stamp", stamp)], [])

let uptime () = (Unix.time ()) -. start_time

module LJID :
sig
  type t = nodepreped * namepreped * resourcepreped
  val compare : t -> t -> int
  val to_string : t -> string
end
  =
struct
  type t = nodepreped * namepreped * resourcepreped
  let compare = compare
  let to_string (u, s, r) = jid_to_string' u s r
end

module LJIDSet =
struct
  include Set.Make(LJID)

  let from_list xs =
    List.fold_left
      (fun s x -> add x s) empty xs
end
