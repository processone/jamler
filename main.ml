type -'a pid

type 'a proc = {id : int;
		queue : 'a Queue.t;
		mutable wakener : 'a Lwt.u option}

external pid_to_proc : 'a pid -> 'a proc = "%identity"
external proc_to_pid : 'a proc -> 'a pid = "%identity"

let id_seq = ref 0

let spawn f =
  let proc = {id = (incr id_seq; !id_seq);
	      queue = Queue.create ();
	      wakener = None}
  in
  let pid = proc_to_pid proc in
  let _ =
    try_lwt
      f pid
    with
      | exn ->
	  lwt () =
            Lwt_io.eprintf "Process raised an exception: %s\n"
	      (Printexc.to_string exn)
	  in
            Lwt.fail exn
  in
    pid

exception Queue_limit

let send pid msg =
  let proc = pid_to_proc pid in
    (match proc.wakener with
       | None ->
	   if Queue.length proc.queue > 10000
	   then raise Queue_limit;
	   Queue.add msg proc.queue
       | Some wakener ->
	   Lwt.wakeup wakener msg
    )

let ($!) = send

let receive pid =
  let proc = pid_to_proc pid in
    if Queue.is_empty proc.queue then (
      let (waiter, wakener) = Lwt.wait () in
	proc.wakener <- Some wakener;
	lwt msg = waiter in
          proc.wakener <- None;
          Lwt.return msg
    ) else (
      Lwt.return (Queue.take proc.queue)
    )

let (exit_waiter, exit_wakener) = Lwt.wait ()

module Tcp =
struct
  type socket = {fd : Lwt_unix.file_descr;
		 pid : msg pid;
		 mutable writer : unit Lwt.u option;
		 mutable buffer : Buffer.t;
		 mutable buffer_limit : int;
		 mutable waiters : unit Lwt.u list;
		 mutable timeout : float;
		}

  and msg =
      [ `Tcp_data of socket * string
      | `Tcp_close of socket ]

  let rec writer socket =
    let rec write socket str pos len =
      lwt n = Lwt_unix.write socket.fd str pos len in
        if len = n
	then Lwt.return ()
	else write socket str (pos + n) (len - n)
    in
    let len = Buffer.length socket.buffer in
      if len > 0 then (
	let data = Buffer.contents socket.buffer in
	  Buffer.reset socket.buffer;
	  lwt () =
	    try_lwt
	      write socket data 0 len
            with
	      | exn ->
	          lwt () = Lwt_unix.close socket.fd in
		  lwt () =
                    Lwt_io.eprintf "Writer raised exception: %s\n"
		      (Printexc.to_string exn)
                  in
		  let senders = socket.waiters in
                    socket.waiters <- [];
	            List.iter (fun w -> Lwt.wakeup_exn w exn) senders;
		    socket.pid $! `Tcp_close socket;
                    Lwt.fail exn
          in
	    writer socket
      ) else (
	let senders = socket.waiters in
	  socket.waiters <- [];
	  List.iter (fun w -> Lwt.wakeup w ()) senders;
	  if Buffer.length socket.buffer = 0 then (
	    let waiter, wakener = Lwt.wait () in
	      socket.writer <- Some wakener;
	      lwt () = waiter in
	        socket.writer <- None;
	        writer socket
          ) else writer socket
      )

  let of_fd fd pid =
    let socket =
      {fd;
       pid = (pid :> msg pid);
       writer = None;
       buffer = Buffer.create 100;
       buffer_limit = -1;
       waiters = [];
       timeout = -1.0;
      }
    in
      ignore (writer socket);
      socket

  let set_timeout socket t =
    socket.timeout <- t

  let set_buffer_limit socket limit =
    socket.buffer_limit <- limit

  let close' socket =
    ignore (Lwt_unix.close socket.fd);
    Buffer.reset socket.buffer;
    socket.pid $! `Tcp_close socket

  let close socket =
    lwt () = Lwt_unix.close socket.fd in
      Buffer.reset socket.buffer;
      Lwt.return ()

  let buf_size = 4096
  let buf = String.make buf_size '\000'

  let activate socket pid =
    ignore (
      lwt len = Lwt_unix.read socket.fd buf 0 buf_size in
        if len > 0 then (
	  let data = String.sub buf 0 len in
	    pid $! `Tcp_data (socket, data)
	) else (
	  close' socket
	);
        Lwt.return ()
    )

  let state socket = Lwt_unix.state socket.fd

  exception Closed

  let send' socket data =
    if Lwt_unix.state socket.fd <> Lwt_unix.Opened
    then raise Closed
    else (
      match socket.writer with
	| None ->
	    Buffer.add_string socket.buffer data
	| Some writer ->
	    Buffer.add_string socket.buffer data;
	    Lwt.wakeup writer ()
    )

  let send socket data =
    let waiter, wakener = Lwt.wait () in
      socket.waiters <- wakener :: socket.waiters;
      if socket.timeout <= 0.0 then (
	send' socket data;
	waiter
      ) else (
	Lwt_unix.with_timeout socket.timeout
	  (fun () ->
	     send' socket data;
	     waiter
	  )
      )

  let send_async socket data =
    if socket.buffer_limit >= 0 &&
      socket.buffer_limit < Buffer.length socket.buffer
    then (
      close' socket
    );
    ignore (
      try_lwt
	send socket data
      with
	| Lwt_unix.Timeout as exn ->
	    close' socket;
	    Lwt.fail exn
    )

end

module XMLReceiver =
struct
  type msg =
      [ `XmlStreamStart of Xml.name * Xml.attribute list
      | `XmlStreamElement of Xml.element
      | `XmlStreamEnd of Xml.name
      | `XmlStreamError of string
      ]

  type t = {pid : msg pid;
	    mutable xml_parser : Xml.t}

  let create pid =
    let pid = (pid :> msg pid) in
    let element_callback el =
      pid $! `XmlStreamElement el
    in
    let start_callback name attrs =
      pid $! `XmlStreamStart (name, attrs)
    in
    let end_callback name =
      pid $! `XmlStreamEnd name
    in
    let xml_parser =
      Xml.create_parser
	~depth:1
	~element_callback
	~start_callback
	~end_callback
	()
    in
      {pid;
       xml_parser}

  let parse st data =
    try
      Xml.parse st.xml_parser data false
    with
      | Expat.Parse_error error ->
	  st.pid $! `XmlStreamError error

  let free st =
    Expat.parser_free st.xml_parser

  let reset_stream st =
    Expat.parser_free st.xml_parser;
    let pid = st.pid in
    let element_callback el =
      pid $! `XmlStreamElement el
    in
    let start_callback name attrs =
      pid $! `XmlStreamStart (name, attrs)
    in
    let end_callback name =
      pid $! `XmlStreamEnd name
    in
    let xml_parser =
      Xml.create_parser
	~depth:1
	~element_callback
	~start_callback
	~end_callback
	()
    in
      st.xml_parser <- xml_parser

end


module GenServer =
struct
  type msg = [ `System ]
  type 'a result =
      [ `Continue of 'a
      | `Stop of 'a
      ] Lwt.t

  module type Type =
  sig
    type msg
    type state
    type init_data
    val init : init_data -> msg pid -> state
    val handle : msg -> state -> state result
    val terminate : state -> unit Lwt.t
  end

  module type S =
  sig
    type msg
    type init_data
    val start : init_data -> msg pid
  end

  module Make (T : Type with type msg = private [> msg]) :
  sig
    type msg = [ `System ]
    type init_data = T.init_data
    val start : init_data -> T.msg pid
  end =
  struct
    type msg = [ `System ]
    type init_data = T.init_data
    let start init_data =
      let rec loop self state =
	lwt msg = receive self in
          match msg with
	    | #msg ->
		loop self state
	    | m ->
		lwt result =
	          try_lwt
		    T.handle m state
		  with
		    | exn ->
			lwt () =
                          Lwt_io.eprintf "GenServer raised an exception: %s\n"
			    (Printexc.to_string exn)
                        in
                          Lwt.return (`Stop state)
                in
		  match result with
		    | `Continue state ->
			loop self state
		    | `Stop state ->
			T.terminate state
      in
        spawn (fun self ->
		 let state = T.init init_data self in
		   loop self state)
  end
end

module Jlib :
sig
  type namepreped = private string
  type nodepreped = private string
  type resourcepreped = private string

  type jid = {user : string;
	      server : string;
	      resource : string;
	      luser : nodepreped;
	      lserver : namepreped;
	      lresource : resourcepreped;
	     }

  val nameprep_exn : string -> namepreped
  val nodeprep_exn : string -> nodepreped
  val resourceprep_exn : string -> resourcepreped

  val nameprep : string -> namepreped option
  val nodeprep : string -> nodepreped option
  val resourceprep : string -> resourcepreped option

  exception Bad_jid

  val make_jid_exn : string -> string -> string -> jid
  val make_jid : string -> string -> string -> jid option
  val make_jid' : nodepreped -> namepreped -> resourcepreped -> jid
  val ljid_to_jid : nodepreped * namepreped * resourcepreped -> jid
  val string_to_jid_exn : string -> jid
  val string_to_jid : string -> jid option
  val jid_to_string : jid -> string
  val jid_to_string' : string -> string -> string -> string
  val jid_tolower : jid -> nodepreped * namepreped * resourcepreped
  val jid_remove_resource : jid -> jid
  val jid_replace_resource' : jid -> resourcepreped -> jid

  val err_bad_request : Xml.element
  val err_conflict : Xml.element
  val err_feature_not_implemented : Xml.element
  val err_forbidden : Xml.element
  val err_gone : Xml.element
  val err_internal_server_error : Xml.element
  val err_item_not_found : Xml.element
  val err_jid_malformed : Xml.element
  val err_not_acceptable : Xml.element
  val err_not_allowed : Xml.element
  val err_not_authorized : Xml.element
  val err_payment_required : Xml.element
  val err_recipient_unavailable : Xml.element
  val err_redirect : Xml.element
  val err_registration_required : Xml.element
  val err_remote_server_not_found : Xml.element
  val err_remote_server_timeout : Xml.element
  val err_resource_constraint : Xml.element
  val err_service_unavailable : Xml.element
  val err_subscription_required : Xml.element
  val err_unexpected_request : Xml.element
  val err_unexpected_request_cancel : Xml.element

  val errt_bad_request : string -> string -> Xml.element
  val errt_conflict : string -> string -> Xml.element
  val errt_feature_not_implemented : string -> string -> Xml.element
  val errt_forbidden : string -> string -> Xml.element
  val errt_gone : string -> string -> Xml.element
  val errt_internal_server_error : string -> string -> Xml.element
  val errt_item_not_found : string -> string -> Xml.element
  val errt_jid_malformed : string -> string -> Xml.element
  val errt_not_acceptable : string -> string -> Xml.element
  val errt_not_allowed : string -> string -> Xml.element
  val errt_not_authorized : string -> string -> Xml.element
  val errt_payment_required : string -> string -> Xml.element
  val errt_recipient_unavailable : string -> string -> Xml.element
  val errt_redirect : string -> string -> Xml.element
  val errt_registration_required : string -> string -> Xml.element
  val errt_remote_server_not_found : string -> string -> Xml.element
  val errt_remote_server_timeout : string -> string -> Xml.element
  val errt_resource_constraint : string -> string -> Xml.element
  val errt_service_unavailable : string -> string -> Xml.element
  val errt_subscription_required : string -> string -> Xml.element
  val errt_unexpected_request : string -> string -> Xml.element

  (* Auth stanza errors *)
  val err_auth_no_resource_provided : string -> Xml.element
  val err_auth_bad_resource_format : string -> Xml.element
  val err_auth_resource_conflict : string -> Xml.element

  val serr_bad_format : Xml.element
  val serr_bad_namespace_prefix : Xml.element
  val serr_conflict : Xml.element
  val serr_connection_timeout : Xml.element
  val serr_host_gone : Xml.element
  val serr_host_unknown : Xml.element
  val serr_improper_addressing : Xml.element
  val serr_internal_server_error : Xml.element
  val serr_invalid_from : Xml.element
  val serr_invalid_id : Xml.element
  val serr_invalid_namespace : Xml.element
  val serr_invalid_xml : Xml.element
  val serr_not_authorized : Xml.element
  val serr_policy_violation : Xml.element
  val serr_remote_connection_failed : Xml.element
  val serr_resourse_constraint : Xml.element
  val serr_restricted_xml : Xml.element
  val serr_see_other_host : string -> Xml.element
  val serr_system_shutdown : Xml.element
  val serr_unsupported_encoding : Xml.element
  val serr_unsupported_stanza_type : Xml.element
  val serr_unsupported_version : Xml.element
  val serr_xml_not_well_formed : Xml.element

  type iq_query =
      [ `Get of Xml.element
      | `Set of Xml.element ]
  type iq_response =
      [ `Result of Xml.element option
      | `Error of Xml.element * Xml.element option ]
  type iq_query_response = [ iq_query | iq_response ]

  type +'a iq =
      {iq_id : string;
       iq_type : [< iq_query_response ] as 'a;
       iq_xmlns : string;
       iq_lang : string}

  val iq_query_or_response_info : Xml.element ->
    [ `IQ of iq_query_response iq
    | `Invalid
    | `Not_iq
    | `Reply ]

  val iq_query_info : Xml.element ->
    [ `IQ of iq_query iq
    | `Invalid
    | `Not_iq
    | `Reply ]

  val make_result_iq_reply : Xml.element -> Xml.element
  val make_error_reply : Xml.element -> Xml.element -> Xml.element

  val replace_from_to_attrs :
    string -> string -> Xml.attributes -> Xml.attributes
  val replace_from_to : jid -> jid -> Xml.element -> Xml.element
  val remove_attr : string -> Xml.element -> Xml.element

  val iq_to_xml : iq_query_response iq -> Xml.element

  val sha1 : string -> string
  val md5 : string -> string

  val decode_base64 : string -> string
  val encode_base64 : string -> string

  val get_random_string : unit -> string
end
  =
struct
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
       [`XmlElement (condition, [("xmlns", <:ns<STANZAS>>)], [])])

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
       [`XmlElement (condition, [("xmlns", <:ns<STANZAS>>)], []);
	`XmlElement ("text", [("xmlns", <:ns<STANZAS>>)],
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
       [`XmlElement (condition, [("xmlns", <:ns<STREAMS>>)],
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

  type 'a iq =
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


  let sha1 s =
    let h = Cryptokit.Hash.sha1 () in
      Cryptokit.hash_string h s

  let md5 s =
    let h = Cryptokit.Hash.md5 () in
      Cryptokit.hash_string h s

  let decode_base64 s =
    let t = Cryptokit.Base64.decode () in
      Cryptokit.transform_string t s

  let encode_base64 s =
    let t = Cryptokit.Base64.encode_compact () in
      Cryptokit.transform_string t s

  let get_random_string () =		(* TODO *)
    string_of_int (Random.int 1000000000)

end

module LJID =
struct
  type t = Jlib.nodepreped * Jlib.namepreped * Jlib.resourcepreped
  let compare = compare
end
module LJIDSet =
struct
  include Set.Make(LJID)

  let from_list xs =
    List.fold_left
      (fun s x -> add x s) empty xs
end


let myhosts () =
  List.map Jlib.nameprep_exn ["localhost"; "e.localhost"] (* TODO *)


module Hooks :
sig
  type result =
    | Stop
    | OK
  type 'a hook

  val create : unit -> 'a hook
  val add : 'a hook -> Jlib.namepreped -> ('a -> result) -> int -> unit
  val run : 'a hook -> Jlib.namepreped -> 'a -> unit

  type ('a, 'b) fold_hook

  val create_fold : unit -> ('a, 'b) fold_hook
  val add_fold :
    ('a, 'b) fold_hook -> Jlib.namepreped -> ('b -> 'a -> result * 'b) ->
    int -> unit
  val run_fold : ('a, 'b) fold_hook -> Jlib.namepreped -> 'b -> 'a -> 'b
end
  =
struct
  type result =
    | Stop
    | OK
  type 'a hook = (Jlib.namepreped, (int * ('a -> result)) list) Hashtbl.t

  let create () = Hashtbl.create 1

  let add hook host f seq =
    let h =
      try
	Hashtbl.find hook host
      with
	| Not_found -> []
    in
    let h = List.sort compare ((seq, f) :: h) in
      Hashtbl.replace hook host h

  let run hook host x =
    let rec aux x =
      function
	| (_, f) :: h ->
	    (try
	       match f x with
		 | OK -> aux x h
		 | Stop -> ()
	     with
	       | exn ->
		   Printf.printf "Exception %s\nrunning hook"
		     (Printexc.to_string exn);
		   aux x h
	    )
	| [] -> ()
    in
      try
	let h = Hashtbl.find hook host in
	  aux x h
      with
	| Not_found -> ()


  type ('a, 'b) fold_hook =
      (Jlib.namepreped, (int * ('b -> 'a -> result * 'b)) list) Hashtbl.t

  let create_fold () = Hashtbl.create 1

  let add_fold hook host f seq =
    let h =
      try
	Hashtbl.find hook host
      with
	| Not_found -> []
    in
    let h = List.sort compare ((seq, f) :: h) in
      Hashtbl.replace hook host h

  let run_fold hook host v x =
    let rec aux v x =
      function
	| (_, f) :: h ->
	    (try
	       match f v x with
		 | (OK, v) -> aux v x h
		 | (Stop, v) -> v
	     with
	       | exn ->
		   Printf.printf "Exception %s\nrunning hook"
		     (Printexc.to_string exn);
		   aux v x h
	    )
	| [] -> v
    in
      try
	let h = Hashtbl.find hook host in
	  aux v x h
      with
	| Not_found -> v

end

module Auth :
sig
  val check_password_with_authmodule :
    Jlib.nodepreped -> Jlib.namepreped -> string -> string option
  val check_password_digest_with_authmodule :
    Jlib.nodepreped -> Jlib.namepreped ->
    string -> string -> (string -> string) -> string option
  val get_password_with_authmodule :
    Jlib.nodepreped -> Jlib.namepreped -> (string * string) option

  val does_user_exist : Jlib.nodepreped -> Jlib.namepreped -> bool
end
  =
struct
  let check_password_with_authmodule _user _server _password =
    Some "none"

  let check_password_digest_with_authmodule _user _server _password
      _digest _digest_gen =
    Some "none"

  let get_password_with_authmodule _user _server =
    Some ("test", "none")

  let does_user_exist _user _server =
    true
end


module SASL =
struct
  type get_password = Jlib.nodepreped -> (string * string) option
  type check_password = Jlib.nodepreped -> string -> string option
  type check_password_digest =
      Jlib.nodepreped -> string -> string -> (string -> string) -> string option

  type props = ([ `Username | `Auth_module | `Authzid ] * string) list

  type step_result =
    | Done of props
    | Continue of string * t
    | ErrorUser of string * string
    | Error of string
  and t = string -> step_result

  module type SASLMechanism =
  sig
    val mech_new : Jlib.namepreped -> get_password ->
      check_password -> check_password_digest -> string -> step_result
  end

  let mechanisms : (string, (module SASLMechanism)) Hashtbl.t =
    Hashtbl.create 10

  let register_mechanism mech_name mech_mod =
    Hashtbl.replace mechanisms mech_name mech_mod

  let listmech _server =
    Hashtbl.fold (fun name _ acc -> name :: acc) mechanisms []

  let check_credentials props =
    try
      let user = List.assoc `Username props in
	match (Jlib.nodeprep user :> string option) with
	  | Some ""
	  | None -> Error "not-authorized"
	  | _ -> Done props
    with
      | Not_found ->
	  Error "not-authorized"

  let rec process_mech_result =
    function
      | Done props ->
	  check_credentials props
      | Continue (server_out, f) ->
	  Continue (server_out, fun s -> process_mech_result (f s))
      | (ErrorUser _ | Error _) as error -> error

  let server_start ~service:_service ~server_fqdn ~user_realm:_user_realm
      ~get_password ~check_password ~check_password_digest ~mech client_in =
    try
      let mech_mod = Hashtbl.find mechanisms mech in
      let module Mech = (val mech_mod : SASLMechanism) in
      let mech =
	Mech.mech_new server_fqdn
	  get_password check_password check_password_digest client_in
      in
	process_mech_result mech
    with
      | Not_found ->
	  Error "no-mechanism"

  let server_step f client_in =
    process_mech_result (f client_in)

end

module SASLPlain =
struct
  let tokenize ?(remove_empty = false) c str =
    let rec aux str from res =
      if from >= String.length str
      then List.rev res
      else 
	try
	  let idx = String.index_from str from c in
	  let token = String.sub str from (idx - from) in
	    match token with
	      | "" when remove_empty -> aux str (idx + 1) res
	      | _ -> aux str (idx + 1) (token :: res)
	with
	  | Not_found ->
	      let str = String.sub str from (String.length str - from) in
		aux "" 0 (str :: res)
    in
      aux str 0 []

  let parse s = tokenize '\000' s
  let parse_domain s = tokenize '@' s

  let prepare client_in =
    match parse client_in with
      | [""; user_maybe_domain; password] -> (
	  match parse_domain user_maybe_domain with
	    | [user; _domain] ->
		(* <NUL>login@domain<NUL>pwd *)
		Some (user_maybe_domain, user, password);
	    | [user] ->
		(* <NUL>login<NUL>pwd *)
		Some ("", user, password)
	    | _ ->
		None
	)
      | [authzid; user; password] ->
	  (* login@domain<NUL>login<NUL>pwd *)
	  Some (authzid, user, password)
      | _ ->
	  None


  let mech_new _host _get_password check_password _check_password_digest
      client_in =
    match prepare client_in with
      | Some (authzid, user, password) -> (
	  match Jlib.nodeprep user with
	    | Some user' -> (
		match check_password user' password with
		  | Some auth_module ->
		      SASL.Done [(`Username, user);
				 (`Authzid, authzid);
				 (`Auth_module, auth_module)]
		  | _ ->
		      SASL.ErrorUser ("not-authorized", user)
	      )
	    | None ->
		SASL.ErrorUser ("not-authorized", user)
	)
      | None ->
	  SASL.Error "bad-protocol"

end

let _ =
  SASL.register_mechanism "PLAIN" (module SASLPlain : SASL.SASLMechanism)


module SASLDigest =
struct
  let tokenize ?(remove_empty = false) c str =
    let rec aux str from res =
      if from >= String.length str
      then List.rev res
      else 
	try
	  let idx = String.index_from str from c in
	  let token = String.sub str from (idx - from) in
	    match token with
	      | "" when remove_empty -> aux str (idx + 1) res
	      | _ -> aux str (idx + 1) (token :: res)
	with
	  | Not_found ->
	      let str = String.sub str from (String.length str - from) in
		aux "" 0 (str :: res)
    in
      aux str 0 []

  type step = One | Three | Five

  type state =
      {step : step;
       nonce : string;
       username : string;
       authzid : string;
       get_password : SASL.get_password;
       check_password_digest : SASL.check_password_digest;
       auth_module : string;
       host : Jlib.namepreped;
      }

  let get_assoc_s = Xml.get_attr_s

  let parse s =
    let rec parse1 s i k ts =
      if i < String.length s then (
	match s.[i] with
	  | '=' ->
	      parse2 s (i + 1) (String.sub s k (i - k)) ts
	  | ' '
	  | ',' when i = k ->
	      parse1 s (i + 1) (i + 1) ts
	  | _c ->
	      parse1 s (i + 1) k ts
      ) else (
	if i = k
	then Some (List.rev ts)
	else None
      )
    and parse2 s i key ts =
      if i < String.length s then (
	match s.[i] with
	  | '"' ->
	      parse3 s (i + 1) key (Buffer.create 10) ts
	  | c ->
	      let v = Buffer.create 10 in
		Buffer.add_char v c;
		parse4 s (i + 1) key v ts
      ) else None
    and parse3 s i key v ts =
      if i < String.length s then (
	match s.[i] with
	  | '"' ->
	      parse4 s (i + 1) key v ts
	  | '\\' when i < String.length s - 1 ->
	      Buffer.add_char v s.[i + 1];
	      parse3 s (i + 2) key v ts
	  | c ->
	      Buffer.add_char v c;
	      parse3 s (i + 1) key v ts
      ) else None
    and parse4 s i key v ts =
      if i < String.length s then (
	match s.[i] with
	  | ',' ->
	      parse1 s (i + 1) (i + 1) ((key, Buffer.contents v) :: ts)
	  | ' ' ->
	      parse4 s (i + 1) key v ts
	  | c ->
	      Buffer.add_char v c;
	      parse4 s (i + 1) key v ts
      ) else parse1 s i i ((key, Buffer.contents v) :: ts)
    in
      parse1 s 0 0 []


  (*
    @doc Check if the digest-uri is valid.
    RFC-2831 allows to provide the IP address in Host,
    however ejabberd doesn't allow that.
    If the service (for example jabber.example.org)
    is provided by several hosts (being one of them server3.example.org),
    then digest-uri can be like xmpp/server3.example.org/jabber.example.org
    In that case, ejabberd only checks the service name, not the host.
  *)
  let is_digesturi_valid digest_uri_case jabber_host =
    let digest_uri = Stringprep.tolower digest_uri_case in
      match tokenize '/' digest_uri with
	| ["xmpp"; host] when host = jabber_host ->
	    true
	| ["xmpp"; _host; servname] when servname = jabber_host ->
	    true
	| _ ->
	    false


  let hex s =
    let t = Cryptokit.Hexa.encode () in
      Cryptokit.transform_string t s

  let response key_vals user passwd nonce authzid a2prefix =
    let realm = get_assoc_s "realm" key_vals in
    let cnonce = get_assoc_s "cnonce" key_vals in
    let digest_uri = get_assoc_s "digest-uri" key_vals in
    let nc = get_assoc_s "nc" key_vals in
    let qop = get_assoc_s "qop" key_vals in
    let a1 =
      match authzid with
	| "" ->
	    Jlib.md5 (user ^ ":" ^ realm ^ ":" ^ passwd) ^
	      ":" ^ nonce ^ ":" ^ cnonce
	| _ ->
	    Jlib.md5 (user ^ ":" ^ realm ^ ":" ^ passwd) ^
	      ":" ^ nonce ^ ":" ^ cnonce ^ ":" ^ authzid
    in
    let a2 =
      match qop with
	| "auth" ->
	    a2prefix ^ ":" ^ digest_uri;
	| _ ->
	    a2prefix ^ ":" ^ digest_uri ^
	      ":00000000000000000000000000000000"
    in
    let t =
      hex (Jlib.md5 a1) ^ ":" ^ nonce ^ ":" ^
	nc ^ ":" ^ cnonce ^ ":" ^ qop ^ ":" ^
	hex (Jlib.md5 a2)
    in
      hex (Jlib.md5 t)


  let rec mech_step state client_in =
    match state, client_in with
      | {step = One; nonce = nonce; _}, _ ->
	  SASL.Continue
	    ("nonce=\"" ^ nonce ^
	       "\",qop=\"auth\",charset=utf-8,algorithm=md5-sess",
	     mech_step {state with step = Three})
      | {step = Three; nonce = nonce; _}, client_in -> (
	  match parse client_in with
	    | None ->
		SASL.Error "bad-protocol"
	    | Some key_vals -> (
		let digest_uri = get_assoc_s "digest-uri" key_vals in
		let username = get_assoc_s "username" key_vals in
		  match
		    is_digesturi_valid digest_uri (state.host :> string),
		    Jlib.nodeprep username
		  with
		    | true, Some lusername -> (
			let authzid = get_assoc_s "authzid" key_vals in
			  match state.get_password lusername with
			    | None ->
				SASL.ErrorUser ("not-authorized", username)
			    | Some (passwd, auth_module) -> (
				match (state.check_password_digest
					 lusername ""
					 (get_assoc_s "response" key_vals)
					 (fun pw ->
					    response key_vals username pw
					      nonce authzid "AUTHENTICATE"))
				with
				  | Some _ ->
				      let rsp_auth =
					response key_vals
					  username passwd
					  nonce authzid ""
				      in
					SASL.Continue
					  ("rspauth=" ^ rsp_auth,
					   mech_step
					     {state with
						step = Five;
						auth_module;
						username;
						authzid});
				  | None ->
				      SASL.ErrorUser
					("not-authorized", username)
			      )
		      )
		    | _, _ ->
			SASL.ErrorUser ("not-authorized", username)
	      )
	)
      | {step = Five; auth_module; username; authzid; _}, "" ->
	  SASL.Done [(`Username, username);
		     (`Authzid, authzid);
		     (`Auth_module, auth_module)];
      | {step = Five; _}, _ ->
	  SASL.Error "bad-protocol"


  let mech_new host get_password _check_password check_password_digest
      client_in =
    let state =
      {step = One;
       nonce = Jlib.get_random_string ();
       username = "";
       authzid = "";
       auth_module = "";
       host;
       get_password;
       check_password_digest}
    in
      mech_step state client_in



end

let _ =
  SASL.register_mechanism "DIGEST-MD5" (module SASLDigest : SASL.SASLMechanism)


module Router :
sig
  type t = Jlib.jid -> Jlib.jid -> Xml.element -> unit

  type msg = [ `Route of Jlib.jid * Jlib.jid * Xml.element ]

  val route : t

  val register_route :
    ?local_hint : t option -> Jlib.namepreped -> msg pid -> unit
  val unregister_route : Jlib.namepreped -> msg pid -> unit
end
  =
struct
  type t = Jlib.jid -> Jlib.jid -> Xml.element -> unit

  type msg = [ `Route of Jlib.jid * Jlib.jid * Xml.element ]

  type route =
      {pid : msg pid;
       local_hint : t option;
      }

  let route_table = Hashtbl.create 10

  let register_route ?(local_hint = None) domain pid =
	    (*case get_component_number(LDomain) of
		undefined ->*)
    Hashtbl.replace route_table domain {pid; local_hint}
		(*N ->
		    F = fun() ->
				case mnesia:wread({route, LDomain}) of
				    [] ->
					mnesia:write(
					  #route{domain = LDomain,
						 pid = Pid,
						 local_hint = 1}),
					lists:foreach(
					  fun(I) ->
						  mnesia:write(
						    #route{domain = LDomain,
							   pid = undefined,
							   local_hint = I})
					  end, lists:seq(2, N));
				    Rs ->
					lists:any(
					  fun(#route{pid = undefined,
						     local_hint = I} = R) ->
						  mnesia:write(
						    #route{domain = LDomain,
							   pid = Pid,
							   local_hint = I}),
						  mnesia:delete_object(R),
						  true;
					     (_) ->
						  false
					  end, Rs)
				end
			end,
		    mnesia:transaction(F)
	    end*)

  let unregister_route domain _pid =
	    (*case get_component_number(LDomain) of
		undefined ->*)
    Hashtbl.remove route_table domain
		(*_ ->
		    F = fun() ->
				case mnesia:match_object(#route{domain=LDomain,
								pid = Pid,
								_ = '_'}) of
				    [R] ->
					I = R#route.local_hint,
					mnesia:write(
					  #route{domain = LDomain,
						 pid = undefined,
						 local_hint = I}),
					mnesia:delete_object(R);
				    _ ->
					ok
				end
			end,
		    mnesia:transaction(F)
	    end*)


  let s2s_route =
    ref (fun from to' packet ->
	   (* TODO *)
           Printf.eprintf "S2S route stub\nfrom: %s\n to: %s\npacket: %s\n"
	     (Jlib.jid_to_string from)
	     (Jlib.jid_to_string to')
	     (Xml.element_to_string packet); flush stderr;
	   ())

  let do_route orig_from orig_to orig_packet =
           (*Printf.eprintf "Route\nfrom: %s\n to: %s\npacket: %s\n"
	     (Jlib.jid_to_string orig_from)
	     (Jlib.jid_to_string orig_to)
	     (Xml.element_to_string orig_packet); flush stderr;*)
    (*?DEBUG("route~n\tfrom ~p~n\tto ~p~n\tpacket ~p~n",
	   [OrigFrom, OrigTo, OrigPacket]),*)
    match (*ejabberd_hooks:run_fold(filter_packet,
				 {OrigFrom, OrigTo, OrigPacket}, [])*)
      (* TODO *)
      Some (orig_from, orig_to, orig_packet) with
	| Some (from, to', packet) -> (
	    let ldstdomain = to'.Jlib.lserver in
	    let r =
	      try
		Some (Hashtbl.find route_table ldstdomain)
	      with
		| Not_found -> None
	    in
	      match r with
		| None ->
		    !s2s_route from to' packet
		| Some r ->
		    let pid = r.pid in
		    (*if
			node(Pid) == node() ->*)
		      match r.local_hint with
			| Some f ->
				f from to' packet
			| None ->
			    pid $! `Route (from, to', packet)
			(*is_pid(Pid) ->
			    Pid ! {route, From, To, Packet};
			true ->
			    drop
		    end;*)
		(*Rs ->
		    Value = case ejabberd_config:get_local_option(
				   {domain_balancing, LDstDomain}) of
				undefined -> now();
				random -> now();
				source -> jlib:jid_tolower(From);
				destination -> jlib:jid_tolower(To);
				bare_source ->
				    jlib:jid_remove_resource(
				      jlib:jid_tolower(From));
				bare_destination ->
				    jlib:jid_remove_resource(
				      jlib:jid_tolower(To))
			    end,
		    case get_component_number(LDstDomain) of
			undefined ->
			    case [R || R <- Rs, node(R#route.pid) == node()] of
				[] ->
				    R = lists:nth(erlang:phash(Value, length(Rs)), Rs),
				    Pid = R#route.pid,
				    if
					is_pid(Pid) ->
					    Pid ! {route, From, To, Packet};
					true ->
					    drop
				    end;
				LRs ->
				    R = lists:nth(erlang:phash(Value, length(LRs)), LRs),
				    Pid = R#route.pid,
				    case R#route.local_hint of
					{apply, Module, Function} ->
					    Module:Function(From, To, Packet);
					_ ->
					    Pid ! {route, From, To, Packet}
				    end
			    end;
			_ ->
			    SRs = lists:ukeysort(#route.local_hint, Rs),
			    R = lists:nth(erlang:phash(Value, length(SRs)), SRs),
			    Pid = R#route.pid,
			    if
				is_pid(Pid) ->
				    Pid ! {route, From, To, Packet};
				true ->
				    drop
			    end
		    end
		*)
	  )
	| None ->
	    (*?DEBUG("packet dropped~n", []),*)
	    ()




  let route from to' packet =
    try
      do_route from to' packet
    with
      | exn ->
	  (* TODO *)
          Printf.eprintf "Exception %s in Router when processing\nfrom: %s\n to: %s\npacket: %s\n"
	    (Printexc.to_string exn)
	    (Jlib.jid_to_string from)
	    (Jlib.jid_to_string to')
	    (Xml.element_to_string packet); flush stderr;
	  ()
	  (*?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);*)

end

module GenIQHandler :
sig
  type component = [ `SM ]
  type response = [ `IQ of Jlib.iq_response Jlib.iq | `Ignore ]
  val add_iq_handler :
    component -> Jlib.namepreped -> string ->
    (Jlib.jid -> Jlib.jid -> Jlib.iq_query Jlib.iq -> response) -> unit -> unit
  val remove_iq_handler : component -> Jlib.namepreped -> string -> unit
  val handle :
    component -> Jlib.namepreped -> string ->
    Jlib.jid -> Jlib.jid -> Jlib.iq_query Jlib.iq -> bool
end
  =
struct
  type component = [ `SM ]
  type response = [ `IQ of Jlib.iq_response Jlib.iq | `Ignore ]

  let handlers =
    (Hashtbl.create 10
       : (component * Jlib.namepreped * string,
	  Jlib.jid -> Jlib.jid -> Jlib.iq_query Jlib.iq -> response) Hashtbl.t)

  let add_iq_handler component host ns f _type' =
    (*case Type of
	no_queue ->*)
    Hashtbl.replace handlers (component, host, ns) f
	(*one_queue ->
	    {ok, Pid} = supervisor:start_child(ejabberd_iq_sup,
					       [Host, Module, Function]),
	    Component:register_iq_handler(Host, NS, Module, Function,
					  {one_queue, Pid});
	{queues, N} ->
	    Pids =
		lists:map(
		  fun(_) ->
			  {ok, Pid} = supervisor:start_child(
					ejabberd_iq_sup,
					[Host, Module, Function]),
			  Pid
		  end, lists:seq(1, N)),
	    Component:register_iq_handler(Host, NS, Module, Function,
					  {queues, Pids});
	parallel ->
	    Component:register_iq_handler(Host, NS, Module, Function, parallel)
    end.*)

  let remove_iq_handler component host ns =
    (* TODO *)
    Hashtbl.remove handlers (component, host, ns)

(*
stop_iq_handler(_Module, _Function, Opts) ->
    case Opts of
	{one_queue, Pid} ->
	    gen_server:call(Pid, stop);
	{queues, Pids} ->
	    lists:foreach(fun(Pid) ->
				  catch gen_server:call(Pid, stop)
			  end, Pids);
	_ ->
	    ok
    end.
*)

  let process_iq _host f from to' iq =
    try
      let res_iq = f from to' iq in
	match res_iq with
	  | `IQ (iq : Jlib.iq_response Jlib.iq) ->
	      Router.route to' from
		(Jlib.iq_to_xml (iq :> Jlib.iq_query_response Jlib.iq))
	  | `Ignore ->
	      ()
    with
      | exn ->
	  Printf.eprintf "Exception %s in GenIQHandler when processing\nfrom: %s\n to: %s\npacket: %s\n"
	    (Printexc.to_string exn)
	    (Jlib.jid_to_string from)
	    (Jlib.jid_to_string to')
	    (Xml.element_to_string (Jlib.iq_to_xml (iq :> Jlib.iq_query_response Jlib.iq))); flush stderr;
	  ()

  let handle component host ns from to' iq =
    let f =
      try
	Some (Hashtbl.find handlers (component, host, ns))
      with
	| Not_found -> None
    in
      match f with
	| None -> false
	| Some f -> (
    (*case Opts of
	no_queue ->*)
	    process_iq host f from to' iq;
	    true
	(*{one_queue, Pid} ->
	    Pid ! {process_iq, From, To, IQ};
	{queues, Pids} ->
	    Pid = lists:nth(erlang:phash(now(), length(Pids)), Pids),
	    Pid ! {process_iq, From, To, IQ};
	parallel ->
	    spawn(?MODULE, process_iq, [Host, Module, Function, From, To, IQ]);
	_ ->
	    todo
    end.*)
	  )


end


module SM :
sig
  type msg = Router.msg
  type info = [ `TODO ] list
  type sid = float * msg pid

  val route : Router.t
  val open_session :
    sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
    int -> info -> unit
  val close_session :
    sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped -> unit
  val close_session_unset_presence :
    sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
    string -> unit
  val set_presence :
    sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
    int -> Xml.element -> info -> unit
  val unset_presence :
    sid -> Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped ->
    string -> info -> unit

  val get_user_resources :
    Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped list

  val roster_in_subscription :
    (Jlib.nodepreped * Jlib.namepreped * Jlib.jid *
       [ `Subscribe | `Subscribed | `Unsubscribe | `Unsubscribed ] *
       string, bool)
    Hooks.fold_hook

end
  =
struct
(*
  module OrderedNode =
  struct
    type t = Jlib.nodepreped
    let compare = compare
  end
  module NodeSet = Set.Make(OrderedNode)

  module OrderedName =
  struct
    type t = Jlib.namepreped
    let compare = compare
  end
  module NameSet = Set.Make(OrderedName)

  module OrderedResource =
  struct
    type t = Jlib.resourcepreped
    let compare = compare
  end
  module ResourceSet = Set.Make(OrderedResource)
*)

  type msg = Router.msg
  type info = [ `TODO ] list

  module Session :
  sig
    type sid = float * msg pid

    type session =
	{usr : LJID.t;
	 priority : int;
	 info : info}

    val add : sid -> session -> unit
    val remove : sid -> unit
    val find_exn : sid -> session
    val find : sid -> session option
    val find_sids_by_usr :
      Jlib.nodepreped -> Jlib.namepreped -> Jlib.resourcepreped -> sid list
    val find_sids_by_us : Jlib.nodepreped -> Jlib.namepreped -> sid list

    val dump_tables : unit -> unit
  end
    =
  struct
    type sid = float * msg pid

    module HashedSID =
    struct
      type t = sid
      let equal (t1, p1) (t2, p2) =
	let p1 = pid_to_proc p1
	and p2 = pid_to_proc p2 in
	  t1 = t2 && p1.id = p2.id
      let hash (t, p) =
	let p = pid_to_proc p in
	  Hashtbl.hash t lxor Hashtbl.hash p.id
    end
    module HashtblSID = Hashtbl.Make(HashedSID)

    type session =
	{usr : LJID.t;
	 priority : int;
	 info : info}

    let sessions = HashtblSID.create 100
    let usr_idx = Hashtbl.create 10

    let add_r_idx r_idx r sid =
      let sids =
	try
	  Hashtbl.find r_idx r
	with
	  | Not_found -> []
      in
      let sids = List.filter (fun s -> not (HashedSID.equal s sid)) sids in
	Hashtbl.replace r_idx r (sid :: sids)

    let add_ur_idx ur_idx u r sid =
      let r_idx =
	try
	  Hashtbl.find ur_idx u
	with
	  | Not_found ->
	      let r_idx = Hashtbl.create 1 in
		Hashtbl.add ur_idx u r_idx;
		r_idx
      in
	add_r_idx r_idx r sid

    let add_usr_idx (u, s, r) sid =
      let ur_idx =
	try
	  Hashtbl.find usr_idx s
	with
	  | Not_found ->
	      let ur_idx = Hashtbl.create 10 in
		Hashtbl.add usr_idx s ur_idx;
		ur_idx
      in
	add_ur_idx ur_idx u r sid

    let add sid session =
      HashtblSID.replace sessions sid session;
      add_usr_idx session.usr sid


    let remove_r_idx r_idx r sid =
      let sids =
	try
	  Hashtbl.find r_idx r
	with
	  | Not_found -> []
      in
      let sids = List.filter (fun s -> not (HashedSID.equal s sid)) sids in
	match sids with
	  | [] ->
	      Hashtbl.remove r_idx r
	  | _ ->
	      Hashtbl.replace r_idx r sids

    let remove_ur_idx ur_idx u r sid =
      let r_idx =
	try
	  Hashtbl.find ur_idx u
	with
	  | Not_found -> assert false
      in
	remove_r_idx r_idx r sid;
	if Hashtbl.length r_idx = 0
	then Hashtbl.remove ur_idx u

    let remove_usr_idx (u, s, r) sid =
      let ur_idx =
	try
	  Hashtbl.find usr_idx s
	with
	  | Not_found -> assert false
      in
	remove_ur_idx ur_idx u r sid;
	if Hashtbl.length ur_idx = 0
	then Hashtbl.remove usr_idx s

    let remove sid =
      try
	let session = HashtblSID.find sessions sid in
	  HashtblSID.remove sessions sid;
	  remove_usr_idx session.usr sid
      with
	| Not_found -> ()

    let find_exn sid =
      HashtblSID.find sessions sid

    let find sid =
      try
	Some (find_exn sid)
      with
	| Not_found -> None

    let find_sids_by_usr u s r =
      try
	let ur_idx = Hashtbl.find usr_idx s in
	let r_idx = Hashtbl.find ur_idx u in
	let sids = Hashtbl.find r_idx r in
	  sids
      with
	| Not_found -> []

    let find_sids_by_us u s =
      try
	let ur_idx = Hashtbl.find usr_idx s in
	let r_idx = Hashtbl.find ur_idx u in
	let sids = Hashtbl.fold (fun _r sids acc -> sids @ acc) r_idx [] in
	  sids
      with
	| Not_found -> []

    let dump_tables () =
      let string_of_sid (f, p) = Printf.sprintf "(%f, %d)" f (Obj.magic p) in
	HashtblSID.iter
	  (fun sid session ->
	     let (u, s, r) = session.usr in
	       Printf.eprintf "sid:%s %s@%s/%s prio:%d\n" (string_of_sid sid)
		 (u :> string) (s :> string) (r :> string) session.priority
	  ) sessions;
	Hashtbl.iter
	  (fun (s : Jlib.namepreped) ur_idx ->
	     Printf.eprintf "%s:\n" (s :> string);
	     Hashtbl.iter
	       (fun (u : Jlib.nodepreped) r_idx ->
		  Printf.eprintf "  %s:\n" (u :> string);
		  Hashtbl.iter
		    (fun (r : Jlib.resourcepreped) sids ->
		       Printf.eprintf "    %s:\n" (r :> string);
		       List.iter
			 (fun sid ->
			    Printf.eprintf "      sid:%s\n" (string_of_sid sid)
			 ) sids
		    ) r_idx;
	       ) ur_idx;
	  ) usr_idx;
	flush stderr;

  end
  include Session


  let set_session sid user server resource priority info =
    let usr = (user, server, resource) in
      add sid {usr; priority; info}

  let check_existing_resources user server resource =
    (* A connection exist with the same resource. We replace it: *)
    let sids = find_sids_by_usr user server resource in
      match sids with
	| [] -> ()
	| s :: sids' ->
	    let max_sid = List.fold_left max s sids' in
	      List.iter
		(fun ((_, pid) as s) ->
		   if s != max_sid then (
		     (* TODO *)
		     ()
		     (* Pid ! replaced; *)
		   )
		) sids

(* Get the user_max_session setting
   This option defines the max number of time a given users are allowed to
   log in
   Defaults to infinity *)
  let get_max_user_sessions user host =
    (* TODO *)
    max_int
    (*case acl:match_rule(
	   Host, max_user_sessions, jlib:make_jid(LUser, Host, "")) of
	Max when is_integer(Max) -> Max;
	infinity -> infinity;
	_ -> ?MAX_USER_SESSIONS
    end.*)


  let check_max_sessions user server =
    (* If the max number of sessions for a given is reached, we replace the
       first one *)
    let sids = find_sids_by_us user server in
    let max_sessions = get_max_user_sessions user server in
      match sids with
	| s :: sids' when List.length sids > max_sessions ->
	    let min_sid = List.fold_left min s sids' in
	    let (_, pid) = min_sid in
	      (* TODO *)
	      ()
	      (*Pid ! replaced*)
	| _ -> ()


  (* On new session, check if some existing connections need to be replace *)
  let check_for_sessions_to_replace user server resource =
    (* TODO: Depending on how this is executed, there could be an unneeded
       replacement for max_sessions. We need to check this at some point. *)
    check_existing_resources user server resource;
    check_max_sessions user server

  let open_session sid user server resource priority info =
    set_session sid user server resource priority info;
    check_for_sessions_to_replace user server resource
    (*JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_register_connection_hook, JID#jid.lserver,
		       [SID, JID, Info]).*)

  let do_close_session sid =
    remove sid
    (*
    Info = case mnesia:dirty_read({session, SID}) of
	       [] -> [];
	       [#session{info=I}] -> I
	   end,
    drop_session(SID),
    Info.*)

  let close_session sid _user _server _resource =
    do_close_session sid
    (*Info = do_close_session(SID),
    US = {jlib:nodeprep(User), jlib:nameprep(Server)},
    case ejabberd_cluster:get_node_new(US) of
	Node when Node /= node() ->
	    rpc:cast(Node, ?MODULE, drop_session, [SID]);
	_ ->
	    ok
    end,
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_remove_connection_hook, JID#jid.lserver,
		       [SID, JID, Info]).
    *)

  let close_session_unset_presence sid user server resource _status =
    close_session sid user server resource
    (*ejabberd_hooks:run(unset_presence_hook, jlib:nameprep(Server),
		       [User, Server, Resource, Status]).*)

  let set_presence sid user server resource priority _presence info =
    set_session sid user server resource priority info
    (*ejabberd_hooks:run(set_presence_hook, jlib:nameprep(Server),
		       [User, Server, Resource, Presence]).*)

  let unset_presence sid user server resource status info =
    set_session sid user server resource (-1) info
    (*ejabberd_hooks:run(unset_presence_hook, jlib:nameprep(Server),
		       [User, Server, Resource, Status]).*)

  let clean_session_list sessions =
    let rec clean_session_list ss res =
      match ss with
	| [] -> res
	| [s] -> s :: res
	| ((sid1, ses1) as s1) :: ((((sid2, ses2) as s2) :: rest) as rest') ->
	    if ses1.usr = ses2.usr then (
	      if sid1 > sid2
	      then clean_session_list (s1 :: rest) res
	      else clean_session_list (s2 :: rest) res
	    ) else clean_session_list rest' (s1 :: res)
    in
      clean_session_list
	(List.sort (fun (_, x) (_, y) -> compare x.usr y.usr) sessions) []

  let get_user_resources luser lserver =
    let sids = find_sids_by_us luser lserver in
    let sessions = List.map (fun s -> (s, find_exn s)) sids in
    let sessions = clean_session_list sessions in
      List.map
	(fun (_sid, {usr = (_, _, r); _}) -> r)
	sessions

  let get_user_present_resources luser lserver =
    let sids = find_sids_by_us luser lserver in
    let sessions = List.map (fun s -> (s, find_exn s)) sids in
    let sessions = clean_session_list sessions in
      List.map
	(fun (sid, {priority; usr = (_, _, r); _}) -> (priority, r, sid))
	sessions

  let bounce_offline_message from to' packet =
    let err = Jlib.make_error_reply packet Jlib.err_service_unavailable in
      Router.route to' from err
	(*stop.*)


  let route_message from to' packet =
    let luser = to'.Jlib.luser in
    let lserver = to'.Jlib.lserver in
    let prio_res = get_user_present_resources luser lserver in
    let priority =
      match prio_res with
	| [] -> None
	| p :: prio_res' ->
	    let (max_p, _, _) = List.fold_left max p prio_res' in
	      if max_p >= 0
	      then Some max_p
	      else None
    in
      match priority with
	| Some priority ->
	    (* Route messages to all priority that equals the max, if
	       positive *)
	    List.iter
	      (fun (p, _r, sid) ->
		 if p = priority then (
		   let (_, pid) = sid in
		     (*?DEBUG("sending to process ~p~n", [Pid]),*)
		     pid $! `Route (from, to', packet)
		 )
	      ) prio_res
	| _ -> (
	    match Xml.get_tag_attr_s "type" packet with
	      | "error" ->
		  ()
	      | "groupchat"
	      | "headline" ->
		  bounce_offline_message from to' packet
	      | _ -> (
		  match Auth.does_user_exist luser lserver with
		    | true -> (
			(* TODO *) ()
			    (*case is_privacy_allow(From, To, Packet) of
				true ->
				    ejabberd_hooks:run(offline_message_hook,
						       LServer,
						       [From, To, Packet]);
				false ->
				    ok
			    end;*)
		      )
		    | _ ->
			let err =
			  Jlib.make_error_reply
			    packet Jlib.err_service_unavailable
			in
			  Router.route to' from err
		)
	  )

  let process_iq from to' packet =
    match Jlib.iq_query_info packet with
      | `IQ ({Jlib.iq_xmlns = xmlns; _} as iq) -> (
	  let host = to'.Jlib.lserver in
	    if not (GenIQHandler.handle `SM host xmlns from to' iq) then (
	      let err =
		Jlib.make_error_reply packet Jlib.err_service_unavailable
	      in
		Router.route to' from err
	    )
	)
      | `Reply -> ()
      | _ ->
	  let err =
	    Jlib.make_error_reply packet Jlib.err_bad_request
	  in
	    Router.route to' from err


(* The default list applies to the user as a whole,
   and is processed if there is no active list set
   for the target session/resource to which a stanza is addressed,
   or if there are no current sessions for the user.
*)
  let is_privacy_allow from to' packet =
    true				(* TODO *)
(*
    User = To#jid.user,
    Server = To#jid.server,
    PrivacyList = ejabberd_hooks:run_fold(privacy_get_user_list, Server,
					  #userlist{}, [User, Server]),
    is_privacy_allow(From, To, Packet, PrivacyList).

%% Check if privacy rules allow this delivery
%% Function copied from ejabberd_c2s.erl
is_privacy_allow(From, To, Packet, PrivacyList) ->
    User = To#jid.user,
    Server = To#jid.server,
    allow == ejabberd_hooks:run_fold(
	       privacy_check_packet, Server,
	       allow,
	       [User,
		Server,
		PrivacyList,
		{From, To, Packet},
		in]).
*)

  let roster_in_subscription = Hooks.create_fold ()

  let rec do_route from to' packet =
    let {Jlib.luser = luser;
	 Jlib.lserver = lserver;
	 Jlib.lresource = lresource; _} = to' in
    let `XmlElement (name, attrs, _els) = packet in
      match (lresource :> string) with
	| "" -> (
	    match name with
	      | "presence" -> (
		  let pass =
		    match Xml.get_attr_s "type" attrs with
		      | "subscribe" ->
			  let reason =
			    Xml.get_path_s packet [`Elem "status"; `Cdata]
			  in
			    is_privacy_allow from to' packet &&
			      (Hooks.run_fold
				 roster_in_subscription
				 lserver
				 false
				 (luser, lserver, from, `Subscribe, reason))
		      | "subscribed" ->
			    is_privacy_allow from to' packet &&
			      (Hooks.run_fold
				 roster_in_subscription
				 lserver
				 false
				 (luser, lserver, from, `Subscribed, ""))
		      | "unsubscribe" ->
			    is_privacy_allow from to' packet &&
			      (Hooks.run_fold
				 roster_in_subscription
				 lserver
				 false
				 (luser, lserver, from, `Unsubscribe, ""))
		      | "unsubscribed" ->
			    is_privacy_allow from to' packet &&
			      (Hooks.run_fold
				 roster_in_subscription
				 lserver
				 false
				 (luser, lserver, from, `Unsubscribed, ""))
		      | _ -> true
		  in
		    if pass then (
		      let presources =
			get_user_present_resources luser lserver
		      in
			List.iter
			  (fun (_, r, _sid) ->
			     do_route
			       from (Jlib.jid_replace_resource' to' r) packet
			  ) presources
		    )
		)
	      | "message" ->
		  route_message from to' packet
	      | "iq" ->
		  process_iq from to' packet
	      | "broadcast" ->
		  List.iter
		    (fun r ->
		       do_route from (Jlib.jid_replace_resource' to' r) packet
		    ) (get_user_resources luser lserver)
	      | _ ->
		  ()
	  )
	| _ -> (
	    match find_sids_by_usr luser lserver lresource with
	      | [] -> (
		  match name with
		    | "message" ->
			route_message from to' packet
		    | "iq" -> (
			match Xml.get_attr_s "type" attrs with
			  | "error"
			  | "result" -> ()
			  | _ ->
			      let err =
				Jlib.make_error_reply
				  packet Jlib.err_service_unavailable
			      in
				Router.route to' from err
		      )
		    | _ ->
			(*?DEBUG("packet droped~n", [])*)
			()
		)
	      | s :: sids ->
		  let sid = List.fold_left max s sids in
		  let (_, pid) = sid in
		    (*?DEBUG("sending to process ~p~n", [Pid]),*)
		    pid $! `Route (from, to', packet)
	  )

  let route from to' packet =
    try
      do_route from to' packet
    with
      | exn ->
	  (* TODO *)
          Printf.eprintf "Exception %s in SM when processing\nfrom: %s\n to: %s\npacket: %s\n"
	    (Printexc.to_string exn)
	    (Jlib.jid_to_string from)
	    (Jlib.jid_to_string to')
	    (Xml.element_to_string packet); flush stderr;
	  ()
	  (*?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);*)

end

module Local =
struct

  let process_iq from to' packet =
    (* TODO *) ()
    (*IQ = jlib:iq_query_info(Packet),
    case IQ of
	#iq{xmlns = XMLNS} ->
	    Host = To#jid.lserver,
	    case ets:lookup(?IQTABLE, {XMLNS, Host}) of
		[{_, Module, Function}] ->
		    ResIQ = Module:Function(From, To, IQ),
		    if
			ResIQ /= ignore ->
			    ejabberd_router:route(
			      To, From, jlib:iq_to_xml(ResIQ));
			true ->
			    ok
		    end;
		[{_, Module, Function, Opts}] ->
		    gen_iq_handler:handle(Host, Module, Function, Opts,
					  From, To, IQ);
		[] ->
		    Err = jlib:make_error_reply(
			    Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
		    ejabberd_router:route(To, From, Err)
	    end;
	reply ->
	    IQReply = jlib:iq_query_or_response_info(Packet),
	    process_iq_reply(From, To, IQReply);
	_ ->
	    Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
	    ejabberd_router:route(To, From, Err),
	    ok
    end.*)


  let do_route from to' packet =
    (*?DEBUG("local route~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
	   [From, To, Packet, 8]),*)
    if (to'.Jlib.luser :> string) <> ""
    then SM.route from to' packet
    else if (to'.Jlib.lresource :> string) = "" then (
      let `XmlElement (name, _attrs, _els) = packet in
	match name with
	  | "iq" ->
	      process_iq from to' packet
	  | "message"
	  | "presence"
	  | _ ->
	      ()
    ) else (
      let `XmlElement (_name, attrs, _els) = packet in
	match Xml.get_attr_s "type" attrs with
	  | "error"
	  | "result" -> ()
	  | _ ->
	      (* TODO *) ()
		    (*ejabberd_hooks:run(local_send_to_resource_hook,
				       To#jid.lserver,
				       [From, To, Packet])*)
    )


  let route from to' packet =
    try
      do_route from to' packet
    with
      | exn ->
	  (* TODO *)
          Printf.eprintf "Exception %s in Local when processing\nfrom: %s\n to: %s\npacket: %s\n"
	    (Printexc.to_string exn)
	    (Jlib.jid_to_string from)
	    (Jlib.jid_to_string to')
	    (Xml.element_to_string packet); flush stderr;
	  ()
	  (*?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);*)

  let () =
    let pid = spawn (fun s -> Lwt.return ()) in
      List.iter
	(fun host ->
	   Router.register_route ~local_hint:(Some route) host pid
	     (*ejabberd_hooks:add(local_send_to_resource_hook, Host,
	       ?MODULE, bounce_resource_packet, 100)*)
	) (myhosts ())


end


module C2S :
sig
  type msg =
      [ Tcp.msg | XMLReceiver.msg | GenServer.msg
      | SM.msg | `Zxc of string * int ]
  type init_data = Lwt_unix.file_descr
  type state
  val init : init_data -> msg pid -> state
  val handle : msg -> state -> state GenServer.result
  val terminate : state -> unit Lwt.t

  val roster_get_subscription_lists :
    (Jlib.nodepreped * Jlib.namepreped, LJID.t list * LJID.t list)
    Hooks.fold_hook
  val roster_out_subscription :
    (Jlib.nodepreped * Jlib.namepreped * Jlib.jid *
       [ `Subscribe | `Subscribed | `Unsubscribe | `Unsubscribed ])
    Hooks.hook
end =
struct
  type msg =
      [ Tcp.msg | XMLReceiver.msg | GenServer.msg
      | SM.msg | `Zxc of string * int ]

  type c2s_state =
    | Wait_for_stream
    | Wait_for_auth
    | Wait_for_feature_request
    | Wait_for_bind
    | Wait_for_session
    | Wait_for_sasl_response of SASL.t
    | Session_established

  type state =
      {pid : msg pid;
       socket : Tcp.socket;
       xml_receiver : XMLReceiver.t;
       state : c2s_state;
       streamid : string;
       (*sasl_state : int;	(* TODO *)
       access,
       shaper,
       zlib = false,
       tls = false,
       tls_required = false,
       tls_enabled = false,
       tls_options = [],*)
       authenticated : bool;
       user : Jlib.nodepreped;
       server : Jlib.namepreped;
       resource : Jlib.resourcepreped;
       jid : Jlib.jid;
       sid : SM.sid;
       pres_t : LJIDSet.t;
       pres_f : LJIDSet.t;
       pres_a : LJIDSet.t;
       pres_i : LJIDSet.t;
       pres_last : Xml.element option;
       (*pres_pri,*)
       pres_timestamp : float;
       pres_invis : bool;
       (*privacy_list = #userlist{},
       conn = unknown,
       auth_module = unknown,*)
       ip : Unix.inet_addr;
       (*redirect = false,
       aux_fields = [],
       fsm_limit_opts,*)
       lang : string;
      }

  type init_data = Lwt_unix.file_descr

  let init socket self =
    let socket = Tcp.of_fd socket self in
    let xml_receiver = XMLReceiver.create self in
    let state = {pid = self;
		 socket;
		 xml_receiver;
		 state = Wait_for_stream;
		 streamid = "";
		 authenticated = false;
		 user = Jlib.nodeprep_exn "";
		 server = Jlib.nameprep_exn "";
		 resource = Jlib.resourceprep_exn "";
		 jid = Jlib.make_jid_exn "" "" "";
		 sid = (0.0, (self :> SM.msg pid));
		 pres_t = LJIDSet.empty;
		 pres_f = LJIDSet.empty;
		 pres_a = LJIDSet.empty;
		 pres_i = LJIDSet.empty;
		 pres_timestamp = 0.0;
		 pres_invis = false;
		 pres_last = None;
		 ip = Unix.inet_addr_any;	(* TODO *)
		 lang = "";
		}
    in
      Tcp.activate socket self;
      state

  let myname = "localhost"		(* TODO *)
  let is_my_host _server = true		(* TODO *)
  let invalid_ns_err = Jlib.serr_invalid_namespace
  let invalid_xml_err = Jlib.serr_xml_not_well_formed
  let host_unknown_err = Jlib.serr_host_unknown
  let invalid_from = Jlib.serr_invalid_from

  let send_text state text =
    (*Printf.printf "Send XML on stream = %S\n" text; flush stdout;*)
    Tcp.send_async state.socket text

  let send_element state el =
    send_text state (Xml.element_to_string el)

  let send_header state server version lang =
    let version_str =
      match version with
	| "" -> ""
	| _ -> " version='" ^ version ^ "'"
    in
    let lang_str =
      match lang with
	| "" -> ""
	| _ -> " xml:lang='" ^ lang ^ "'"
    in
    let stream_header_fmt =
      "<?xml version='1.0'?>" ^^
	"<stream:stream xmlns='jabber:client' " ^^
	"xmlns:stream='http://etherx.jabber.org/streams' " ^^
	"id='%s' from='%s'%s%s>"
    in
    let header =
      Printf.sprintf stream_header_fmt
	state.streamid
	server
	version_str
	lang_str
    in
      send_text state header

  let send_trailer state =
    let stream_trailer = "</stream:stream>" in
      send_text state stream_trailer

  let rec get_auth_tags (els : Xml.element_cdata list) u p d r =
    match els with
      | `XmlElement (name, _attrs, els) :: l -> (
	  let cdata = Xml.get_cdata els in
	    match name with
	      | "username" -> get_auth_tags l cdata p d r
	      | "password" -> get_auth_tags l u cdata d r
	      | "digest" ->   get_auth_tags l u p cdata r
	      | "resource" -> get_auth_tags l u p d cdata
	      | _ ->          get_auth_tags l u p d r
	)
      | _ :: l ->
	  get_auth_tags l u p d r
      | [] ->
	  (u, p, d, r)

  let is_auth_packet el =
    match Jlib.iq_query_info el with
      | `IQ {Jlib.iq_id = id; Jlib.iq_type = (`Set subel | `Get subel) as type';
	     Jlib.iq_xmlns = <:ns<AUTH>>; _} ->
	  let `XmlElement (_, _, els) = subel in
	  let type' =
	    match type' with
	      | `Set _ -> `Set
	      | `Get _ -> `Get
	  in
	    Some (id, type', get_auth_tags els "" "" "" "")
      | _ ->
	  None

  let new_id () =
    Jlib.get_random_string ()

  let process_unauthenticated_stanza state el =
    let el =
      match Xml.get_tag_attr_s "xml:lang" el with
	| "" -> (
	    match state.lang with
	      | "" -> el
	      | lang -> Xml.replace_tag_attr "xml:lang" lang el
	  )
	| _ -> el
    in
      match Jlib.iq_query_info el with
	| `IQ iq -> (
	    let res = None in		(* TODO *)
	    (*res = ejabberd_hooks:run_fold(c2s_unauthenticated_iq,
					  StateData#state.server,
					  empty,
					  [StateData#state.server, IQ,
					   StateData#state.ip]),
	    *)
	    match res with
	      | None ->
		  (* The only reasonable IQ's here are auth and register IQ's
		     They contain secrets, so don't include subelements to
		     response *)
		  let iq = {iq with
			      Jlib.iq_type = `Error (
				Jlib.err_service_unavailable, None)}
		  in
		  let res =
		    Jlib.replace_from_to
		      (Jlib.make_jid_exn "" (state.server :> string) "")
		      (Jlib.make_jid_exn "" "" "")
		      (Jlib.iq_to_xml iq)
		  in
		  let res = Jlib.remove_attr "to" res in
		    send_element state res
	      | Some res ->
		  send_element state res
	  )
	| _ ->
	    (* Drop any stanza, which isn't IQ stanza *)
	    ()

  let privacy_check_packet state from to' packet dir =
    (* TODO *)
    true
    (*ejabberd_hooks:run_fold(
      privacy_check_packet, StateData#state.server,
      allow,
      [StateData#state.user,
       StateData#state.server,
       StateData#state.privacy_list,
       {From, To, Packet},
       Dir]).
    *)


  let presence_broadcast state from jidset packet =
    LJIDSet.iter
      (fun ljid ->
	 let fjid = Jlib.ljid_to_jid ljid in
	   match privacy_check_packet state from fjid packet `Out with
	     | false -> ()
	     | true ->
		 Router.route from fjid packet
      ) jidset

  let presence_broadcast_to_trusted state from t a packet =
    LJIDSet.iter
      (fun ljid ->
	 if LJIDSet.mem ljid t then (
	   let fjid = Jlib.ljid_to_jid ljid in
	     match privacy_check_packet state from fjid packet `Out with
	       | false -> ()
	       | true ->
		   Router.route from fjid packet
	 )
      ) a


  let presence_broadcast_first from state packet =
    LJIDSet.iter
      (fun ljid ->
	 let fjid = Jlib.ljid_to_jid ljid in
	 (* TODO *)
	   ()
		      (* ejabberd_router:route(
			 From,
			 jlib:make_jid(JID),
			 {xmlelement, "presence",
			  [{"type", "probe"}],
			  []}),
		      *)
      ) state.pres_t;
    if state.pres_invis
    then state
    else (
      let pres_a =
	LJIDSet.fold
	  (fun ljid a ->
	     let fjid = Jlib.ljid_to_jid ljid in
	       (match privacy_check_packet state from fjid packet `Out with
		  | false -> ()
		  | true ->
		      Router.route from fjid packet
	       );
	       LJIDSet.add ljid a
	  ) state.pres_f state.pres_a
      in
	{state with pres_a}
    )


  let update_priority priority packet state =
    (*Info = [{ip, StateData#state.ip}, {conn, StateData#state.conn},
	     {auth_module, StateData#state.auth_module}],*)
    let info = [] in
      SM.set_presence
	state.sid state.user state.server state.resource
	priority packet info


  let get_priority_from_presence presence_packet =
    match Xml.get_subtag presence_packet "priority" with
      | None -> 0
      | Some sub_el -> (
	  try
	    int_of_string (Xml.get_tag_cdata sub_el)
	  with
	    | Failure "int_of_string" -> 0
	)


  let resend_offline_messages state =
    (* TODO *)
    ()
    (*case ejabberd_hooks:run_fold(
	   resend_offline_messages_hook, StateData#state.server,
	   [],
	   [StateData#state.user, StateData#state.server]) of
	Rs when is_list(Rs) ->
	    lists:foreach(
	      fun({route,
		   From, To, {xmlelement, _Name, _Attrs, _Els} = Packet}) ->
		      Pass = case privacy_check_packet(StateData, From, To, Packet, in) of
				 allow ->
				     true;
				 deny ->
				     false
			     end,
		      if
			  Pass ->
			      %% Attrs2 = jlib:replace_from_to_attrs(
			      %%		 jlib:jid_to_string(From),
			      %%		 jlib:jid_to_string(To),
			      %%		 Attrs),
			      %% FixedPacket = {xmlelement, Name, Attrs2, Els},
                              %% Use route instead of send_element to go through standard workflow
                              ejabberd_router:route(From, To, Packet);
			      %% send_element(StateData, FixedPacket),
			      %% ejabberd_hooks:run(user_receive_packet,
			      %%			 StateData#state.server,
			      %%			 [StateData#state.jid,
			      %%			  From, To, FixedPacket]);
			  true ->
			      ok
		      end
	      end, Rs)
    end.
    *)

  let resend_subscription_requests ({user = user;
				     server = server; _} as state) =
    (* TODO *)
    ()
    (*PendingSubscriptions = ejabberd_hooks:run_fold(
			     resend_subscription_requests_hook,
			     Server,
			     [],
			     [User, Server]),
    lists:foreach(fun(XMLPacket) ->
			  send_element(StateData,
				       XMLPacket)
		  end,
		  PendingSubscriptions).
    *)


  let check_privacy_route from state from_route to' packet =
    match privacy_check_packet state from to' packet `Out with
      | false ->
	  let lang = state.lang in
	  let err_text = "Your active privacy list has denied the routing of this stanza." in
	  let err =
	    Jlib.make_error_reply packet
	      (Jlib.errt_not_acceptable lang err_text)
	  in
	    Router.route to' from err
      | true ->
	  Router.route from_route to' packet


  let process_presence_probe from to' state =
    let lfrom = Jlib.jid_tolower from in
    let lbfrom =
      Jlib.jid_tolower (Jlib.jid_remove_resource from)
    in
      match state.pres_last with
	| None ->
	    ()
	| Some pres_last ->
	    let cond1 =
	      (not state.pres_invis) &&
		(LJIDSet.mem lfrom state.pres_f ||
		   (lfrom <> lbfrom && LJIDSet.mem lbfrom state.pres_f)) &&
		(not
		   (LJIDSet.mem lfrom state.pres_i ||
		      (lfrom <> lbfrom && LJIDSet.mem lbfrom state.pres_i)))
	    in
	    let cond2 =
	      state.pres_invis &&
		LJIDSet.mem lfrom state.pres_f &&
		LJIDSet.mem lfrom state.pres_a
	    in
	    if cond1 then (
	      let packet = pres_last in
	      let timestamp = state.pres_timestamp in
		    (*Packet1 = maybe_add_delay(Packet, utc, To, "", Timestamp),
		    case ejabberd_hooks:run_fold(
			   privacy_check_packet, StateData#state.server,
			   allow,
			   [StateData#state.user,
			    StateData#state.server,
			    StateData#state.privacy_list,
			    {To, From, Packet1},
			    out]) of
			deny ->
			    ok;
			allow ->
			    Pid=element(2, StateData#state.sid),
			    ejabberd_hooks:run(presence_probe_hook, StateData#state.server, [From, To, Pid]),*)
			    (* Don't route a presence probe to oneself *)
		if lfrom <> Jlib.jid_tolower to' then (
		  Router.route to' from packet
		)
	    ) else if cond2 then (
		    Router.route to' from
		      (`XmlElement ("presence", [], []));
	    ) else ()

  (* User updates his presence (non-directed presence packet) *)
  let presence_update from packet state =
    let `XmlElement (_name, attrs, _els) = packet in
      match Xml.get_attr_s "type" attrs with
	| "unavailable" ->
	    let status =
	      match Xml.get_subtag packet "status" with
		| None -> ""
		| Some status_tag ->
		    Xml.get_tag_cdata status_tag
	    in
	    (*Info = [{ip, StateData#state.ip}, {conn, StateData#state.conn},
		    {auth_module, StateData#state.auth_module}],*)
	    let info = [] in
	      SM.unset_presence
		state.sid state.user state.server state.resource
		status info;
	      presence_broadcast state from state.pres_a packet;
	      presence_broadcast state from state.pres_i packet;
	      {state with
		 pres_last = None;
		 pres_timestamp = 0.0;
		 pres_a = LJIDSet.empty;
		 pres_i = LJIDSet.empty;
		 pres_invis = false}
	| "invisible" ->
	    let new_priority = get_priority_from_presence packet in
	      update_priority new_priority packet state;
	      let state =
		if not state.pres_invis then (
		  presence_broadcast state from state.pres_a packet;
		  presence_broadcast state from state.pres_i packet;
		  let state =
		    {state with
		       pres_last = None;
		       pres_timestamp = 0.0;
		       pres_a = LJIDSet.empty;
		       pres_i = LJIDSet.empty;
		       pres_invis = true}
		  in
		    presence_broadcast_first from state packet
		) else state
	      in
		state
	| "error"
	| "probe"
	| "subscribe"
	| "subscribed"
	| "unsubscribe"
	| "unsubscribed" ->
	    state
	| _ ->
	    let old_priority =
	      match state.pres_last with
		| None -> 0
		| Some old_presence ->
		    get_priority_from_presence old_presence
	    in
	    let new_priority = get_priority_from_presence packet in
	    let timestamp = Unix.gettimeofday () in
	      update_priority new_priority packet state;
	      let from_unavail = state.pres_last = None || state.pres_invis in
		(*?DEBUG("from unavail = ~p~n", [FromUnavail]),*)
	      let state =
                let state = {state with
			       pres_last = Some packet;
                               pres_invis = false;
                               pres_timestamp = timestamp}
		in
		  if from_unavail then (
		    (*ejabberd_hooks:run(user_available_hook,
				       NewStateData#state.server,
				       [NewStateData#state.jid]),*)
		    if new_priority >= 0 then (
		      resend_offline_messages state;
		      resend_subscription_requests state
		    );
		    presence_broadcast_first from state packet
		  ) else (
		    presence_broadcast_to_trusted
		      state from state.pres_f state.pres_a packet;
		    if old_priority < 0 && new_priority >= 0
		    then resend_offline_messages state;
		    state
		  )
	      in
		state


  let roster_out_subscription = Hooks.create ()

  (* User sends a directed presence packet *)
  let presence_track from to' packet state =
    let `XmlElement (_name, attrs, _els) = packet in
    let lto = Jlib.jid_tolower to' in
    let user = state.user in
    let server = state.server in
      match Xml.get_attr_s "type" attrs with
	| "unavailable" ->
	    check_privacy_route from state from to' packet;
	    let pres_i = LJIDSet.remove lto state.pres_i in
	    let pres_a = LJIDSet.remove lto state.pres_a in
	      {state with pres_i; pres_a}
	| "invisible" ->
	    check_privacy_route from state from to' packet;
	    let pres_i = LJIDSet.add lto state.pres_i in
	    let pres_a = LJIDSet.remove lto state.pres_a in
	      {state with pres_i; pres_a}
	| "subscribe" ->
	    Hooks.run roster_out_subscription server
	      (user, server, to', `Subscribe);
	    check_privacy_route from state (Jlib.jid_remove_resource from)
	      to' packet;
	    state
	| "subscribed" ->
	    Hooks.run roster_out_subscription server
	      (user, server, to', `Subscribed);
	    check_privacy_route from state (Jlib.jid_remove_resource from)
	      to' packet;
	    state
	| "unsubscribe" ->
	    Hooks.run roster_out_subscription server
	      (user, server, to', `Unsubscribe);
	    check_privacy_route from state (Jlib.jid_remove_resource from)
	      to' packet;
	    state
	| "unsubscribed" ->
	    Hooks.run roster_out_subscription server
	      (user, server, to', `Unsubscribed);
	    check_privacy_route from state (Jlib.jid_remove_resource from)
	      to' packet;
	    state
	| "error"
	| "probe" ->
	    check_privacy_route from state from to' packet;
	    state
	| _ ->
	    check_privacy_route from state from to' packet;
	    let pres_i = LJIDSet.remove lto state.pres_i in
	    let pres_a = LJIDSet.add lto state.pres_a in
	      {state with pres_i; pres_a}

  let roster_get_subscription_lists = Hooks.create_fold ()

  (* Check from attributes
     returns invalid-from|NewElement *)
  let check_from el from_jid =
    match Xml.get_tag_attr "from" el with
      | None -> true
      | Some sjid -> (
	  let jid' = Jlib.string_to_jid sjid in
	    match jid' with
	      | None -> false
	      | Some jid ->
		    if jid.Jlib.luser = from_jid.Jlib.luser &&
		      jid.Jlib.lserver = from_jid.Jlib.lserver &&
		      jid.Jlib.lresource = from_jid.Jlib.lresource
		    then true
		    else if jid.Jlib.luser = from_jid.Jlib.luser &&
		      jid.Jlib.lserver = from_jid.Jlib.lserver &&
		      (jid.Jlib.lresource :> string) = ""
		    then true
		    else false
	)


  let fsm_next_state state =
    (* TODO *)
    Lwt.return (`Continue state)

  let maybe_migrate state =
    (*PackedStateData = pack(StateData),*)
    let {user = u; server = s; resource = r; sid = sid; _} = state in
    (*case ejabberd_cluster:get_node({jlib:nodeprep(U), jlib:nameprep(S)}) of
	Node when Node == node() ->*)
	    (*Conn = get_conn_type(StateData),
	    Info = [{ip, StateData#state.ip}, {conn, Conn},
		    {auth_module, StateData#state.auth_module}],*)
    let info = [] in
    let presence = state.pres_last in
    let priority =
      match presence with
        | None -> -1
        | Some presence -> get_priority_from_presence presence
    in
      SM.open_session sid u s r priority info;
      fsm_next_state state
	(*Node ->
	    fsm_migrate(StateName, PackedStateData, Node, 0)
    end.*)


  let wait_for_stream msg state =
    match msg with
      | `XmlStreamStart (_name, attrs) -> (
	  let default_lang = "en" in	(* TODO *)
	    match Xml.get_attr_s "xmlns:stream" attrs with
	      | <:ns<STREAM>> -> (
		  let server = Jlib.nameprep (Xml.get_attr_s "to" attrs) in
		    match server with
		      | Some server when is_my_host server -> (
			  let lang =
			    let lang = Xml.get_attr_s "xml:lang" attrs in
			      if String.length lang <= 35 then (
				(* As stated in BCP47, 4.4.1:
				   Protocols or specifications that
				   specify limited buffer sizes for
				   language tags MUST allow for
				   language tags of at least 35 characters. *)
				lang
			      ) else (
				(* Do not store long language tag to
				   avoid possible DoS/flood attacks *)
				""
			      )
			  in
			    (*change_shaper(StateData, jlib:make_jid("", Server, "")),*)
			    match Xml.get_attr_s "version" attrs with
			      | "1.0" -> (
				  send_header state (server :> string) "1.0" default_lang;
				  if not state.authenticated then (
				    (*SASLState =
				      cyrsasl:server_new(
				      "jabber", Server, "", [],
				      fun(U) ->
				      ejabberd_auth:get_password_with_authmodule(
				      U, Server)
				      end,
				      fun(U, P) ->
				      ejabberd_auth:check_password_with_authmodule(
				      U, Server, P)
				      end,
				      fun(U, P, D, DG) ->
				      ejabberd_auth:check_password_with_authmodule(
				      U, Server, P, D, DG)
				      end),*)
				    let mechs =
				      List.map
					(fun s ->
					   `XmlElement ("mechanism", [],
							[`XmlCdata s]))
					(SASL.listmech server)
				    in
				      (*SockMod =
					(StateData#state.sockmod):get_sockmod(
					StateData#state.socket),
					Zlib = StateData#state.zlib,
					CompressFeature =
					case Zlib andalso
					((SockMod == gen_tcp) orelse
					(SockMod == tls)) of
					true ->
					[{xmlelement, "compression",
					[{"xmlns", ?NS_FEATURE_COMPRESS}],
					[{xmlelement, "method",
					[], [{xmlcdata, "zlib"}]}]}];
					_ ->
					[]
					end,
					TLS = StateData#state.tls,
					TLSEnabled = StateData#state.tls_enabled,
					TLSRequired = StateData#state.tls_required,
					TLSFeature =
					case (TLS == true) andalso
					(TLSEnabled == false) andalso
					(SockMod == gen_tcp) of
					true ->
					case TLSRequired of
					true ->
					[{xmlelement, "starttls",
					[{"xmlns", ?NS_TLS}],
					[{xmlelement, "required",
					[], []}]}];
					_ ->
					[{xmlelement, "starttls",
					[{"xmlns", ?NS_TLS}], []}]
					end;
					false ->
					[]
					end,
				      *)
				      send_element
					state
					(`XmlElement
					   ("stream:features", [],
					    (*TLSFeature ++
					      CompressFeature ++*)
					    [`XmlElement
					       ("mechanisms",
						[("xmlns", <:ns<SASL>>)],
						mechs)]
					      (*++
						ejabberd_hooks:run_fold(
						c2s_stream_features,
						Server,
						[], [Server])*))
					);
				      Lwt.return (
					`Continue
					  {state with
					     state = Wait_for_feature_request;
					     server = server;
					     (*sasl_state = SASLState,*)
					     lang = lang});
				  ) else (
				    match (state.resource :> string) with
				      | "" ->
					  (* RosterVersioningFeature =
					     ejabberd_hooks:run_fold(
					     roster_get_versioning_feature,
					     Server, [], [Server]),*)
					  let stream_features =
					    [`XmlElement
					       ("bind",
						[("xmlns", <:ns<BIND>>)], []);
					     `XmlElement
					       ("session",
						[("xmlns", <:ns<SESSION>>)], [])]
					      (*++ RosterVersioningFeature
						++ ejabberd_hooks:run_fold(
						c2s_stream_features,
						Server,
						[], [Server])*)
					  in
					    send_element
					      state
					      (`XmlElement
						 ("stream:features", [],
						  stream_features));
					    Lwt.return (
					      `Continue
						{state with
						   state = Wait_for_bind;
						   server = server;
						   lang = lang})
				      | _ ->
					  send_element
					    state
					    (`XmlElement
					       ("stream:features", [], []));
					  Lwt.return (
					    `Continue
					      {state with
						 state = Wait_for_session;
						 server = server;
						 lang = lang})
				  )
				)
			      | _ ->
				  send_header state (server :> string) "" default_lang;
				  (*if
				    (not StateData#state.tls_enabled) and
				    StateData#state.tls_required ->
				    send_element(
				    StateData,
				    ?POLICY_VIOLATION_ERR(
				    Lang,
				    "Use of STARTTLS required")),
				    send_trailer(StateData),
				    {stop, normal, StateData};
				    true ->*)
				  Lwt.return (
				    `Continue
				      {state with
					 state = Wait_for_auth;
					 server = server;
					 lang = lang})
			)
		      | _ -> (
			  send_header state myname "" default_lang;
			  send_element state host_unknown_err;
			  send_trailer state;
			  Lwt.return (`Stop state)
			)
		)
	      | _ ->
		      send_header state myname "" default_lang;
		      send_element state invalid_ns_err;
		      send_trailer state;
		      Lwt.return (`Stop state)
	)
(*
wait_for_stream(timeout, StateData) ->
    {stop, normal, StateData};
*)

      | `XmlStreamElement _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamEnd _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError _ ->
	  send_header state myname "1.0" "";
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)


  let wait_for_auth msg state =
    match msg with
      | `XmlStreamElement el -> (
	  match is_auth_packet el with
	    | Some (_id, `Get, (u, _, _, _)) -> (
		let `XmlElement (name, attrs, _els) =
		  Jlib.make_result_iq_reply el
		in
		let ucdata =
		  match u with
		    | "" -> [];
		    | _ -> [`XmlCdata u]
		in
		let res =
		  match (*ejabberd_auth:plain_password_required(
			  StateData#state.server)*)false with (* TODO *)
		    | false ->
			  `XmlElement
			    (name, attrs,
			     [`XmlElement
				("query", [("xmlns", <:ns<AUTH>>)],
				 [`XmlElement ("username", [], ucdata);
				  `XmlElement ("password", [], []);
				  `XmlElement ("digest", [], []);
				  `XmlElement ("resource", [], [])
				 ])])
		    | true ->
			  `XmlElement
			    (name, attrs,
			     [`XmlElement
				("query", [("xmlns", <:ns<AUTH>>)],
				 [`XmlElement ("username", [], ucdata);
				  `XmlElement ("password", [], []);
				  `XmlElement ("resource", [], [])
				 ])])
		in
		  send_element state res;
		  Lwt.return
		    (`Continue
		       {state with state = Wait_for_auth})
	      )
	    | Some (_id, `Set, (_u, _p, _d, "")) ->
		let err =
		  Jlib.make_error_reply el
		    (Jlib.err_auth_no_resource_provided state.lang)
		in
		  send_element state err;
		  Lwt.return
		    (`Continue
		       {state with state = Wait_for_auth})
	    | Some (_id, `Set, (u, _p, _d, r)) -> (
		match Jlib.make_jid u (state.server :> string) r with
		  | Some jid (*when 
			       (acl:match_rule(StateData#state.server,
			       StateData#state.access, JID) == allow) *) (* TODO *) -> (
		      (*let dgen = fun pw ->
                        Jlib.sha1 (state.streamid ^ pw)
		      in*)
			match (*ejabberd_auth:check_password_with_authmodule(
			  U, StateData#state.server, P, D, DGen) *) (* TODO *)
			  (true, 0)
			with
			  | (true, _auth_module) -> (
			      (*?INFO_MSG(
				"(~w) Accepted legacy authentication for ~s by ~p",
				[StateData#state.socket,
				jlib:jid_to_string(JID), AuthModule]),*)
			      lwt () = Lwt_io.printf "Accepted legacy authentication for %s\n" (Jlib.jid_to_string jid) in
				match (*need_redirect(StateData#state{user = U})*) (* TODO *)
				  None
				with
                                  | Some host -> (
                                    (*?INFO_MSG("(~w) Redirecting ~s to ~s",
                                              [StateData#state.socket,
                                               jlib:jid_to_string(JID), Host]),*)
				      lwt () = Lwt_io.printf "Redirecting %s to %s\n" (Jlib.jid_to_string jid) host in
                                        send_element state
					  (Jlib.serr_see_other_host host);
                                        send_trailer state;
					Lwt.return
					  (`Stop state);
	                            )
	                          | None ->
                                      let sid =
					(Unix.gettimeofday (),
					 (state.pid :> SM.msg pid))
				      in
				      (*Conn = get_conn_type(StateData),*)
                                      let res = Jlib.make_result_iq_reply el in
					(*Res = setelement(4, Res1, []),*)
					send_element state res;
					(*change_shaper(StateData, JID),*)
					let (fs, ts) =
					  Hooks.run_fold
					    roster_get_subscription_lists
					    state.server
					    ([], [])
					    (jid.Jlib.luser, state.server)
					in
					let ljid =
					  Jlib.jid_tolower (Jlib.jid_remove_resource jid)
					in
					let fs = ljid :: fs in
					let ts = ljid :: ts in
					  (*PrivList =
                                          ejabberd_hooks:run_fold(
                                          privacy_get_user_list,
                                          StateData#state.server,
                                          #userlist{},
                                          [U, StateData#state.server]),*)
					let state =
                                          {state with
                                             user = jid.Jlib.luser;
                                             resource = jid.Jlib.lresource;
                                             jid = jid;
                                             sid = sid;
                                               (*conn = Conn,
                                               auth_module = AuthModule,
                                               pres_f = ?SETS:from_list(Fs1),
                                               pres_t = ?SETS:from_list(Ts1),
                                               privacy_list = PrivList*)}
					in
					  (*DebugFlag = ejabberd_hooks:run_fold(
                                            c2s_debug_start_hook,
                                            NewStateData#state.server,
                                            false,
                                            [self(), NewStateData]),*)
					  maybe_migrate
					    {state with
					       state = Session_established}
                            )
			  | _ ->
			    (*?INFO_MSG(
			       "(~w) Failed legacy authentication for ~s",
			       [StateData#state.socket,
				jlib:jid_to_string(JID)]),*)
			      lwt () = Lwt_io.printf "Failed legacy authentication for %s\n" (Jlib.jid_to_string jid) in
		              let err =
				Jlib.make_error_reply el Jlib.err_not_authorized
			      in
				send_element state err;
				Lwt.return
				  (`Continue
				     {state with state = Wait_for_auth})
		    )
		  | None ->
		      (*?INFO_MSG(
			"(~w) Forbidden legacy authentication for "
			"username '~s' with resource '~s'",
			[StateData#state.socket, U, R]),*)
		      lwt () = Lwt_io.printf "Forbidden legacy authentication for %s with resource '%s'\n" u r in
		      let err = Jlib.make_error_reply el Jlib.err_jid_malformed
		      in
			send_element state err;
			Lwt.return
			  (`Continue
			     {state with state = Wait_for_auth})
		  | Some jid ->
		      (*?INFO_MSG(
			"(~w) Forbidden legacy authentication for ~s",
			[StateData#state.socket,
			jlib:jid_to_string(JID)]),*)
		      lwt () = Lwt_io.printf "Forbidden legacy authentication for %s\n" (Jlib.jid_to_string jid) in
		      let err = Jlib.make_error_reply el Jlib.err_not_allowed
		      in
			send_element state err;
			Lwt.return
			  (`Continue
			     {state with state = Wait_for_auth})
	      )
	    | None ->
		process_unauthenticated_stanza state el;
		Lwt.return
		  (`Continue
		     {state with state = Wait_for_auth})
	)
(*
wait_for_auth(timeout, StateData) ->
    {stop, normal, StateData};
*)

      | `XmlStreamStart _ -> assert false

      | `XmlStreamEnd _ ->
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)

  let wait_for_feature_request msg state =
    match msg with
      | `XmlStreamElement el -> (
	  let `XmlElement (name, attrs, els) = el in
	    (* Zlib = StateData#state.zlib,
	       TLS = StateData#state.tls,
	       TLSEnabled = StateData#state.tls_enabled,
	       TLSRequired = StateData#state.tls_required,
	       SockMod = (StateData#state.sockmod):get_sockmod(StateData#state.socket),*)
	    match (Xml.get_attr_s "xmlns" attrs), name with
	      | <:ns<SASL>>, "auth" (*when not ((SockMod == gen_tcp) and
				      TLSRequired)*) (* TODO *) -> (
		  let mech = Xml.get_attr_s "mechanism" attrs in
		  let client_in = Jlib.decode_base64 (Xml.get_cdata els) in
		  let sasl_result =
		    SASL.server_start
		      ~service:"jabber"
		      ~server_fqdn:state.server
		      ~user_realm:""
		      ~get_password:
		      (fun u ->
			 Auth.get_password_with_authmodule u state.server
		      )
		      ~check_password:
		      (fun u p ->
			 Auth.check_password_with_authmodule u state.server p
		      )
		      ~check_password_digest:
		      (fun u p d dg ->
			 Auth.check_password_digest_with_authmodule
			   u state.server p d dg
		      )
		      ~mech
		      client_in
		  in
		    match sasl_result with
		      | SASL.Done props -> (
			  (*catch (StateData#state.sockmod):reset_stream(
                            StateData#state.socket),*)
			  XMLReceiver.reset_stream state.xml_receiver;
			  let u = Jlib.nodeprep_exn (List.assoc `Username props) in	(* TODO *)
			    (*AuthModule = xml:get_attr_s(auth_module, Props),*)
			    (*?INFO_MSG("(~w) Accepted authentication for ~s by ~p",
                              [StateData#state.socket, U, AuthModule]),*)
			  lwt () = Lwt_io.printf "Accepted authentication for %s\n" (u :> string) in
                            send_element state
                              (`XmlElement ("success",
                                            [("xmlns", <:ns<SASL>>)], []));
                            Lwt.return
			      (`Continue
				 {state with
				    state = Wait_for_stream;
                                    streamid = new_id ();
                                    authenticated = true;
                                    (*auth_module = AuthModule,*)
                                    user = u})
			)
		      | SASL.Continue (server_out, sasl_mech) ->
			  send_element state
			    (`XmlElement
			       ("challenge",
				[("xmlns", <:ns<SASL>>)],
				[`XmlCdata (Jlib.encode_base64 server_out)]));
			  Lwt.return
			    (`Continue
			       {state with
				  state = Wait_for_sasl_response sasl_mech})
		      | SASL.ErrorUser (error, username) ->
			  (*?INFO_MSG(
			    "(~w) Failed authentication for ~s@~s",
			    [StateData#state.socket,
			    Username, StateData#state.server]),*)
			  lwt () = Lwt_io.printf "Failed authentication for %s@%s\n" username (state.server :> string) in
			    send_element state
			      (`XmlElement
				 ("failure",
				  [("xmlns", <:ns<SASL>>)],
				  [`XmlElement (error, [], [])]));
			    Lwt.return (
			      `Continue
				{state with
				   state = Wait_for_feature_request})
		      | SASL.Error error ->
			  send_element state
			    (`XmlElement
			       ("failure",
				[("xmlns", <:ns<SASL>>)],
				[`XmlElement (error, [], [])]));
			  Lwt.return (
			    `Continue
			      {state with
				 state = Wait_for_feature_request})
		)
		(* | <:ns<TLS>>, "starttls" (*when TLS == true,
					 TLSEnabled == false,
					 SockMod == gen_tcp*) (* TODO *)
		  ->
	    TLSOpts = case ejabberd_config:get_local_option(
			     {domain_certfile, StateData#state.server}) of
			  undefined ->
			      StateData#state.tls_options;
			  CertFile ->
			      [{certfile, CertFile} |
			       lists:keydelete(
				 certfile, 1, StateData#state.tls_options)]
		      end,
	    Socket = StateData#state.socket,
	    TLSSocket = (StateData#state.sockmod):starttls(
			  Socket, TLSOpts,
			  xml:element_to_binary(
			    {xmlelement, "proceed", [{"xmlns", ?NS_TLS}], []})),
	    fsm_next_state(wait_for_stream,
			   StateData#state{socket = TLSSocket,
					   streamid = new_id(),
					   tls_enabled = true
					  });
	{?NS_COMPRESS, "compress"} when Zlib == true,
					((SockMod == gen_tcp) or
					 (SockMod == tls)) ->
	    case xml:get_subtag(El, "method") of
		false ->
		    send_element(StateData,
				 {xmlelement, "failure",
				  [{"xmlns", ?NS_COMPRESS}],
				  [{xmlelement, "setup-failed", [], []}]}),
		    fsm_next_state(wait_for_feature_request, StateData);
		Method ->
		    case xml:get_tag_cdata(Method) of
			"zlib" ->
			    Socket = StateData#state.socket,
			    ZlibSocket = (StateData#state.sockmod):compress(
					   Socket,
					   xml:element_to_binary(
					     {xmlelement, "compressed",
					      [{"xmlns", ?NS_COMPRESS}], []})),
			    fsm_next_state(wait_for_stream,
			     StateData#state{socket = ZlibSocket,
					     streamid = new_id()
					    });
			_ ->
			    send_element(StateData,
					 {xmlelement, "failure",
					  [{"xmlns", ?NS_COMPRESS}],
					  [{xmlelement, "unsupported-method",
					    [], []}]}),
			    fsm_next_state(wait_for_feature_request,
					   StateData)
		    end
	    end;
*)
	      | _ ->
		  (* TODO *)
	    (*if
		(SockMod == gen_tcp) and TLSRequired ->
		    Lang = StateData#state.lang,
		    send_element(StateData, ?POLICY_VIOLATION_ERR(
					       Lang,
					       "Use of STARTTLS required")),
		    send_trailer(StateData),
		    {stop, normal, StateData};
		true ->*)
		  process_unauthenticated_stanza state el;
		  Lwt.return (
		    `Continue
		      {state with
			 state = Wait_for_feature_request})
	)

(*
wait_for_feature_request(timeout, StateData) ->
    {stop, normal, StateData};
*)

      | `XmlStreamStart _ -> assert false

      | `XmlStreamEnd _ ->
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)


  let wait_for_sasl_response msg sasl_state state =
    match msg with
      | `XmlStreamElement el -> (
	  let `XmlElement (name, attrs, els) = el in
	    match Xml.get_attr_s "xmlns" attrs, name with
	      | <:ns<SASL>>, "response" -> (
		  let client_in = Jlib.decode_base64 (Xml.get_cdata els) in
		    match SASL.server_step sasl_state client_in with
		      | SASL.Done props -> (
			  (*catch (StateData#state.sockmod):reset_stream(
                            StateData#state.socket),*)
			  XMLReceiver.reset_stream state.xml_receiver;
			  let u = Jlib.nodeprep_exn (List.assoc `Username props) in	(* TODO *)
			    (*AuthModule = xml:get_attr_s(auth_module, Props),
			      ?INFO_MSG("(~w) Accepted authentication for ~s by ~p",
			      [StateData#state.socket, U, AuthModule]),*)
			  lwt () = Lwt_io.printf "Accepted authentication for %s\n" (u :> string) in
                            send_element state
                              (`XmlElement ("success",
                                            [("xmlns", <:ns<SASL>>)], []));
                            Lwt.return
			      (`Continue
				 {state with
				    state = Wait_for_stream;
                                    streamid = new_id ();
                                    authenticated = true;
                                    (*auth_module = AuthModule,*)
                                    user = u})
			)
		      | SASL.Continue (server_out, sasl_mech) ->
			  send_element state
			    (`XmlElement
			       ("challenge",
				[("xmlns", <:ns<SASL>>)],
				[`XmlCdata (Jlib.encode_base64 server_out)]));
			  Lwt.return
			    (`Continue
			       {state with
				  state = Wait_for_sasl_response sasl_mech})
		      | SASL.ErrorUser (error, username) ->
			  (*?INFO_MSG(
			    "(~w) Failed authentication for ~s@~s",
			    [StateData#state.socket,
			    Username, StateData#state.server]),*)
			  lwt () = Lwt_io.printf "Failed authentication for %s@%s\n" username (state.server :> string) in
			    send_element state
			      (`XmlElement
				 ("failure",
				  [("xmlns", <:ns<SASL>>)],
				  [`XmlElement (error, [], [])]));
			    Lwt.return (
			      `Continue
				{state with
				   state = Wait_for_feature_request})
		      | SASL.Error error ->
			  send_element state
			    (`XmlElement
			       ("failure",
				[("xmlns", <:ns<SASL>>)],
				[`XmlElement (error, [], [])]));
			  Lwt.return (
			    `Continue
			      {state with
				 state = Wait_for_feature_request})
		)
	      | _ ->
		process_unauthenticated_stanza state el;
		Lwt.return
		  (`Continue
		     {state with state = Wait_for_feature_request})
	)

(*
wait_for_sasl_response(timeout, StateData) ->
    {stop, normal, StateData};
*)

      | `XmlStreamStart _ -> assert false

      | `XmlStreamEnd _ ->
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)


  let wait_for_bind msg state =
    match msg with
      | `XmlStreamElement el -> (
	  match Jlib.iq_query_info el with
	    | `IQ ({Jlib.iq_type = `Set sub_el;
		    Jlib.iq_xmlns = <:ns<BIND>>; _} as iq) -> (
		let u = state.user in
		let r = Xml.get_path_s sub_el [`Elem "resource"; `Cdata] in
		let r =
		  match Jlib.resourceprep r with
		    | None -> None;
		    | Some resource when (resource :> string) = "" ->
			Some (Jlib.resourceprep_exn
				(Jlib.get_random_string () ^
				   string_of_int (int_of_float (Unix.time ())))
			     )
		    | Some resource -> Some resource
		in
		  match r with
		    | None ->
			let err =
			  Jlib.make_error_reply el Jlib.err_bad_request
			in
			  send_element state err;
			  Lwt.return
			    (`Continue
			       {state with state = Wait_for_bind})
		    | Some r ->
			let jid = Jlib.make_jid' u state.server r in
			let res =
			  {iq with
			     Jlib.iq_type =
			      `Result
				(Some (`XmlElement
					 ("bind",
					  [("xmlns", <:ns<BIND>>)],
					  [`XmlElement
					     ("jid", [],
					      [`XmlCdata
						 (Jlib.jid_to_string jid)])])
				      ))}
			in
			  send_element state (Jlib.iq_to_xml res);
			  Lwt.return
			    (`Continue {state with
					  state = Wait_for_session;
					  resource = r;
					  jid = jid})
	      )
	    | _ ->
		Lwt.return (`Continue {state with state = Wait_for_bind})
	)

(*
wait_for_bind(timeout, StateData) ->
    {stop, normal, StateData};
*)

      | `XmlStreamStart _ -> assert false

      | `XmlStreamEnd _ ->
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)

  let wait_for_session msg state =
    match msg with
      | `XmlStreamElement el -> (
	  match Jlib.iq_query_info el with
	    | `IQ ({Jlib.iq_type = `Set sub_el;
		    Jlib.iq_xmlns = <:ns<SESSION>>; _} as iq) -> (
		let u = state.user in
		let jid = state.jid in
		  match (*acl:match_rule(StateData#state.server,
				       StateData#state.access, JID)*)true (* TODO *) with
		    | true ->
			(*?INFO_MSG("(~w) Opened session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(JID)]),*)
		      lwt () = Lwt_io.printf "Opened session for %s\n" (Jlib.jid_to_string jid) in
		      let res = Jlib.make_result_iq_reply el in
			send_element state res;
		    (*change_shaper(StateData, JID),*)
			let (fs, ts) =
			  Hooks.run_fold
			    roster_get_subscription_lists
			    state.server
			    ([], [])
			    (u, state.server)
			in
			let ljid =
			  Jlib.jid_tolower (Jlib.jid_remove_resource jid)
			in
			let fs = ljid :: fs in
			let ts = ljid :: ts in
		    (*PrivList =
			ejabberd_hooks:run_fold(
			  privacy_get_user_list, StateData#state.server,
			  #userlist{},
			  [U, StateData#state.server]),*)
                        let sid =
			  (Unix.gettimeofday (), (state.pid :> SM.msg pid))
			in
		    (*Conn = get_conn_type(StateData),
		    %% Info = [{ip, StateData#state.ip}, {conn, Conn},
		    %% 	    {auth_module, StateData#state.auth_module}],
		    %% ejabberd_sm:open_session(
		    %%   SID, U, StateData#state.server, R, Info),
		    *)
			let state =
                          {state with
			     sid = sid;
			       (*conn = Conn;*)
			     pres_f = LJIDSet.from_list fs;
			     pres_t = LJIDSet.from_list ts;
			       (*privacy_list = PrivList*)}
			in
		    (*DebugFlag = ejabberd_hooks:run_fold(c2s_debug_start_hook,
							NewStateData#state.server,
							false,
							[self(), NewStateData]),*)
			  maybe_migrate
			    {state with state = Session_established}
		    | _ ->
		    (*ejabberd_hooks:run(forbidden_session_hook,
				       StateData#state.server, [JID]),
		    ?INFO_MSG("(~w) Forbidden session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(JID)]),*)
			lwt () = Lwt_io.printf "Forbidden session for %s\n" (Jlib.jid_to_string jid) in
			let err = Jlib.make_error_reply el Jlib.err_not_allowed
			in
			  send_element state err;
			  Lwt.return
			    (`Continue
			       {state with state = Wait_for_session})
	      )
	    | _ ->
		Lwt.return
		  (`Continue
		     {state with state = Wait_for_session})
	)
(*
wait_for_session(timeout, StateData) ->
    {stop, normal, StateData};
*)

      | `XmlStreamStart _ -> assert false

      | `XmlStreamEnd _ ->
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)


(* Process packets sent by user (coming from user on c2s XMPP
   connection)
*)
  let session_established2 el state =
    let `XmlElement (name, attrs, _els) = el in
    let user = state.user in
    let server = state.server in
    let from_jid = state.jid in
    let to' = Xml.get_attr_s "to" attrs in
    let to_jid =
      match to' with
	| "" ->
	    Some (Jlib.jid_remove_resource state.jid)
	| _ ->
	    Jlib.string_to_jid to'
    in
    let el = Jlib.remove_attr "xmlns" el in
    let el =
      match Xml.get_attr_s "xml:lang" attrs with
	| "" -> (
	    match state.lang with
	      | "" -> el
	      | lang -> Xml.replace_tag_attr "xml:lang" lang el
	  )
	| _ -> el
    in
    let state =
      match to_jid with
	| None -> (
	    match Xml.get_attr_s "type" attrs with
	      | "error"
	      | "result" -> state
	      | _ ->
		  let err = Jlib.make_error_reply el Jlib.err_jid_malformed in
	  	    send_element state err;
		    state
	  )
	| Some to_jid -> (
	    match name with
	      | "presence" -> (
			(*PresenceEl = ejabberd_hooks:run_fold(
				       c2s_update_presence,
				       Server,
				       NewEl,
				       [User, Server]),
			ejabberd_hooks:run(
			  user_send_packet,
			  Server,
			  [StateData#state.debug, FromJID, ToJID, PresenceEl]),*)
		  if to_jid.Jlib.luser = user &&
		    to_jid.Jlib.lserver = server &&
		    (to_jid.Jlib.lresource :> string) = ""
		  then (
		    (*?DEBUG("presence_update(~p,~n\t~p,~n\t~p)",
		      [FromJID, PresenceEl, StateData]),*)
		    presence_update from_jid el state
		  ) else (
		    presence_track from_jid to_jid el state
		  )
		)
	      | "iq" -> (
		  match Jlib.iq_query_info el with
		    (*| #iq{xmlns = ?NS_PRIVACY} = IQ ->
				ejabberd_hooks:run(
				  user_send_packet,
				  Server,
				  [StateData#state.debug, FromJID, ToJID, NewEl]),
				process_privacy_iq(
				  FromJID, ToJID, IQ, StateData);*)
		    | _ ->
				(*ejabberd_hooks:run(
				  user_send_packet,
				  Server,
                                  [StateData#state.debug, FromJID, ToJID, NewEl]),*)
			check_privacy_route from_jid state from_jid to_jid el;
			state
		)
	      | "message" ->
			(*ejabberd_hooks:run(user_send_packet,
					   Server,
					   [StateData#state.debug, FromJID, ToJID, NewEl]),
			*)
		  check_privacy_route from_jid state from_jid to_jid el;
		  state
	      | _ ->
		  state
	  )
    in
      (*ejabberd_hooks:run(c2s_loop_debug, [{xmlstreamelement, El}]),*)
      fsm_next_state state


  let session_established msg state =
    match msg with
      | `XmlStreamElement el -> (
	  let from_jid = state.jid in
	    (* Check 'from' attribute in stanza RFC 3920 Section 9.1.2 *)
	    match check_from el from_jid with
	      | false ->
		  send_element state invalid_from;
		  send_trailer state;
		  Lwt.return (`Stop state)
	      | true ->
		  session_established2 el state
	)

(*
%% We hibernate the process to reduce memory consumption after a
%% configurable activity timeout
session_established(timeout, StateData) ->
    %% TODO: Options must be stored in state:
    Options = [],
    proc_lib:hibernate(?GEN_FSM, enter_loop,
		       [?MODULE, Options, session_established, StateData]),
    fsm_next_state(session_established, StateData);
*)

      | `XmlStreamStart _ -> assert false

      | `XmlStreamEnd _ ->
	  send_trailer state;
	  Lwt.return (`Stop state)

      | `XmlStreamError "XML stanza is too big" -> (* TODO *)
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)
      | `XmlStreamError _ ->
	  send_element state invalid_xml_err;
	  send_trailer state;
	  Lwt.return (`Stop state)


  let handle_xml msg state =
    match state.state, msg with
      | Wait_for_stream, _ -> wait_for_stream msg state
      | Wait_for_auth, _ -> wait_for_auth msg state
      | Wait_for_feature_request, _ -> wait_for_feature_request msg state
      | Wait_for_bind, _ -> wait_for_bind msg state
      | Wait_for_session, _ -> wait_for_session msg state
      | Wait_for_sasl_response sasl_state, _ ->
	  wait_for_sasl_response msg sasl_state state
      | Session_established, _ -> session_established msg state
(*      | _, `XmlStreamStart (name, attrs) ->
          lwt () = Lwt_io.printf "stream start: %s %s\n"
            name (Xml.attrs_to_string attrs)
          in
            Lwt.return (`Continue state)
      | _, `XmlStreamElement el ->
          lwt () = Lwt_io.printf "stream el: %s\n"
            (Xml.element_to_string el)
          in
            Lwt.return (`Continue state)
      | _, `XmlStreamEnd name ->
          lwt () = Lwt_io.printf "stream end: %s\n" name in
            Lwt.return (`Continue state)
      | _, `XmlStreamError error ->
          lwt () = Lwt_io.printf "stream error: %s\n" error in
            Lwt.return (`Stop state)
*)

  let handle_route (`Route (from, to', packet)) state =
    let `XmlElement (name, attrs, els) = packet in
    let (pass, attrs, state) =
      match name with
	| "presence" -> (
		(*State = ejabberd_hooks:run_fold(
			  c2s_presence_in, StateData#state.server,
			  StateData,
			  [{From, To, Packet}]),*)
	    match Xml.get_attr_s "type" attrs with
	      | "probe" ->
		  let lfrom = Jlib.jid_tolower from in
		  let lbfrom =
		    Jlib.jid_tolower (Jlib.jid_remove_resource from)
		  in
		  let state =
		    if LJIDSet.mem lfrom state.pres_a ||
		      LJIDSet.mem lbfrom state.pres_a then (
			state
		      ) else (
			if LJIDSet.mem lfrom state.pres_f then (
			  let a = LJIDSet.add lfrom state.pres_a in
			    {state with pres_a = a}
			) else (
			  if LJIDSet.mem lbfrom state.pres_f then (
			    let a = LJIDSet.add lbfrom state.pres_a in
			      {state with pres_a = a}
			  ) else state
			)
		      )
		  in
		    process_presence_probe from to' state;
		    (false, attrs, state)
	      | "error" ->
		  let a = LJIDSet.remove (Jlib.jid_tolower from) state.pres_a
		  in
		    (true, attrs, {state with pres_a = a})
	      | "invisible" ->
		  let attrs = List.remove_assoc "type" attrs in
		    (true, ("type", "unavailable") :: attrs, state)
	      | "subscribe"
	      | "subscribed"
	      | "unsubscribe"
	      | "unsubscribed" ->
		  let sres = privacy_check_packet state from to' packet `In in
		    (sres, attrs, state)
	      | _ -> (
		  match privacy_check_packet state from to' packet `In with
		    | true -> (
			let lfrom = Jlib.jid_tolower from in
			let lbfrom =
			  Jlib.jid_tolower (Jlib.jid_remove_resource from)
			in
		    if LJIDSet.mem lfrom state.pres_a ||
		      LJIDSet.mem lbfrom state.pres_a then (
			(true, attrs, state)
		      ) else (
			if LJIDSet.mem lfrom state.pres_f then (
			  let a = LJIDSet.add lfrom state.pres_a in
			    (true, attrs, {state with pres_a = a})
			) else (
			  if LJIDSet.mem lbfrom state.pres_f then (
			    let a = LJIDSet.add lbfrom state.pres_a in
			      (true, attrs, {state with pres_a = a})
			  ) else (true, attrs, state)
			)
		      )
		      )
		    | false ->
			(false, attrs, state)
		)
	  )
	| "broadcast" -> (
		(*?DEBUG("broadcast~n~p~n", [Els]),
		case Els of
		    [{item, IJID, ISubscription}] ->
			{false, Attrs,
			 roster_change(IJID, ISubscription,
				       StateData)};
		    [{exit, Reason}] ->
			{exit, Attrs, Reason};
		    [{privacy_list, PrivList, PrivListName}] ->
			case ejabberd_hooks:run_fold(
			       privacy_updated_list, StateData#state.server,
			       false,
			       [StateData#state.privacy_list,
				PrivList]) of
			    false ->
				{false, Attrs, StateData};
			    NewPL ->
				PrivPushIQ =
				    #iq{type = set, xmlns = ?NS_PRIVACY,
					id = "push" ++ randoms:get_string(),
					sub_el = [{xmlelement, "query",
						   [{"xmlns", ?NS_PRIVACY}],
						   [{xmlelement, "list",
						     [{"name", PrivListName}],
						     []}]}]},
				PrivPushEl =
				    jlib:replace_from_to(
				      jlib:jid_remove_resource(
					StateData#state.jid),
				      StateData#state.jid,
				      jlib:iq_to_xml(PrivPushIQ)),
				send_element(StateData, PrivPushEl),
				{false, Attrs, StateData#state{privacy_list = NewPL}}
			end;
		    _ ->*)
	    (false, attrs, state)
	  )
	| "iq" -> (
	    match Jlib.iq_query_info packet with
	      (*| `IQ {Jlib.iq_xmlns = <:ns<LAST>>} ->
			LFrom = jlib:jid_tolower(From),
			LBFrom = jlib:jid_remove_resource(LFrom),
			HasFromSub = (?SETS:is_element(LFrom, StateData#state.pres_f) orelse ?SETS:is_element(LBFrom, StateData#state.pres_f))
			    andalso is_privacy_allow(StateData, To, From, {xmlelement, "presence", [], []}, out),
			case HasFromSub of
			    true ->
				case privacy_check_packet(StateData, From, To, Packet, in) of
				    allow ->
					{true, Attrs, StateData};
				    deny ->
					{false, Attrs, StateData}
				end;
			    _ ->
				Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
				ejabberd_router:route(To, From, Err),
				{false, Attrs, StateData}
			end;
	      *)
	      | (`IQ _ | `Reply) as iq -> (
		  match privacy_check_packet state from to' packet `In, iq with
		    | true, _ ->
			(true, attrs, state)
		    | false, `IQ _ ->
			let err =
			  Jlib.make_error_reply
			    packet Jlib.err_service_unavailable
			in
			  Router.route to' from err;
			  (false, attrs, state)
		    | false, `Reply ->
			(false, attrs, state)
		)
	      | `Invalid
	      | `Not_iq ->
		  (false, attrs, state)
	  )
	| "message" -> (
	    match privacy_check_packet state from to' packet `In with
	      | true -> (
		  (*case ejabberd_hooks:run_fold(
				       feature_check_packet, StateData#state.server,
				       allow,
				       [StateData#state.jid,
					StateData#state.server,
					StateData#state.pres_last,
					{From, To, Packet},
					in]) of
				    allow ->*)
		  (true, attrs, state)
				    (*deny ->
					{false, Attrs, StateData}
				end;*)
		)
	      | false ->
		  (false, attrs, state)
	  )
	| _ ->
	    (true, attrs, state)
    in
      (*if
	Pass == exit ->
	    catch send_trailer(StateData),
	    case NewState of
		rebind ->
		    {stop, normal, StateData#state{authenticated = rebinded}};
		_ ->
		    {stop, normal, StateData}
	    end;*)
      if pass then (
	let attrs =
	  Jlib.replace_from_to_attrs
	    (Jlib.jid_to_string from)
	    (Jlib.jid_to_string to')
	    attrs
	in
	let fixed_packet = `XmlElement (name, attrs, els) in
	  send_element state fixed_packet;
	(*ejabberd_hooks:run(user_receive_packet,
			       StateData#state.server,
			       [StateData#state.debug, StateData#state.jid, From, To, FixedPacket]),
	  ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),*)
	  fsm_next_state state
      ) else (
	(*ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),*)
	fsm_next_state state
      )

  let handle msg state =
    match msg with
      | `Tcp_data (socket, data) when socket == state.socket ->
          (*lwt () = Lwt_io.printf "tcp data %d %S\n" (String.length data) data in*)
            XMLReceiver.parse state.xml_receiver data;
            Tcp.activate state.socket state.pid;
            (*state.pid $! `Zxc (data, 1);*)
            Lwt.return (`Continue state)
      | `Tcp_data (_socket, _data) -> assert false
      | `Tcp_close socket when socket == state.socket ->
          lwt () = Lwt_io.printf "tcp close\n" in
            (*Gc.print_stat stdout;
            Gc.compact ();
            Gc.print_stat stdout; flush stdout;*)
            Lwt.return (`Stop state)
      | `Tcp_close _socket -> assert false
      | #XMLReceiver.msg as m ->
	  handle_xml m state
      | #SM.msg as m ->
	  handle_route m state
      | `Zxc (s, n) ->
          if n <= 1000000 then (
            Tcp.send_async state.socket (string_of_int n ^ s);
            state.pid $! `Zxc (s, n + 1)
          );
          Lwt_main.yield () >>
          Lwt.return (`Continue state)
      | #GenServer.msg -> assert false

  let terminate state =
    XMLReceiver.free state.xml_receiver;
    lwt () =
      if Tcp.state state.socket = Lwt_unix.Opened
      then Tcp.close state.socket
      else Lwt.return ()
    in
      (match state.state with
	 | Session_established -> (
	    (*case StateData#state.authenticated of
		replaced ->
		    ?INFO_MSG("(~w) Replaced session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(StateData#state.jid)]),
		    From = StateData#state.jid,
		    Packet = {xmlelement, "presence",
			      [{"type", "unavailable"}],
			      [{xmlelement, "status", [],
				[{xmlcdata, "Replaced by new connection"}]}]},
		    ejabberd_sm:close_session_unset_presence(
		      StateData#state.sid,
		      StateData#state.user,
		      StateData#state.server,
		      StateData#state.resource,
		      "Replaced by new connection"),
		    presence_broadcast(
		      StateData, From, StateData#state.pres_a, Packet),
		    presence_broadcast(
		      StateData, From, StateData#state.pres_i, Packet);
		_ ->*)
		    (*?INFO_MSG("(~w) Close session for ~s",
			      [StateData#state.socket,
			       jlib:jid_to_string(StateData#state.jid)]),*)
	     lwt () = Lwt_io.printf "Close session for %s\n" (Jlib.jid_to_string state.jid) in
	       (match state with
		  | {pres_last = None;
		     pres_a;
		     pres_i;
		     pres_invis = false; _} when (LJIDSet.is_empty pres_a &&
						    LJIDSet.is_empty pres_i) ->
		      SM.close_session
			state.sid state.user state.server state.resource;
		  | _ ->
		      let from = state.jid in
		      let packet =
			`XmlElement ("presence",
				     [("type", "unavailable")], [])
		      in
			SM.close_session_unset_presence
			  state.sid state.user state.server state.resource "";
			presence_broadcast state from state.pres_a packet;
			presence_broadcast state from state.pres_i packet
	       );
	       Lwt.return ()
	   )
	 | _ ->
	     Lwt.return ()
      );
      Lwt.return ()
end

module C2SServer = GenServer.Make(C2S)

module ModRoster :
sig
end
  =
struct

  type subscription =
    [ `None of [ `None | `Out | `In | `Both ]
    | `To of [ `None | `In ]
    | `From of [ `None | `Out ]
    | `Both ]

  type subscription_request =
    | Subscribe
    | Subscribed
    | Unsubscribe
    | Unsubscribed

  type +'a roster_item =
      {jid : string * string * string;
       name : string;
       subscription : [> subscription ] as 'a;
       groups : string list;
       askmessage : string;
      }

  let rosters = (Hashtbl.create 100
		   : (Jlib.nodepreped * Jlib.namepreped,
		      (LJID.t, subscription roster_item) Hashtbl.t) Hashtbl.t)

  let read_roster' u s =
    try
      Some (Hashtbl.find rosters (u, s))
    with
      | Not_found -> None

  let read_roster u s =
    try
      let roster = Hashtbl.find rosters (u, s) in
	Hashtbl.fold
	  (fun jid item acc -> (jid, item) :: acc) roster []
    with
      | Not_found -> []

  let delete_roster u s =
    Hashtbl.remove rosters (u, s)

  let read_roster_item u s jid =
    try
      let roster = Hashtbl.find rosters (u, s) in
	Some (Hashtbl.find roster jid)
    with
      | Not_found -> None

  let write_roster_item u s jid item =
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

  let delete_roster_item u s jid =
    let us = (u, s) in
      try
	let roster = Hashtbl.find rosters us in
	  Hashtbl.remove roster jid;
	  if Hashtbl.length roster = 0
	  then Hashtbl.remove rosters us
      with
	| Not_found -> ()




  let roster_hash items =
    string_of_int
      (Hashtbl.hash
	 (List.sort compare
	    (List.map
	       (fun (jid, item) ->
		  (jid, {item with groups = List.sort compare item.groups})
	       ) items)))
		
  let roster_versioning_enabled host =
    (* TODO *)
    false
    (*gen_mod:get_module_opt(Host, ?MODULE, versioning, false).*)

  let roster_version_on_db host =
    (* TODO *)
    false
    (*gen_mod:get_module_opt(Host, ?MODULE, store_current_id, false).*)

  (* Returns a list that may contain an xmlelement with the XEP-237 feature if it's enabled. *)
  let get_versioning_feature acc host =
    if roster_versioning_enabled host then (
      let feature = `XmlElement ("ver", [("xmlns", <:ns<ROSTER_VER>>)], []) in
	feature :: acc
    ) else []

  let roster_version lserver luser =
    (* TODO *)
    ""
(*
	US = {LUser, LServer},
	case roster_version_on_db(LServer) of
		true ->
			case mnesia:dirty_read(roster_version, US) of
				[#roster_version{version = V}] -> V;
				[] -> not_found
			end;
		false ->
			roster_hash(ejabberd_hooks:run_fold(roster_get, LServer, [], [US]))
	end.
*)

  let item_to_xml item =
    let (u, s, r) = item.jid in
    let attrs = [("jid", Jlib.jid_to_string' u s r)] in
    let attrs =
      match item.name with
	| "" -> attrs
	| name -> ("name", name) :: attrs
    in
    let attrs =
      match item.subscription with
	| `None _ -> ("subscription", "none")   :: attrs
	| `From _ -> ("subscription", "from")   :: attrs
	| `To _ ->   ("subscription", "to")     :: attrs
	| `Both ->   ("subscription", "both")   :: attrs
	| `Remove -> ("subscription", "remove") :: attrs
    in
    let attrs =
      let pending =
	match item.subscription with
	  | `None pending -> (pending :> [ `Both | `In | `None | `Out ])
	  | `From pending -> (pending :> [ `Both | `In | `None | `Out ])
	  | `To pending -> (pending :> [ `Both | `In | `None | `Out ])
	  | `Both
	  | `Remove -> `None
      in
	match pending with
	  | `Out -> ("ask", "subscribe") :: attrs
	  | `Both -> ("ask", "subscribe") :: attrs
	  | _ -> attrs
    in
    let subels =
      List.map
	(fun g -> `XmlElement ("group", [], [(`XmlCdata g)]))
	item.groups
    in
      (`XmlElement ("item", attrs, subels) : Xml.element)

  let item_to_xml' (_jid, item) = item_to_xml item

  let roster_get = Hooks.create_fold ()

(*
 Load roster from DB only if neccesary. 
 It is neccesary if
     - roster versioning is disabled in server OR
     - roster versioning is not used by the client OR
     - roster versioning is used by server and client, BUT the server isn't storing versions on db OR
     - the roster version from client don't match current version.
*)
  let process_iq_get from to' iq =
    let `Get subel = iq.Jlib.iq_type in
    let luser = from.Jlib.luser in
    let lserver = from.Jlib.lserver in
    let us = (luser, lserver) in
      try
	let to_send =
	  match Xml.get_tag_attr "ver" subel, 
	    roster_versioning_enabled lserver,
	    roster_version_on_db lserver with
	      | Some requested_version, true, true ->
		  (* Retrieve version from DB. Only load entire roster
		     when neccesary. *)
			(*case mnesia:dirty_read(roster_version, US) of
				[#roster_version{version = RequestedVersion}] ->
					{false, false};
				[#roster_version{version = NewVersion}] ->
					{lists:map(fun item_to_xml/1, 
						ejabberd_hooks:run_fold(roster_get, To#jid.lserver, [], [US])), NewVersion};
				[] ->*)
		  let roster_version =
		    string_of_int (Hashtbl.hash (Unix.gettimeofday ()))
		  in
		    (*mnesia:dirty_write(#roster_version{us = US, version = RosterVersion}),
					{lists:map(fun item_to_xml/1,
						ejabberd_hooks:run_fold(roster_get, To#jid.lserver, [], [US])), RosterVersion}*) (* TODO *)
		    Some ([], Some roster_version)

	      | Some requested_version, true, false ->
		  let roster_items =
		    Hooks.run_fold roster_get to'.Jlib.lserver [] us
		  in
		  let hash = roster_hash roster_items in
		    if hash = requested_version
		    then None
		    else Some (List.map item_to_xml' roster_items, Some hash)
			
	      | _ ->
		  let roster_items =
		    Hooks.run_fold roster_get to'.Jlib.lserver [] us
		  in
		    Some (List.map item_to_xml' roster_items, None)
	in
	let subel =
	  match to_send with
	    | None -> None
	    | Some (items, None) ->
		Some (`XmlElement ("query", [("xmlns", <:ns<ROSTER>>)],
				   (items :> Xml.element_cdata list)))
	    | Some (items, Some version) ->
		Some (`XmlElement ("query", [("xmlns", <:ns<ROSTER>>);
					     ("ver", version)],
				   (items :> Xml.element_cdata list)))
	in
	let subel = (subel) in
	  {iq with Jlib.iq_type = `Result subel}
      with
    	| _ ->
	    {iq with Jlib.iq_type = `Error (Jlib.err_internal_server_error,
					    Some subel)}

  let get_user_roster acc (u, s) =
    let items = read_roster u s in
    let items =
      List.filter
	(function
	   | (jid, {subscription = `None `In; _}) -> false
	   | _ -> true) items @ acc
    in
      (Hooks.OK, items)

  let rec process_item_attrs item =
    function
      | (attr, value) :: attrs -> (
	  match attr with
	    | "jid" -> (
		match Jlib.string_to_jid value with
		  | None ->
		      process_item_attrs item attrs
		  | Some jid1 ->
		      let jid =
			(jid1.Jlib.user, jid1.Jlib.server, jid1.Jlib.resource)
		      in
			process_item_attrs {item with jid} attrs
	      )
	    | "name" ->
		process_item_attrs {item with name = value} attrs
	    | "subscription" -> (
		match value with
		  | "remove" ->
		      process_item_attrs
			{item with subscription = `Remove} attrs
		  | _ ->
		      process_item_attrs item attrs
	      )
	    | "ask" ->
		process_item_attrs item attrs
	    | _ ->
		process_item_attrs item attrs
	)
      | [] ->
	  item


  let rec process_item_els item =
    function
      | `XmlElement (name, _attrs, sels) :: els -> (
	  match name with
	    | "group" ->
		let groups = Xml.get_cdata sels :: item.groups in
		  process_item_els {item with groups} els
	    | _ ->
		process_item_els item els
	)
      | `XmlCdata _ :: els ->
	  process_item_els item els
      | [] ->
	  item

  let push_item'' user server resource from item roster_version =
    let extra_attrs =
      match roster_version with
	| None -> []
	| Some roster_version -> [("ver", roster_version)]
    in
    let resiq =
      {Jlib.iq_type =
	  `Set (`XmlElement ("query",
			     ("xmlns", <:ns<ROSTER>>) :: extra_attrs,
			     [(item_to_xml item :> Xml.element_cdata)]));
       Jlib.iq_xmlns = <:ns<ROSTER>>;
       Jlib.iq_id = "push" ^ Jlib.get_random_string ();
       Jlib.iq_lang = "";
      }
    in
      Router.route
	from
	(Jlib.make_jid' user server resource)
	(Jlib.iq_to_xml resiq)

  (* TODO: don't push to those who didn't load roster *)
  let push_item' user server resource from item =
    push_item'' user server resource from item None


  let push_item user server from item =
(* TODO *)
    (*SM.route
      (Jlib.make_jid "" "" "")
      (jlib:make_jid' user server (Jlib.resourceprep_exn ""))
      (`XmlElement
	 ("broadcast", [],
	  [{item,
	    Item#roster.jid,
	    Item#roster.subscription}])),*)
(* TODO *)
  (*
    case roster_versioning_enabled(Server) of
	true ->
		push_item_version(Server, User, From, Item, roster_version(Server, User));
	false ->*)
    List.iter
      (fun resource ->
	 push_item' user server resource from item)
      (SM.get_user_resources user server)

(*
%% @doc Roster push, calculate and include the version attribute.
%% TODO: don't push to those who didn't load roster
push_item_version(Server, User, From, Item, RosterVersion)  ->
    lists:foreach(fun(Resource) ->
			  push_item(User, Server, Resource, From, Item, RosterVersion)
		end, ejabberd_sm:get_user_resources(User, Server)).
*)


  let send_presence_type from to' type' =
    Router.route from to'
      (`XmlElement ("presence", [("type", type')], []))

  let send_unsubscribing_presence from jid item =
    let is_to =
      match item.subscription with
	| `Both -> true
	| `To _ -> true
	| _ -> false
    in
    let is_from =
      match item.subscription with
	| `Both -> true
	| `From _ -> true
	| _ -> false
    in
      if is_to then (
	send_presence_type
	  (Jlib.jid_remove_resource from)
	  (Jlib.ljid_to_jid jid)
	  "unsubscribe"
      );
      if is_from then (
	send_presence_type
	  (Jlib.jid_remove_resource from)
	  (Jlib.ljid_to_jid jid)
	  "unsubscribed"
      )


  let process_item_set from to' =
    function
      | `XmlElement (_name, attrs, els) -> (
	  let jid = Jlib.string_to_jid (Xml.get_attr_s "jid" attrs) in
	  let {Jlib.luser = luser; Jlib.lserver = lserver; _} = from in
	    match jid with
	      | None ->
		  ()
	      | Some jid1 ->
		  let jid =
		    (jid1.Jlib.user, jid1.Jlib.server, jid1.Jlib.resource)
		  in
		  let ljid = Jlib.jid_tolower jid1 in
		  let f =
		    fun () ->
		      let res = read_roster_item luser lserver ljid in
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
			       delete_roster_item luser lserver ljid
			   | {subscription = #subscription; _} as item ->
			       write_roster_item luser lserver ljid item
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
			(item', item, ljid)
		  in
		    match f () with
		      | (old_item, item, ljid) -> (
			  push_item luser lserver to' item;
			  match item.subscription with
			    | `Remove ->
				send_unsubscribing_presence from ljid old_item
			    | _ ->
				()
			)
			  (*E ->
			    ?DEBUG("ROSTER: roster item set error: ~p~n", [E]),
			    ok*)
	)
      | `XmlCdata _ -> ()


  let process_iq_set from to' iq =
    let `Set subel = iq.Jlib.iq_type in
    let `XmlElement (_name, _attrs, els) = subel in
      List.iter (fun el -> process_item_set from to' el) els;
      {iq with Jlib.iq_type = `Result None}


  let process_local_iq from to' iq =
    match iq with
      | {Jlib.iq_type = `Set _; _} as iq ->
	  process_iq_set from to' iq
      | {Jlib.iq_type = `Get _; _} as iq ->
	  process_iq_get from to' iq

  let process_iq from to' iq =
    let (`Set sub_el | `Get sub_el) = iq.Jlib.iq_type in
    let lserver = from.Jlib.lserver in
      if List.mem lserver (myhosts ())
      then `IQ (process_local_iq from to' iq)
      else `IQ {iq with
		  Jlib.iq_type = `Error (Jlib.err_item_not_found, Some sub_el)}


  let rec fill_subscription_lists items f t =
    match items with
      | (j, i) :: is -> (
	  match i.subscription with
	    | `Both ->
		fill_subscription_lists is (j :: f) (j :: t)
	    | `From _ ->
		fill_subscription_lists is (j :: f) t
	    | `To _ ->
		fill_subscription_lists is f (j :: t)
	    | `None _ ->
		fill_subscription_lists is f t
	)
      | [] ->
	  (f, t)

  let get_subscription_lists _ (user, server) =
    let items = read_roster user server in
      (Hooks.OK, fill_subscription_lists items [] [])


  let in_state_change subscription t =
    match subscription, t with
      | `None `None, `Subscribe    -> Some (`None `In)
      | `None `None, `Subscribed   -> None
      | `None `None, `Unsubscribe  -> None
      | `None `None, `Unsubscribed -> None
      | `None `Out,  `Subscribe    -> Some (`None `Both)
      | `None `Out,  `Subscribed   -> Some (`To `None)
      | `None `Out,  `Unsubscribe  -> None
      | `None `Out,  `Unsubscribed -> Some (`None `None)
      | `None `In,   `Subscribe    -> None
      | `None `In,   `Subscribed   -> None
      | `None `In,   `Unsubscribe  -> Some (`None `None)
      | `None `In,   `Unsubscribed -> None
      | `None `Both, `Subscribe    -> None
      | `None `Both, `Subscribed   -> Some (`To `In)
      | `None `Both, `Unsubscribe  -> Some (`None `Out)
      | `None `Both, `Unsubscribed -> Some (`None `In)
      | `To   `None, `Subscribe    -> Some (`To `In)
      | `To   `None, `Subscribed   -> None
      | `To   `None, `Unsubscribe  -> None
      | `To   `None, `Unsubscribed -> Some (`None `None)
      | `To   `In,   `Subscribe    -> None
      | `To   `In,   `Subscribed   -> None
      | `To   `In,   `Unsubscribe  -> Some (`To `None)
      | `To   `In,   `Unsubscribed -> Some (`None `In)
      | `From `None, `Subscribe    -> None
      | `From `None, `Subscribed   -> Some `Both
      | `From `None, `Unsubscribe  -> Some (`None `None)
      | `From `None, `Unsubscribed -> None
      | `From `Out,  `Subscribe    -> None
      | `From `Out,  `Subscribed   -> Some `Both
      | `From `Out,  `Unsubscribe  -> Some (`None `Out)
      | `From `Out,  `Unsubscribed -> Some (`From `None)
      | `Both,       `Subscribe    -> None
      | `Both,       `Subscribed   -> None
      | `Both,       `Unsubscribe  -> Some (`To `None)
      | `Both,       `Unsubscribed -> Some (`From `None)

  let out_state_change subscription t =
    match subscription, t with
      | `None `None, `Subscribe    -> Some (`None `Out)
      | `None `None, `Subscribed   -> None
      | `None `None, `Unsubscribe  -> None
      | `None `None, `Unsubscribed -> None
      | `None `Out,  `Subscribe    -> Some (`None `Out)
      | `None `Out,  `Subscribed   -> None
      | `None `Out,  `Unsubscribe  -> Some (`None `None)
      | `None `Out,  `Unsubscribed -> None
      | `None `In,   `Subscribe    -> Some (`None `Both)
      | `None `In,   `Subscribed   -> Some (`From `None)
      | `None `In,   `Unsubscribe  -> None
      | `None `In,   `Unsubscribed -> Some (`None `None)
      | `None `Both, `Subscribe    -> None
      | `None `Both, `Subscribed   -> Some (`From `Out)
      | `None `Both, `Unsubscribe  -> Some (`None `In)
      | `None `Both, `Unsubscribed -> Some (`None `Out)
      | `To   `None, `Subscribe    -> None
      | `To   `None, `Subscribed   -> Some `Both
      | `To   `None, `Unsubscribe  -> Some (`None `None)
      | `To   `None, `Unsubscribed -> None
      | `To   `In,   `Subscribe    -> None
      | `To   `In,   `Subscribed   -> Some `Both
      | `To   `In,   `Unsubscribe  -> Some (`None `In)
      | `To   `In,   `Unsubscribed -> Some (`To `None)
      | `From `None, `Subscribe    -> Some (`From `Out)
      | `From `None, `Subscribed   -> None
      | `From `None, `Unsubscribe  -> None
      | `From `None, `Unsubscribed -> Some (`None `None)
      | `From `Out,  `Subscribe    -> None
      | `From `Out,  `Subscribed   -> None
      | `From `Out,  `Unsubscribe  -> Some (`From `None)
      | `From `Out,  `Unsubscribed -> Some (`None `Out)
      | `Both,       `Subscribe    -> None
      | `Both,       `Subscribed   -> None
      | `Both,       `Unsubscribe  -> Some (`From `None)
      | `Both,       `Unsubscribed -> Some (`To `None)

  let in_auto_reply subscription t =
    match subscription, t with
      | `From `None, `Subscribe   -> Some `Subscribed
      | `From `Out,  `Subscribe   -> Some `Subscribed
      | `Both,       `Subscribe   -> Some `Subscribed
      | `None `In,   `Unsubscribe -> Some `Unsubscribed
      | `None `Both, `Unsubscribe -> Some `Unsubscribed
      | `To   `In,   `Unsubscribe -> Some `Unsubscribed
      | `From `None, `Unsubscribe -> Some `Unsubscribed
      | `From `Out,  `Unsubscribe -> Some `Unsubscribed
      | `Both,       `Unsubscribe -> Some `Unsubscribed
      | _,           _            -> None


  let process_subscription direction luser lserver jid1 type' reason =
    let us = (luser, lserver) in
    let ljid = Jlib.jid_tolower jid1 in
    let f =
      fun() ->
	let item =
	  match read_roster_item luser lserver ljid with
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
	    | `Out -> out_state_change item.subscription type'
	    | `In -> in_state_change item.subscription type'
	in
	let autoreply =
	  match direction with
	    | `Out -> None
	    | `In -> in_auto_reply item.subscription type'
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
		(None, autoreply)
	    | Some (`None `None) when item.subscription = `None `In ->
		delete_roster_item luser lserver ljid;
		(None, autoreply)
	    | Some subscription ->
		let new_item =
		  {item with
		     subscription = subscription;
		     askmessage = ask_message}
		in
		  write_roster_item luser lserver ljid new_item;
			(*case roster_version_on_db(LServer) of
				true -> mnesia:write(#roster_version{us = {LUser, LServer}, version = sha:sha(term_to_binary(now()))});
				false -> ok
			end,*)
		  (Some new_item, autoreply)
    in
      match f () with
	| (push, auto_reply) -> (
	    (match auto_reply with
	       | None -> ()
	       | Some auto_reply ->
		   let t =
		     match auto_reply with
		       | `Subscribed -> "subscribed"
		       | `Unsubscribed -> "unsubscribed"
		   in
		     Router.route
		       (Jlib.make_jid'
			  luser lserver (Jlib.resourceprep_exn ""))
		       jid1
		       (`XmlElement ("presence", [("type", t)], []))
	    );
	    match push with
	      | Some item -> (
		    if item.subscription <> `None `In then (
		      push_item luser lserver
			(Jlib.make_jid'
			   luser lserver (Jlib.resourceprep_exn ""))
			item
		    );
		    true;
	  )
	      | None ->
		  false
	  )

  let in_subscription _ (user, server, jid, type', reason) =
    (Hooks.OK, process_subscription `In user server jid type' reason)

  let out_subscription (user, server, jid, type') =
    process_subscription `Out user server jid type' "";
    Hooks.OK


(*
remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    send_unsubscription_to_rosteritems(LUser, LServer),
    F = fun() ->
		lists:foreach(fun(R) ->
				      mnesia:delete_object(R)
			      end,
			      mnesia:index_read(roster, US, #roster.us))
        end,
    mnesia:transaction(F).

%% For each contact with Subscription:
%% Both or From, send a "unsubscribed" presence stanza;
%% Both or To, send a "unsubscribe" presence stanza.
send_unsubscription_to_rosteritems(LUser, LServer) ->
    RosterItems = get_user_roster([], {LUser, LServer}),
    From = jlib:make_jid({LUser, LServer, ""}),
    lists:foreach(fun(RosterItem) ->
			  send_unsubscribing_presence(From, RosterItem)
		  end,
		  RosterItems).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_items(User, Server, SubEl) ->
    {xmlelement, _Name, _Attrs, Els} = SubEl,
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    F = fun() ->
		lists:foreach(fun(El) ->
				      process_item_set_t(LUser, LServer, El)
			      end, Els)
	end,
    mnesia:transaction(F).

process_item_set_t(LUser, LServer, {xmlelement, _Name, Attrs, Els}) ->
    JID1 = jlib:string_to_jid(xml:get_attr_s("jid", Attrs)),
    case JID1 of
	error ->
	    ok;
	_ ->
	    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
	    LJID = {JID1#jid.luser, JID1#jid.lserver, JID1#jid.lresource},
	    Item = #roster{usj = {LUser, LServer, LJID},
			   us = {LUser, LServer},
			   jid = JID},
	    Item1 = process_item_attrs_ws(Item, Attrs),
	    Item2 = process_item_els(Item1, Els),
	    case Item2#roster.subscription of
		remove ->
		    mnesia:delete({roster, {LUser, LServer, LJID}});
		_ ->
		    mnesia:write(Item2)
	    end
    end;
process_item_set_t(_LUser, _LServer, _) ->
    ok.

process_item_attrs_ws(Item, [{Attr, Val} | Attrs]) ->
    case Attr of
	"jid" ->
	    case jlib:string_to_jid(Val) of
		error ->
		    process_item_attrs_ws(Item, Attrs);
		JID1 ->
		    JID = {JID1#jid.user, JID1#jid.server, JID1#jid.resource},
		    process_item_attrs_ws(Item#roster{jid = JID}, Attrs)
	    end;
	"name" ->
	    process_item_attrs_ws(Item#roster{name = Val}, Attrs);
	"subscription" ->
	    case Val of
		"remove" ->
		    process_item_attrs_ws(Item#roster{subscription = remove},
					  Attrs);
		"none" ->
		    process_item_attrs_ws(Item#roster{subscription = none},
					  Attrs);
		"both" ->
		    process_item_attrs_ws(Item#roster{subscription = both},
					  Attrs);
		"from" ->
		    process_item_attrs_ws(Item#roster{subscription = from},
					  Attrs);
		"to" ->
		    process_item_attrs_ws(Item#roster{subscription = to},
					  Attrs);
		_ ->
		    process_item_attrs_ws(Item, Attrs)
	    end;
	"ask" ->
	    process_item_attrs_ws(Item, Attrs);
	_ ->
	    process_item_attrs_ws(Item, Attrs)
    end;
process_item_attrs_ws(Item, []) ->
    Item.

get_in_pending_subscriptions(Ls, User, Server) ->
    JID = jlib:make_jid(User, Server, ""),
    US = {JID#jid.luser, JID#jid.lserver},
    case mnesia:dirty_index_read(roster, US, #roster.us) of
	Result when is_list(Result) ->
    	    Ls ++ lists:map(
		    fun(R) ->
			    Message = R#roster.askmessage,
			    Status  = if is_binary(Message) ->
					      binary_to_list(Message);
					 true ->
					      ""
				      end,
			    {xmlelement, "presence",
			     [{"from", jlib:jid_to_string(R#roster.jid)},
			      {"to", jlib:jid_to_string(JID)},
			      {"type", "subscribe"}],
			     [{xmlelement, "status", [],
			       [{xmlcdata, Status}]}]}
		    end,
		    lists:filter(
		      fun(R) ->
			      case R#roster.ask of
				  in   -> true;
				  both -> true;
				  _ -> false
			      end
		      end,
		      Result));
	_ ->
	    Ls
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_jid_info(_, User, Server, JID) ->
    LUser = jlib:nodeprep(User),
    LJID = jlib:jid_tolower(JID),
    LServer = jlib:nameprep(Server),
    case catch mnesia:dirty_read(roster, {LUser, LServer, LJID}) of
	[#roster{subscription = Subscription, groups = Groups}] ->
	    {Subscription, Groups};
	_ ->
	    LRJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
	    if
		LRJID == LJID ->
		    {none, []};
		true ->
		    case catch mnesia:dirty_read(
				 roster, {LUser, LServer, LRJID}) of
			[#roster{subscription = Subscription,
				 groups = Groups}] ->
			    {Subscription, Groups};
			_ ->
			    {none, []}
		    end
	    end
    end.
*)
(*
webadmin_page(_, Host,
	      #request{us = _US,
		       path = ["user", U, "roster"],
		       q = Query,
		       lang = Lang} = _Request) ->
    Res = user_roster(U, Host, Query, Lang),
    {stop, Res};

webadmin_page(Acc, _, _) -> Acc.

user_roster(User, Server, Query, Lang) ->
    US = {jlib:nodeprep(User), jlib:nameprep(Server)},
    Items1 = mnesia:dirty_index_read(roster, US, #roster.us),
    Res = user_roster_parse_query(User, Server, Items1, Query),
    Items = mnesia:dirty_index_read(roster, US, #roster.us),
    SItems = lists:sort(Items),
    FItems =
	case SItems of
	    [] ->
		[?CT("None")];
	    _ ->
		[?XE("table",
		     [?XE("thead",
			  [?XE("tr",
			       [?XCT("td", "Jabber ID"),
				?XCT("td", "Nickname"),
				?XCT("td", "Subscription"),
				?XCT("td", "Pending"),
				?XCT("td", "Groups")
			       ])]),
		      ?XE("tbody",
			  lists:map(
			    fun(R) ->
				    Groups =
					lists:flatmap(
					  fun(Group) ->
						  [?C(Group), ?BR]
					  end, R#roster.groups),
				    Pending = ask_to_pending(R#roster.ask),
				    TDJID = build_contact_jid_td(R#roster.jid),
				    ?XE("tr",
					[TDJID,
					 ?XAC("td", [{"class", "valign"}],
					      R#roster.name),
					 ?XAC("td", [{"class", "valign"}],
					      atom_to_list(R#roster.subscription)),
					 ?XAC("td", [{"class", "valign"}],
					      atom_to_list(Pending)),
					 ?XAE("td", [{"class", "valign"}], Groups),
					 if
					     Pending == in ->
						 ?XAE("td", [{"class", "valign"}],
						      [?INPUTT("submit",
							       "validate" ++
							       ejabberd_web_admin:term_to_id(R#roster.jid),
							       "Validate")]);
					     true ->
						 ?X("td")
					 end,
					 ?XAE("td", [{"class", "valign"}],
					      [?INPUTT("submit",
						       "remove" ++
						       ejabberd_web_admin:term_to_id(R#roster.jid),
						       "Remove")])])
			    end, SItems))])]
	end,
    [?XC("h1", ?T("Roster of ") ++ us_to_list(US))] ++
	case Res of
	    ok -> [?XREST("Submitted")];
	    error -> [?XREST("Bad format")];
	    nothing -> []
	end ++
	[?XAE("form", [{"action", ""}, {"method", "post"}],
	      FItems ++
	      [?P,
	       ?INPUT("text", "newjid", ""), ?C(" "),
	       ?INPUTT("submit", "addjid", "Add Jabber ID")
	      ])].

build_contact_jid_td(RosterJID) ->
    %% Convert {U, S, R} into {jid, U, S, R, U, S, R}:
    ContactJID = jlib:make_jid(RosterJID),
    JIDURI = case {ContactJID#jid.luser, ContactJID#jid.lserver} of
		 {"", _} -> "";
		 {CUser, CServer} ->
		     case lists:member(CServer, ?MYHOSTS) of
			 false -> "";
			 true -> "/admin/server/" ++ CServer ++ "/user/" ++ CUser ++ "/"
		     end
	     end,
    case JIDURI of
	[] ->
	    ?XAC("td", [{"class", "valign"}], jlib:jid_to_string(RosterJID));
	URI when is_list(URI) ->
	    ?XAE("td", [{"class", "valign"}], [?AC(JIDURI, jlib:jid_to_string(RosterJID))])
    end.

user_roster_parse_query(User, Server, Items, Query) ->
    case lists:keysearch("addjid", 1, Query) of
	{value, _} ->
	    case lists:keysearch("newjid", 1, Query) of
		{value, {_, undefined}} ->
		    error;
		{value, {_, SJID}} ->
		    case jlib:string_to_jid(SJID) of
			JID when is_record(JID, jid) ->
			    user_roster_subscribe_jid(User, Server, JID),
			    ok;
			error ->
			    error
		    end;
		false ->
		    error
	    end;
	false ->
	    case catch user_roster_item_parse_query(
			 User, Server, Items, Query) of
		submitted ->
		    ok;
		{'EXIT', _Reason} ->
		    error;
		_ ->
		    nothing
	    end
    end.


user_roster_subscribe_jid(User, Server, JID) ->
    out_subscription(User, Server, JID, subscribe),
    UJID = jlib:make_jid(User, Server, ""),
    ejabberd_router:route(
      UJID, JID, {xmlelement, "presence", [{"type", "subscribe"}], []}).

user_roster_item_parse_query(User, Server, Items, Query) ->
    lists:foreach(
      fun(R) ->
	      JID = R#roster.jid,
	      case lists:keysearch(
		     "validate" ++ ejabberd_web_admin:term_to_id(JID), 1, Query) of
		  {value, _} ->
		      JID1 = jlib:make_jid(JID),
		      out_subscription(
			User, Server, JID1, subscribed),
		      UJID = jlib:make_jid(User, Server, ""),
		      ejabberd_router:route(
			UJID, JID1, {xmlelement, "presence",
				     [{"type", "subscribed"}], []}),
		      throw(submitted);
		  false ->
		      case lists:keysearch(
			     "remove" ++ ejabberd_web_admin:term_to_id(JID), 1, Query) of
			  {value, _} ->
			      UJID = jlib:make_jid(User, Server, ""),
			      process_iq(
				UJID, UJID,
				#iq{type = set,
				    sub_el = {xmlelement, "query",
					      [{"xmlns", ?NS_ROSTER}],
					      [{xmlelement, "item",
						[{"jid", jlib:jid_to_string(JID)},
						 {"subscription", "remove"}],
						[]}]}}),
			      throw(submitted);
			  false ->
			      ok
		      end

	      end
      end, Items),
    nothing.

us_to_list({User, Server}) ->
    jlib:jid_to_string({User, Server, ""}).

webadmin_user(Acc, _User, _Server, Lang) ->
    Acc ++ [?XE("h3", [?ACT("roster/", "Roster")])].
*)

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
      write_roster_item test.Jlib.luser test.Jlib.lserver
	(test10.Jlib.luser, test10.Jlib.lserver, test10.Jlib.lresource)
	i1;
      write_roster_item test10.Jlib.luser test10.Jlib.lserver
	(test.Jlib.luser, test.Jlib.lserver, test.Jlib.lresource)
	i2;
      ()

  let _ =
    let host = Jlib.nameprep_exn "e.localhost" in
      Hooks.add_fold roster_get host get_user_roster 50;
      Hooks.add_fold SM.roster_in_subscription host in_subscription 50;
      Hooks.add C2S.roster_out_subscription host out_subscription 50;
      Hooks.add_fold C2S.roster_get_subscription_lists host
	get_subscription_lists 50;
    (*ejabberd_hooks:add(roster_get_jid_info, Host,
		       ?MODULE, get_jid_info, 50),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(resend_subscription_requests_hook, Host,
		       ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:add(roster_get_versioning_feature, Host,
		       ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:add(webadmin_page_host, Host,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host,
		       ?MODULE, webadmin_user, 50),*)
      GenIQHandler.add_iq_handler `SM host <:ns<ROSTER>> process_iq ()

(*
  start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(roster,[{disc_copies, [node()]},
				{attributes, record_info(fields, roster)}]),
    mnesia:create_table(roster_version, [{disc_copies, [node()]},
    				{attributes, record_info(fields, roster_version)}]),

    update_table(),
    mnesia:add_table_index(roster, us),
    mnesia:add_table_index(roster_version, us),
    ejabberd_hooks:add(roster_get, Host,
		       ?MODULE, get_user_roster, 50),
    ejabberd_hooks:add(roster_in_subscription, Host,
		       ?MODULE, in_subscription, 50),
    ejabberd_hooks:add(roster_out_subscription, Host,
		       ?MODULE, out_subscription, 50),
    ejabberd_hooks:add(roster_get_subscription_lists, Host,
		       ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:add(roster_get_jid_info, Host,
		       ?MODULE, get_jid_info, 50),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(anonymous_purge_hook, Host,
		       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(resend_subscription_requests_hook, Host,
		       ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:add(roster_get_versioning_feature, Host,
		       ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:add(webadmin_page_host, Host,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host,
		       ?MODULE, webadmin_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ROSTER,
				  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(roster_get, Host,
			  ?MODULE, get_user_roster, 50),
    ejabberd_hooks:delete(roster_in_subscription, Host,
			  ?MODULE, in_subscription, 50),
    ejabberd_hooks:delete(roster_out_subscription, Host,
			  ?MODULE, out_subscription, 50),
    ejabberd_hooks:delete(roster_get_subscription_lists, Host,
			  ?MODULE, get_subscription_lists, 50),
    ejabberd_hooks:delete(roster_get_jid_info, Host,
			  ?MODULE, get_jid_info, 50),
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(anonymous_purge_hook, Host,
			  ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(resend_subscription_requests_hook, Host,
			  ?MODULE, get_in_pending_subscriptions, 50),
    ejabberd_hooks:delete(roster_get_versioning_feature, Host,
		          ?MODULE, get_versioning_feature, 50),
    ejabberd_hooks:delete(webadmin_page_host, Host,
			  ?MODULE, webadmin_page, 50),
    ejabberd_hooks:delete(webadmin_user, Host,
			  ?MODULE, webadmin_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ROSTER).
*)



end


let rec accept listen_socket =
  lwt (socket, _) = Lwt_unix.accept listen_socket in
    (*ignore (spawn (C2S.start socket));*)
    ignore (C2SServer.start socket);
    accept listen_socket

let listener_start () =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_INET (Unix.inet_addr_any, 5222) in
    Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
    Lwt_unix.bind socket addr;
    Lwt_unix.listen socket 1024;
    accept socket

let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let main () =
  let _ = listener_start () in
    exit_waiter

let () = Lwt_main.run (main ())
