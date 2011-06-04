type -'a pid

type 'a proc = {queue : 'a Queue.t;
		mutable wakener : 'a Lwt.u option}

external pid_to_proc : 'a pid -> 'a proc = "%identity"
external proc_to_pid : 'a proc -> 'a pid = "%identity"

let spawn f =
  let proc = {queue = Queue.create ();
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
  val string_to_jid_exn : string -> jid
  val string_to_jid : string -> jid option
  val jid_to_string : jid -> string

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

  type 'a iq =
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

  val decode_base64 : string -> string
  val encode_base64 : string -> string
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

  let jid_to_string {user; server; resource; _} =
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

  let decode_base64 s =
    let t = Cryptokit.Base64.decode () in
      Cryptokit.transform_string t s

  let encode_base64 s =
    let t = Cryptokit.Base64.encode_compact () in
      Cryptokit.transform_string t s

end

module Auth :
sig
  val check_password_with_authmodule :
    Jlib.nodepreped -> Jlib.namepreped -> string -> string option
  val check_password_digest_with_authmodule :
    Jlib.nodepreped -> Jlib.namepreped ->
    string -> string -> string -> string option
  val get_password_with_authmodule :
    Jlib.nodepreped -> Jlib.namepreped -> (string * string) option
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
end


module SASL =
struct
  type get_password = Jlib.nodepreped -> (string * string) option
  type check_password = Jlib.nodepreped -> string -> string option
  type check_password_digest =
      Jlib.nodepreped -> string -> string -> string -> string option

  type props = ([ `Username | `Auth_module | `Authzid ] * string) list

  type step_result =
    | Done of props
    | Continue of string * (string -> step_result)
    | ErrorUser of string * string
    | Error of string

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


module C2S :
sig
  type msg = [ Tcp.msg | XMLReceiver.msg | GenServer.msg | `Zxc of string * int ]
  type init_data = Lwt_unix.file_descr
  type state
  val init : init_data -> msg pid -> state
  val handle : msg -> state -> state GenServer.result
  val terminate : state -> unit Lwt.t
end =
struct
  type msg = [ Tcp.msg | XMLReceiver.msg | GenServer.msg | `Zxc of string * int ]

  type c2s_state =
    | Wait_for_stream
    | Wait_for_auth
    | Wait_for_feature_request
    | Wait_for_bind
    | Wait_for_session
    | Wait_for_sasl_response of (string -> SASL.step_result)
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
       (*sid : string;
       pres_t = ?SETS:new(),
       pres_f = ?SETS:new(),
       pres_a = ?SETS:new(),
       pres_i = ?SETS:new(),
       pres_last, pres_pri,
       pres_timestamp,
       pres_invis = false,
       privacy_list = #userlist{},
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

  let send_text state text =
    Printf.printf "Send XML on stream = %S\n" text; flush stdout;
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

  let new_id () =			(* TODO *)
    string_of_int (Random.int 1000000000)

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
                                      (*SID = {now(), self()},
					Conn = get_conn_type(StateData),*)
                                      let res = Jlib.make_result_iq_reply el in
					(*Res = setelement(4, Res1, []),*)
					send_element state res;
					(*change_shaper(StateData, JID),
					  {Fs, Ts} = ejabberd_hooks:run_fold(
                                          roster_get_subscription_lists,
                                          StateData#state.server,
                                          {[], []},
                                          [U, StateData#state.server]),
					  LJID = jlib:jid_tolower(
                                          jlib:jid_remove_resource(JID)),
					  Fs1 = [LJID | Fs],
					  Ts1 = [LJID | Ts],
					  PrivList =
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
                                             (*sid = SID,
                                               conn = Conn,
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
					  (*maybe_migrate(session_established,
                                                  NewStateData#state{debug=DebugFlag})*)
					  Lwt.return
					    (`Continue
					       {state with
						  state = Session_established})
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
    Printf.printf "aaaaaaaaaaaassssssssssssdddddddddd0\n"; flush stdout;
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
		  Printf.printf "aaaaaaaaaaaassssssssssssdddddddddd\n"; flush stdout;
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


  let handle_xml msg state =
    match state.state, msg with
      | Wait_for_stream, _ -> wait_for_stream msg state
      | Wait_for_auth, _ -> wait_for_auth msg state
      | Wait_for_feature_request, _ -> wait_for_feature_request msg state
      | _, `XmlStreamStart (name, attrs) ->
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

  let handle msg state =
    match msg with
      | `Tcp_data (socket, data) when socket == state.socket ->
          lwt () = Lwt_io.printf "tcp data %d %S\n" (String.length data) data in
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
      Lwt.return ()
end

module C2SServer = GenServer.Make(C2S)


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
