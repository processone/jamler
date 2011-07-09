open Process

let section = Jamler_log.new_section "router"

module Router = Jamler_router
module SM = Jamler_sm
module GenIQHandler = Jamler_gen_iq_handler

let process_iq from to' packet =
  match Jlib.iq_query_info packet with
    | `IQ ({Jlib.iq_xmlns = xmlns; _} as iq) -> (
        let host = to'.Jlib.lserver in
	lwt handle_res =
	  GenIQHandler.handle `Local host xmlns from to' iq
	in
          if not handle_res then (
            let err =
      	      Jlib.make_error_reply packet Jlib.err_service_unavailable
            in
      	      Router.route to' from err
          );
	  Lwt.return ()
      )
    | `Reply ->
        (*IQReply = jlib:iq_query_or_response_info(Packet),
          process_iq_reply(From, To, IQReply);*)
	Lwt.return ()
    | _ ->
        let err =
          Jlib.make_error_reply packet Jlib.err_bad_request
        in
          Router.route to' from err;
	  Lwt.return ()


let do_route from to' packet =
  (*?DEBUG("local route~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
         [From, To, Packet, 8]),*)
  if (to'.Jlib.luser :> string) <> ""
  then SM.route from to' packet
  else if (to'.Jlib.lresource :> string) = "" then (
    let `XmlElement (name, _attrs, _els) = packet in
      match name with
        | "iq" ->
            ignore (process_iq from to' packet)
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
	ignore (
	  Lwt_log.error_f
	    ~section
	    ~exn:exn
	    "exception when processing packet\nfrom: %s\nto: %s\npacket: %s\nexception"
            (Jlib.jid_to_string from)
            (Jlib.jid_to_string to')
            (Xml.element_to_string packet)
	)

let start () =
  let pid = spawn (fun s -> Lwt.return ()) in
    List.iter
      (fun host ->
         Router.register_route ~local_hint:(Some route) host pid
           (*ejabberd_hooks:add(local_send_to_resource_hook, Host,
             ?MODULE, bounce_resource_packet, 100)*)
      ) (Jamler_config.myhosts ())


