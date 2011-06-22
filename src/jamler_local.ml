open Process
module Router = Jamler_router
module SM = Jamler_sm

let myhosts () =
  List.map Jlib.nameprep_exn ["localhost"; "e.localhost"] (* TODO *)

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


