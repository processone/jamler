module Router = Jamler_router

let src = Jamler_log.new_src "gen_iq_handler"

type component = [ `SM | `Local ]
type response = [ `IQ of Jlib.iq_response Jlib.iq | `Ignore ]

let handlers =
  (Hashtbl.create 10
     : (component * Jlib.namepreped * string,
        Jlib.jid -> Jlib.jid -> Jlib.iq_query Jlib.iq -> response)
     Hashtbl.t)

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
      	 (Jlib.iq_to_xml (iq :> Jlib.iq_query_response Jlib.iq));
    | `Ignore ->
       ()
  with
  | exn ->
     Logs.err ~src
       (fun m ->
         m "exception when processing packet\nfrom: %s\nto: %s\npacket: %s\n%a"
           (Jlib.jid_to_string from)
           (Jlib.jid_to_string to')
	   (Xml.element_to_string
	      (Jlib.iq_to_xml (iq :> Jlib.iq_query_response Jlib.iq)))
           Jamler_log.pp_exn exn);
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
        let () = process_iq host f from to' iq in
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

