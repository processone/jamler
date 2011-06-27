open Process

let section = Jamler_log.new_section "router"

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
  ignore (
    Lwt_log.debug_f
      ~section
      "Route\nfrom: %s\n to: %s\npacket: %s"
      (Jlib.jid_to_string orig_from)
      (Jlib.jid_to_string orig_to)
      (Xml.element_to_string orig_packet)
  );
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
	ignore (
	  Lwt_log.error_f
	    ~section
	    ~exn:exn
	    "Exception when processing packet\nfrom: %s\nto: %s\npacket: %s\nexception"
            (Jlib.jid_to_string from)
            (Jlib.jid_to_string to')
            (Xml.element_to_string packet)
	)

let dirty_get_all_domains () =
  Hashtbl.fold (fun route _ acc -> route :: acc) route_table []
