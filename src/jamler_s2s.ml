open Process

let section = Jamler_log.new_section "s2s"

module LJID = Jlib.LJID
module Hooks = Jamler_hooks
module Auth = Jamler_auth
module Router = Jamler_router
module GenIQHandler = Jamler_gen_iq_handler
module Config = Jamler_config
module S2SOut = Jamler_s2s_out.S2SOut
module S2SOutServer = Jamler_s2s_out.S2SOutServer

type msg = Router.msg

module S2S :
sig

  val route : Jlib.jid -> Jlib.jid -> Xml.element -> unit

  val s2s_send_packet : (Jlib.jid * Jlib.jid * Xml.element) Hooks.hook

end =
struct
  let s2s_send_packet = Hooks.create ()

  let choose_pid from pids =
    (* Pids1 = case [P || P <- Pids, node(P) == node()] of
                [] -> Pids;
                Ps -> Ps
            end,
    % Use sticky connections based on the JID of the sender (whithout
    % the resource to ensure that a muc room always uses the same
    % connection)
    Pid = lists:nth(erlang:phash(jlib:jid_remove_resource(From), length(Pids1)),
                    Pids1),
    ?DEBUG("Using ejabberd_s2s_out ~p~n", [Pid]), *)
    match pids with
      | pid :: _ -> Some pid
      | _ -> None

  let choose_connection from connections =
    choose_pid from (List.map (fun (_, p) -> p) connections)

  open Jamler_s2s_lib

  let new_connection
      myserver server from from_to
      max_s2s_connections_number max_s2s_connections_number_per_node =
    let key = new_key () in
    let pid = S2SOutServer.start (myserver, server, `New key) in
    let s2s_list = find_s2s_list from_to in
    let needed_connections =
      Jamler_s2s_lib.needed_connections_number
	s2s_list
	max_s2s_connections_number
	max_s2s_connections_number_per_node in
    let pid' = match needed_connections with
      | n when n > 0 ->
	Hashtbl.replace s2s_table from_to ((key, (pid :> s2s_out_msg Process.pid)) :: s2s_list);
	(pid :> s2s_out_msg pid)
      | _ ->
	match choose_connection from s2s_list with
	  | Some p -> p
	  | None -> (pid :> s2s_out_msg pid)
    in
      if (pid :> s2s_out_msg pid) == pid'
      then S2SOut.start_connection (pid :> s2s_out_msg pid)
      else S2SOut.stop_connection (pid :> s2s_out_msg pid);
    pid'

  let open_several_connections
      n myserver server from from_to
      max_s2s_connections_number max_s2s_connections_number_per_node =
    let connections_result =
      List.map (fun _ ->
	new_connection
	  myserver server from from_to
	  max_s2s_connections_number max_s2s_connections_number_per_node)
	(Array.to_list (Array.init n ((+) 1)))
    in choose_pid from connections_result

  let find_connection from to' =
    let myserver = from.Jlib.lserver in
    let server = to'.Jlib.lserver in
    let from_to = (myserver, server) in
    let max_s2s_connections_number = max_s2s_connections_number from_to in
    let max_s2s_connections_number_per_node =
      max_s2s_connections_number_per_node from_to in
    (* lwt () = Lwt_log.debug_f ~section
	  "Finding connection for %s -> %s"
	  (myserver:>string) (server:>string) in Lwt.return (); *)
    match find_s2s_list from_to with
      | [] ->
	if not (is_service from to') && (allow_host myserver server) then (
	  let needed_connections =
	    needed_connections_number
	      []
	      max_s2s_connections_number
	      max_s2s_connections_number_per_node in
	  open_several_connections
	    needed_connections myserver server from from_to
	    max_s2s_connections_number max_s2s_connections_number_per_node
	) else
	  None
      | s2s_list ->
	let needed_connections =
	    needed_connections_number
	      s2s_list
	      max_s2s_connections_number
	      max_s2s_connections_number_per_node in
	if needed_connections > 0 then (
	  (* We establish the missing connections for this pair *)
	  open_several_connections
	    needed_connections myserver server from from_to
	    max_s2s_connections_number max_s2s_connections_number_per_node
	) else
	  choose_connection from s2s_list

  let send_element pid el =
    pid $! (`Send_element el)

  let do_route from to' packet =
    (* ?DEBUG("s2s manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
       [From, To, Packet, 8]), *)
    match find_connection from to' with
      | Some pid ->
	(* ?DEBUG("sending to process ~p~n", [Pid]), *)
	let `XmlElement (name, attrs, els) = packet in
	let new_attrs = Jlib.replace_from_to_attrs
	  (Jlib.jid_to_string from) (Jlib.jid_to_string to') attrs in
	let myserver = from.Jlib.lserver in
	Hooks.run s2s_send_packet myserver (from, to', packet);
	send_element pid (`XmlElement (name, new_attrs, els));
      | None -> (
	match Xml.get_tag_attr_s "type" packet with
	  | "error"
	  | "result" -> ()
	  | _ ->
	    let err = Jlib.make_error_reply
	      packet Jlib.err_service_unavailable in
	    Router.route to' from err)

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

  let () =
    Jamler_router.register_s2s_route route
end

