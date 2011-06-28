open Process

let section = Jamler_log.new_section "s2s"

module LJID = Jlib.LJID
module Hooks = Jamler_hooks
module Auth = Jamler_auth
module Router = Jamler_router
module GenIQHandler = Jamler_gen_iq_handler
module Config = Jamler_config
module S2SOut = Jamler_s2s_out

type msg = Router.msg

module S2S :
sig
  type s2s = string * msg pid
  type fromto = Jlib.namepreped * Jlib.namepreped
  type s2s_table = (fromto, s2s list) Hashtbl.t

  val route : Jlib.jid -> Jlib.jid -> Xml.element -> unit
  val have_connection : fromto -> bool
  val has_key : fromto -> string -> bool
  val get_connections_pids : fromto -> msg pid list
  val try_register : fromto -> msg pid -> string option
  val remove_connection : fromto -> msg pid -> string -> unit
  val find_connection : Jlib.jid -> Jlib.jid -> msg pid option
  val dirty_get_connections: unit -> fromto list
  val allow_host : Jlib.namepreped -> Jlib.namepreped -> bool
  val is_service : Jlib.jid -> Jlib.jid -> bool

  val s2s_send_packet : (Jlib.jid * Jlib.jid * Xml.element) Hooks.hook

end =
struct
  type s2s = string * msg pid
  type fromto = Jlib.namepreped * Jlib.namepreped
  type s2s_table = (fromto, s2s list) Hashtbl.t

  let s2s_table = Hashtbl.create 10

  let default_max_s2s_connections_number = 1
  let default_max_s2s_connections_number_per_node = 1

  let s2s_send_packet = Hooks.create ()

  let new_key () =
    Jlib.get_random_string ()

  let find_s2s_list from_to =
    try
      Hashtbl.find s2s_table from_to
    with
      | Not_found -> []

  let parent_domains domain =
    List.fold_right
      (fun label acc ->
	match acc with
	  | [] -> [label]
	  | head :: tail ->
	    [label ^ "." ^ head; head] @ tail
      ) (Str.split (Str.regexp_string "\\.") domain) []

  let is_service from to' =
    let lfrom = from.Jlib.lserver in
    (* case ejabberd_config:get_local_option({route_subdomains, LFromDomain}) of *)
    match "s2s" with
      | "s2s" -> false
      | _ ->
	let hosts = Config.myhosts () in
	let pdomains = parent_domains (to'.Jlib.lserver :> string) in
	let rec is_service_rec = function
	  | pdomain :: xs ->
	    if List.mem pdomain (hosts :> string list) then true
	    else is_service_rec xs
	  | [] -> false in
	is_service_rec pdomains

  let allow_host' myhost s2shost =
    (* case ejabberd_config:get_local_option({{s2s_host, S2SHost}, MyHost}) of
        deny -> false;
        allow -> true;
        _ ->
            case ejabberd_config:get_local_option({s2s_default_policy, MyHost}) of
                deny -> false;
                _ ->
                    case ejabberd_hooks:run_fold(s2s_allow_host, MyHost,
                                                 allow, [MyHost, S2SHost]) of
                        deny -> false;
                        allow -> true;
                        _ -> true
                    end
            end
    end. *)
    true

  let allow_host (myserver:Jlib.namepreped) (s2shost:Jlib.namepreped) =
    (* Check if host is in blacklist or white list *)
    let hosts = Config.myhosts () in
    let rec allow_host_rec = function
      | pdomain :: xs when not (List.mem pdomain (hosts :> string list)) ->
	allow_host_rec xs
      | xs ->
	xs
    in
    match allow_host_rec (parent_domains (myserver :> string)) with
      | myhost :: _ ->
	allow_host' myhost s2shost
      | [] ->
	allow_host' myserver s2shost

  let have_connection from_to =
    Hashtbl.mem s2s_table from_to

  let remove_connection from_to pid key =
    let s2s_list =
      List.filter
	(function
	  | (k, p) when (k = key && p = pid) -> false
	  | _ -> true)
	(find_s2s_list from_to)
    in match s2s_list with
      | [] ->
	Hashtbl.remove s2s_table from_to
      | _ ->
	Hashtbl.replace s2s_table from_to s2s_list

  let has_key from_to key =
    match find_s2s_list from_to with
      | [] -> false
      | xs -> List.mem_assoc key xs

  let get_connections_pids from_to =
    List.map (fun (_, pid) -> pid) (find_s2s_list from_to)

  let max_s2s_connections_number (from, to') =
    (* case acl:match_rule(
           From, max_s2s_connections, jlib:make_jid("", To, "")) of
        Max when is_integer(Max) -> Max;
        _ -> ?DEFAULT_MAX_S2S_CONNECTIONS_NUMBER
    end. *)
    default_max_s2s_connections_number

  let max_s2s_connections_number_per_node (from, to') =
    (* case acl:match_rule(
           From, max_s2s_connections_per_node, jlib:make_jid("", To, "")) of
        Max when is_integer(Max) -> Max;
        _ -> ?DEFAULT_MAX_S2S_CONNECTIONS_NUMBER_PER_NODE
    end. *)
    default_max_s2s_connections_number_per_node

  let needed_connections_number
      (s2s_list : s2s list)
      max_s2s_connections_number
      max_s2s_connections_number_per_node =
    (* LocalLs = [L || L <- Ls, node(L#s2s.pid) == node()],
       lists:min([MaxS2SConnectionsNumber - length(Ls),
               MaxS2SConnectionsNumberPerNode - length(LocalLs)]). *)
    (* TODO *)
    let local_s2s_list = s2s_list in
    min (max_s2s_connections_number - List.length s2s_list)
      (max_s2s_connections_number_per_node - List.length local_s2s_list)

  let try_register from_to self =
    let key = new_key () in
    let max_s2s_connections_number = max_s2s_connections_number from_to in
    let max_s2s_connections_number_per_node =
      max_s2s_connections_number_per_node from_to in
    let s2s_list = find_s2s_list from_to in
    let needed_connections =
      needed_connections_number
	s2s_list
	max_s2s_connections_number
	max_s2s_connections_number_per_node in
    if needed_connections > 0 then (
      Hashtbl.replace s2s_table from_to ((key, self) :: s2s_list);
      Some key
    ) else None

  let dirty_get_connections () =
    Hashtbl.fold (fun from_to _ acc -> from_to :: acc) s2s_table []

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

  let new_connection
      myserver server from from_to
      max_s2s_connections_number max_s2s_connections_number_per_node =
    let key = new_key () in
    let pid = S2SOut.start myserver server key in
    let s2s_list = find_s2s_list from_to in
    let needed_connections =
      needed_connections_number
	s2s_list
	max_s2s_connections_number
	max_s2s_connections_number_per_node in
    let pid' = match needed_connections with
      | n when n > 0 ->
	Hashtbl.replace s2s_table from_to ((key, pid) :: s2s_list);
	pid
      | _ ->
	match choose_connection from s2s_list with
	  | Some p -> p
	  | None -> pid
    in if pid = pid' then S2SOut.start_connection pid
      else S2SOut.stop_connection pid;
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
    (* TODO *) ()

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
end

let has_key = S2S.has_key
let allow_host = S2S.allow_host
