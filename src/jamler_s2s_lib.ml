open Process

let section = Jamler_log.new_section "s2s_lib"

type s2s_out_msg =
    [ `Send_element of Xml.element
    | `Init
    | `Closed
    ]
type validation_msg =
    [ `Valid of Jlib.namepreped * Jlib.namepreped
    | `Invalid of Jlib.namepreped * Jlib.namepreped
    ]
type s2s = string * s2s_out_msg pid
  type fromto = Jlib.namepreped * Jlib.namepreped
  type s2s_table = (fromto, s2s list) Hashtbl.t

  let (s2s_table : s2s_table) = Hashtbl.create 10

  let default_max_s2s_connections_number = 1
  let default_max_s2s_connections_number_per_node = 1

  let find_s2s_list from_to =
    try
      Hashtbl.find s2s_table from_to
    with
      | Not_found -> []

  let new_key () =
    Jlib.get_random_string ()

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

  let have_connection from_to =
    Hashtbl.mem s2s_table from_to

  let has_key from_to key =
    match find_s2s_list from_to with
      | [] -> false
      | xs -> List.mem_assoc key xs

  let get_connections_pids from_to =
    List.map (fun (_, pid) -> pid) (find_s2s_list from_to)

  let dirty_get_connections () =
    Hashtbl.fold (fun from_to _ acc -> from_to :: acc) s2s_table []

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
	let hosts = Jamler_config.myhosts () in
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
    let hosts = Jamler_config.myhosts () in
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

