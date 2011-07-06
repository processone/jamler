module GenIQHandler = Jamler_gen_iq_handler
module Hooks = Jamler_hooks

module ModOfflineSQL :
sig
  include Gen_mod.Module
  type offline_msg
end
  =
struct
  type offline_msg = {user : Jlib.nodepreped;
		      timestamp : float;
		      expire : float;
		      from : Jlib.jid;
		      to' : Jlib.jid;
		      packet : Xml.element}

  let name = "mod_offline_sql"
  let offline_message_hook = Hooks.create ()
  let resend_offline_messages_hook = Hooks.create_fold ()
  let remove_user = Hooks.create ()
  let anonymous_purge_hook = Hooks.create ()
  let disco_sm_features = Hooks.create_fold ()
  let disco_local_features = Hooks.create_fold ()

  let store_packet msg =
    (* Msgs = receive_all(User, [Msg]),
       Len = length(Msgs),
       MaxOfflineMsgs = get_max_user_messages(AccessMaxOfflineMsgs,
       User, Host), *)
    let max_offline_msgs = max_int in
    let count =
      if max_offline_msgs <> max_int then (
	count_offline_messages msg.user (msg.to').Jlib.lserver
      ) else 0
    in
      if count > max_offline_msgs then (
	discard_warn_sender msg
      ) else (
	let username = ((msg.to').Jlib.luser : Jlib.nodepreped :> string) in
	let from = msg.from in
	let to' = msg.to' in
	let `XmlElement (name, attrs, els) = msg.packet in
	let attrs2 = Jlib.replace_from_to_attrs
	  (Jlib.jid_to_string from) (Jlib.jid_to_string to') attrs in
	let packet = 



  let store_packet (from, to', packet) =
    let type' = Xml.get_tag_attr_s "type" packet in
      if (type' <> "error"
	  && type' <> "groupchat"
	  && type' <> "headline") then (
	match check_event_chatstates from to' packet with
	  | true ->
	      let luser = to'.Jlib.luser in
	      let timestamp = Unix.time () in
	      let `XmlElement (_name, _attrs, els) = packet in
	      let expire = find_x_expire to'.Jlib.lserver els in
	      lwt _ = store_packet' {user = luser;
				     timestamp = timestamp;
				     expire = expire;
				     from = from;
				     to' = to';
				     packet = packet} in
		Lwt.return (Hooks.Stop)
	  | false ->
	      Lwt.return (Hooks.OK)
      ) else
	Lwt.return (Hooks.OK)

  let start host =
    (* ejabberd_hooks:add(webadmin_page_host, Host,
		       ?MODULE, webadmin_page, 50),
    ejabberd_hooks:add(webadmin_user, Host,
		       ?MODULE, webadmin_user, 50),
    ejabberd_hooks:add(webadmin_user_parse_query, Host,
                       ?MODULE, webadmin_user_parse_query, 50),
    AccessMaxOfflineMsgs = gen_mod:get_opt(access_max_user_messages, Opts, max_user_offline_messages), *)
    Lwt.return (
      [Gen_mod.hook offline_message_hook host store_packet 50;
       Gen_mod.fold_hook resend_offline_messages_hook host pop_offline_messages 50;
       Gen_mod.hook remove_user host remove_user_h 50;
       Gen_mod.hook anonymous_purge_hook host remove_user_h 50;
       Gen_mod.fold_hook disco_sm_features host get_sm_features 50;
       Gen_mod.fold_hook disco_local_features host get_sm_features 50;
      ]
    )

  let stop _host =
    Lwt.return ()

end

let () = Gen_mod.register_mod (module ModOfflineSQL : Gen_mod.Module)
