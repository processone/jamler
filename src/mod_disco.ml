module GenIQHandler = Jamler_gen_iq_handler
module Config = Jamler_config
module Hooks = Jamler_hooks
module Router = Jamler_router
module SM = Jamler_sm
module Auth = Jamler_auth

let section = Jamler_log.new_section "mod_disco"

let rec dropwhile pred = function
  | [] ->
      []
  | (hd :: tail) as rest ->
      match pred hd with
	| true -> dropwhile pred tail
	| false -> rest

let suffix suff string =
  let len = String.length suff in
    try
      (Str.last_chars string len) = suff
    with
      | _ -> false

(* Eliminate consecutive duplicates of list elements.
   Borrowed from:
   http://www.christiankissig.de/cms/files/ocaml99/problem08.ml *)
let compress l =
  let rec compress_2 l e =
    match l with
      | [] -> [e]
      | h::t ->
          if ( h = e ) 
          then ( compress_2 t e )
          else e::( compress_2 t h )
  in
    match l with
      | [] -> []
      | h::t -> compress_2 t h

(* Sort list with removing duplicates *)
let usort l =
  compress (List.sort compare l)

type items_t = | Items of (Xml.element list)
	       | IError of Xml.element
	       | IEmpty
		   
type features_t = | Features of (string list)
		  | FError of Xml.element
		  | FEmpty

module ModDisco :
sig
  include Gen_mod.Module
  val register_feature : Jlib.namepreped -> string -> unit
  val unregister_feature : Jlib.namepreped -> string -> unit
  val register_extra_domain : Jlib.namepreped -> Jlib.namepreped -> unit
  val unregister_extra_domain : Jlib.namepreped -> Jlib.namepreped -> unit
    
  val disco_local_items :
    (Jlib.jid * Jlib.jid * string * string, items_t)
    Jamler_hooks.fold_hook
  val disco_local_identity :
    (Jlib.jid * Jlib.jid * string * string, Xml.element list)
    Jamler_hooks.fold_hook
  val disco_info :
    (Jlib.namepreped * string option * string * string, Xml.element list)
    Jamler_hooks.fold_hook
  val disco_local_features :
    (Jlib.jid * Jlib.jid * string * string, features_t)
    Jamler_hooks.fold_hook
  val disco_sm_items :
    (Jlib.jid * Jlib.jid * string * string, items_t)
    Jamler_hooks.fold_hook
  val disco_sm_identity :
    (Jlib.jid * Jlib.jid * string * string, Xml.element list)
    Jamler_hooks.fold_hook
  val disco_sm_features :
    (Jlib.jid * Jlib.jid * string * string, features_t)
    Jamler_hooks.fold_hook

end
  =
struct

  module DFTable = Set.Make(
    struct
      let compare = Pervasives.compare
      type t = string * Jlib.namepreped
    end)

  module EDTable = Set.Make(
    struct
      let compare = Pervasives.compare
      type t = Jlib.namepreped * Jlib.namepreped
    end)

  let name = "mod_disco"

  let extra_domains = Config.(get_module_opt_with_default
				name ["extra_domains"] (list namepreped) [])
  let server_info = Config.(get_module_opt_with_default
			      name ["server_info"] (list string) [])

  let disco_features = ref DFTable.empty
  let disco_extra_domains = ref EDTable.empty

  let register_feature host feature =
    disco_features := DFTable.add (feature, host) !disco_features

  let unregister_feature host feature =
    disco_features := DFTable.remove (feature, host) !disco_features

  let register_extra_domain host domain =
    disco_extra_domains := EDTable.add (domain, host) !disco_extra_domains

  let unregister_extra_domain host domain =
    disco_extra_domains := EDTable.remove (domain, host) !disco_extra_domains

  let disco_local_items = Hooks.create_fold ()
  let disco_local_identity = Hooks.create_fold ()
  let disco_info = Hooks.create_fold ()
  let disco_local_features = Hooks.create_fold ()
  let disco_sm_items = Hooks.create_fold ()
  let disco_sm_identity = Hooks.create_fold ()
  let disco_sm_features = Hooks.create_fold ()

  (* let is_presence_subscribed
      {Jlib.luser = user; Jlib.lserver = server; _}
      {Jlib.luser = luser; Jlib.lserver = lserver; _} =
    lwt roster_items =
      Hooks.run_fold Gen_roster.roster_get server [] (user, server) in
      Lwt.return (
	(List.exists
	   (function
	      | _, {Gen_roster.jid = (tuser, tserver, _);
		    Gen_roster.subscription = s; _} ->
		  if (tuser = luser && lserver = tserver &&
		      s <> (`None `None)) then true
		  else false
	   ) roster_items) || (user = luser && server = lserver)) *)

  let is_presence_subscribed from to' = Lwt.return false

  let get_user_resources ({Jlib.user = user;
			   Jlib.luser = luser;
			   Jlib.lserver = lserver; _} as jid) = 
    let rs = SM.get_user_resources luser lserver in
      List.map
	(fun r ->
	   `XmlElement ("item",
			[("jid", Jlib.jid_to_string jid);
			 ("name", user)], []))
	(List.sort compare rs)

  let features_to_xml (feature_list:string list) =
    (* Avoid duplicating features *)
    List.map
      (fun feat -> `XmlElement ("feature", [("var", feat)], []))
      (usort feature_list)

  let domain_to_xml domain =
    `XmlElement ("item", [("jid", domain)], [])

  let get_vh_services host =
    let hosts = List.sort
      (fun h1 h2 ->
	 let len1 = String.length h1 in
	 let len2 = String.length h2 in
	   if len1 > len2 then 1
	   else if len1 < len2 then -1
	   else 0) ((Config.myhosts ()) :> string list)
    in
      List.filter
	(fun h ->
	   match dropwhile (fun vh -> not (suffix ("." ^ vh) h)) hosts with
	     | [] ->
		 false
	     | vh :: _ ->
		 vh = host
	) ((Router.dirty_get_all_routes ()) :> string list)

  let select_disco_extra_domains host =
    EDTable.fold (fun (d, h) acc ->
		    if h = host then (d :> string) :: acc
		    else acc)
      !disco_extra_domains []

  let select_disco_features host =
    DFTable.fold (fun (f, h) acc ->
		    if h = host then f :: acc
		    else acc)
      !disco_features []

  let get_local_services acc (from, to', node, lang) =
    match acc with
      | IError _ ->
	  Lwt.return (Hooks.OK, acc)
      | _ when node = "" ->
	  let items = match acc with
	    | Items its -> its
	    | IEmpty -> []
	  in
	  let host = to'.Jlib.lserver in
	  let domains =
	    (get_vh_services (host :> string)) @ (select_disco_extra_domains host) in
	  let res = List.map domain_to_xml (usort domains) in
	    Lwt.return (Hooks.OK, Items (res @ items))
      | Items _ ->
	  Lwt.return (Hooks.OK, acc)
      | IEmpty ->
	  Lwt.return (Hooks.OK, IError Jlib.err_item_not_found)

  let get_local_identity acc (_from, _to, node, _lang) =
    match node with
      | "" ->
	  Lwt.return
	    (Hooks.OK,
	     acc @ [`XmlElement
		      ("identity",
		       [("category", "server");
			("type", "im");
			("name", Cfg.name)],
		       [])])
      | _ ->
	  Lwt.return (Hooks.OK, acc)

  let get_local_features acc (_from, to', node, _lang) =
    match acc with
      | FError _ ->
	  Lwt.return (Hooks.OK, acc)
      | _ when node = "" ->
	  let feats = match acc with
	    | Features features -> features
	    | FEmpty -> []
	  in
	  let host = to'.Jlib.lserver in
	    Lwt.return
	      (Hooks.OK, Features ((select_disco_features host) @ feats))
      | Features _ ->
	  Lwt.return (Hooks.OK, acc)
      | FEmpty ->
	  Lwt.return (Hooks.OK, FError Jlib.err_item_not_found)

  let get_sm_items acc (from, to', node, _lang) =
    match acc with
      | IError _ ->
	  Lwt.return (Hooks.OK, acc)
      | _ when node = "" ->
	  let items = match acc with
	    | Items its -> its
	    | IEmpty -> []
	  in
	  let%lwt items1 =
	    match%lwt is_presence_subscribed from to' with
	      | true ->
		  Lwt.return (get_user_resources to')
	      | false ->
		  Lwt.return []
	  in
	    Lwt.return (Hooks.OK, Items (items @ items1))
      | Items _ ->
	  Lwt.return (Hooks.OK, acc)
      | IEmpty ->
	  let {Jlib.luser = lfrom; Jlib.lserver = lsfrom; _} = from in
	  let {Jlib.luser = lto; Jlib.lserver = lsto; _} = to' in
	    if (lfrom = lto && lsfrom = lsto) then
	      Lwt.return (Hooks.OK, IError Jlib.err_item_not_found)
	    else
	      Lwt.return (Hooks.OK, IError Jlib.err_not_allowed)

  let get_sm_features acc (from, to', _node, _lang) =
    match acc with
      | FEmpty ->
	  let {Jlib.luser = lfrom; Jlib.lserver = lsfrom; _} = from in
	  let {Jlib.luser = lto; Jlib.lserver = lsto; _} = to' in
	    if (lfrom = lto && lsfrom = lsto) then
	      Lwt.return (Hooks.OK, FError Jlib.err_item_not_found)
	    else
	      Lwt.return (Hooks.OK, FError Jlib.err_not_allowed)
      | _ ->
	  Lwt.return (Hooks.OK, acc)

  let get_sm_identity acc (_from,
			   {Jlib.luser = luser; Jlib.lserver = lserver; _},
			   _node, _lang) =
    match%lwt Auth.does_user_exist luser lserver with
      | true ->
	  Lwt.return
	    (Hooks.OK, acc @
	       [`XmlElement ("identity",
			     [("category", "account");
			      ("type", "registered")],
			     [])])
      | false ->
	  Lwt.return (Hooks.OK, acc)

  (* Support for: XEP-0157 Contact Addresses for XMPP Services *)

  (* TODO : need to clarify "server_info" option type
  let values_to_xml values =
    List.map (fun value -> `XmlElement ("value", [], [`XmlCdata value])) values

  let field_to_xml (_, var, values) =
    let values_xml = values_to_xml values in
      `XmlElement ("field", [("var", var)], values_xml)

  let fields_to_xml fields =
    List.map field_to_xml fields

  let get_fields_xml host module' =
    let fields = server_info host in
      (* filter, and get only the ones allowed for this module *)
    let fields_good = List.filter
      (fun (modules, _, _) ->
	 match modules with
	   | ["all"] -> true
	   | _ -> List.mem module' modules
      ) fields in
      fields_to_xml fields_good
	
  let get_info acc (host, mod', node, _lang) =
    if node = "" then (
      let module' = match mod' with
	| None -> name
	| Some m -> m
      in
      let serverinfo_fields = get_fields_xml host module' in
	Lwt.return (Hooks.OK,
		    [`XmlElement
		       ("x",
			[("xmlns", <:ns<XDATA>>);
			 ("type", "result")],
			[`XmlElement
			   ("field",
			    [("var", "FORM_TYPE");
			     ("type", "hidden")],
			    [`XmlElement
			       ("value", [],
				[`XmlCdata <:ns<SERVERINFO>>]
			       )]
			   )]
			@ serverinfo_fields
		       )])
    ) else
      Lwt.return (Hooks.OK, acc) *)

  let get_info acc _ =
    Lwt.return (Hooks.OK, acc)

  let process_local_iq_items from to' iq =
    match iq.Jlib.iq_type with
      | `Set subel ->
	  Lwt.return (`IQ {iq with
                             Jlib.iq_type =
                          `Error (Jlib.err_not_allowed, Some subel)})
      | `Get subel ->
	  let node = Xml.get_tag_attr_s "node" subel in
	  let host = to'.Jlib.lserver in
	  let lang = iq.Jlib.iq_lang in
	    match%lwt Hooks.run_fold disco_local_items
	      host IEmpty (from, to', node, lang) with
		| Items items ->
		    let anode = match node with
		      | "" -> []
		      | _ -> [("node", node)]
		    in
		      Lwt.return
			(`IQ {iq with
				Jlib.iq_type =
			     `Result
			       (Some (`XmlElement
					("query",
					 ("xmlns", [%ns:DISCO_ITEMS]) :: anode,
					 (items :> Xml.element_cdata list))))})
		| IError error ->
		    Lwt.return (`IQ {iq with
				       Jlib.iq_type =
				    `Error (error, Some subel)})

  let process_local_iq_info from to' iq =
    match iq.Jlib.iq_type with
      | `Set subel ->
	  Lwt.return (`IQ {iq with
                             Jlib.iq_type =
                          `Error (Jlib.err_not_allowed, Some subel)})
      | `Get subel ->
	  let host = to'.Jlib.lserver in
	  let node = Xml.get_tag_attr_s "node" subel in
	  let lang = iq.Jlib.iq_lang in
	  let%lwt identity = Hooks.run_fold disco_local_identity
	    host [] (from, to', node, lang) in
	  let%lwt info = Hooks.run_fold disco_info
	    host [] (host, Some name, node, lang) in
	    match%lwt Hooks.run_fold disco_local_features
	      host FEmpty (from, to', node, lang) with
		| Features features ->
		    let anode = match node with
		      | "" -> []
		      | _ -> [("node", node)]
		    in
		    let res_els = identity @ info @ (features_to_xml features) in
		      Lwt.return
			(`IQ {iq with
				Jlib.iq_type =
			     `Result
			       (Some (`XmlElement
					("query",
					 ("xmlns", [%ns:DISCO_INFO]) :: anode,
					 (res_els :> Xml.element_cdata list))))})
		| FError error ->
		    Lwt.return (`IQ {iq with
				       Jlib.iq_type =
				    `Error (error, Some subel)})

  let process_sm_iq_items from to' iq =
    match iq.Jlib.iq_type with
      | `Set subel ->
	  Lwt.return (`IQ {iq with
                             Jlib.iq_type =
                          `Error (Jlib.err_not_allowed, Some subel)})
      | `Get subel ->
	  match%lwt is_presence_subscribed from to' with
	    | true -> (
		let host = to'.Jlib.lserver in
		let node = Xml.get_tag_attr_s "node" subel in
		let lang = iq.Jlib.iq_lang in
		  match%lwt Hooks.run_fold disco_sm_items
		    host IEmpty (from, to', node, lang) with
		      | Items items ->
			  let anode = match node with
			    | "" -> []
			    | _ -> [("node", node)]
			  in
			    Lwt.return
			      (`IQ {iq with
				      Jlib.iq_type =
				   `Result
				     (Some (`XmlElement
					      ("query",
					       ("xmlns", [%ns:DISCO_ITEMS])
					       :: anode,
					       (items :> Xml.element_cdata list)
					      )))})
		      | IError error ->
			  Lwt.return (`IQ {iq with
					     Jlib.iq_type =
					  `Error (error, Some subel)}))
	    | false ->
		Lwt.return
		  (`IQ {iq with
			  Jlib.iq_type =
		       `Error (Jlib.err_service_unavailable, Some subel)})

  let process_sm_iq_info from to' iq =
    match iq.Jlib.iq_type with
      | `Set subel ->
	  Lwt.return (`IQ {iq with
                             Jlib.iq_type =
                          `Error (Jlib.err_not_allowed, Some subel)})
      | `Get subel ->
	  match%lwt is_presence_subscribed from to' with
	    | true -> (
		let host = to'.Jlib.lserver in
		let node = Xml.get_tag_attr_s "node" subel in
		let lang = iq.Jlib.iq_lang in
		let%lwt identity = Hooks.run_fold disco_sm_identity
		  host [] (from, to', node, lang) in
		  match%lwt Hooks.run_fold disco_sm_features
		    host FEmpty (from, to', node, lang) with
		      | Features features ->
			  let anode = match node with
			    | "" -> []
			    | _ -> [("node", node)]
			  in
			  let res_els = identity @ (features_to_xml features) in
			    Lwt.return
			      (`IQ {iq with
				      Jlib.iq_type =
				   `Result
				     (Some (`XmlElement
					      ("query",
					       ("xmlns", [%ns:DISCO_INFO])
					       :: anode,
					       (res_els :> Xml.element_cdata list))))})
		      | FError error ->
			  Lwt.return (`IQ {iq with
					     Jlib.iq_type =
					  `Error (error, Some subel)}))
	    | false ->
		Lwt.return
		  (`IQ {iq with
			  Jlib.iq_type =
		       `Error (Jlib.err_service_unavailable, Some subel)})

  let start host =
    (* ejabberd_local:refresh_iq_handlers() *)
    register_feature host "iq";
    register_feature host "presence";
    register_feature host "presence-invisible";
    register_feature host [%ns:DISCO_ITEMS];
    register_feature host [%ns:DISCO_INFO];
    List.iter (fun domain -> register_extra_domain host domain) (extra_domains host);
    Lwt.return (
      [Gen_mod.iq_handler `Local host [%ns:DISCO_ITEMS] process_local_iq_items ();
       Gen_mod.iq_handler `Local host [%ns:DISCO_INFO] process_local_iq_info ();
       Gen_mod.iq_handler `SM host [%ns:DISCO_ITEMS] process_sm_iq_items ();
       Gen_mod.iq_handler `SM host [%ns:DISCO_INFO] process_sm_iq_info ();
       Gen_mod.fold_hook disco_local_items host get_local_services 100;
       Gen_mod.fold_hook disco_local_features host get_local_features 100;
       Gen_mod.fold_hook disco_local_identity host get_local_identity 100;
       Gen_mod.fold_hook disco_sm_items host get_sm_items 100;
       Gen_mod.fold_hook disco_sm_features host get_sm_features 100;
       Gen_mod.fold_hook disco_sm_identity host get_sm_identity 100;
       Gen_mod.fold_hook disco_info host get_info 100;
      ]
    )

  let stop host =
    let filter (_, h) = h <> host in
      disco_features := DFTable.filter filter !disco_features;
      disco_extra_domains := EDTable.filter filter !disco_extra_domains;
      Lwt.return ()

end

let () = Gen_mod.register_mod (module ModDisco : Gen_mod.Module)

let register_feature = ModDisco.register_feature
let unregister_feature = ModDisco.unregister_feature
let register_extra_domain = ModDisco.register_extra_domain
let unregister_extra_domain = ModDisco.unregister_extra_domain

let disco_local_items = ModDisco.disco_local_items
let disco_local_identity = ModDisco.disco_local_identity
let disco_info = ModDisco.disco_info
let disco_local_features = ModDisco.disco_local_features
let disco_sm_items = ModDisco.disco_sm_items
let disco_sm_identity = ModDisco.disco_sm_identity
let disco_sm_features = ModDisco.disco_sm_features
