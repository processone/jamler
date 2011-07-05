let section = Jamler_log.new_section "config"

module JSON = Yojson.Safe
type json = JSON.json

let (config : json ref) = ref (`Assoc [])
let config_timestamp = ref 0.0

type 'a p = P of (json -> 'a)

exception Error of string

let int =
  P (function
       | `Int x -> x
       | `Intlit s ->
	   raise (Error (Printf.sprintf "int value out of range: %s" s))
       | json ->
	   raise (Error (Printf.sprintf "expected int value, got %s"
			   (JSON.to_string json)))
    )

let bool =
  P (function
       | `Bool x -> x
       | json ->
	   raise (Error (Printf.sprintf "expected bool value, got %s"
			   (JSON.to_string json)))
    )

let string =
  P (function
       | `String x -> x
       | json ->
	   raise (Error (Printf.sprintf "expected string value, got %s"
			   (JSON.to_string json)))
    )

let namepreped =
  P (function
       | `String x -> (
	   match Jlib.nameprep x with
	     | Some s -> s
	     | None ->
		 raise (Error (Printf.sprintf
				 "the value \"%s\" is not nameprep-compatible"
				 x))
	 )
       | json ->
	   raise (Error (Printf.sprintf "expected string value, got %s"
			   (JSON.to_string json)))
    )

let list (P p) =
  P (function
       | `List xs -> List.map p xs
       | json ->
	   raise (Error (Printf.sprintf "expected list, got %s"
			   (JSON.to_string json)))
    )

let enum vals =
  P (function
       | `String x -> (
	   try
	     List.assoc x vals
	   with
	     | Not_found ->
		 let vs = List.map (fun (v, _) -> "\"" ^ v ^ "\"") vals in
		 let vs = String.concat ", " vs in
		   raise
		     (Error
			(Printf.sprintf
			   "got \"%s\", but only the following values are allowed: %s"
			   x vs))
	 )
       | json ->
	   raise (Error (Printf.sprintf "expected string, got %s"
			   (JSON.to_string json)))
    )

let keys =
  P (function
       | `Assoc assoc -> List.map fst assoc
       | json ->
	   raise (Error (Printf.sprintf "expected JSON object, got %s"
			   (JSON.to_string json)))
    )

let parse (P p) json =
  p json

type 'a opt =
    {mutable v : 'a;
     mutable timestamp : float}

type path = string list

let rec get_global_path path (json : json) =
  match path with
    | [] -> Some json
    | s :: path -> (
	match json with
	  | `Assoc assoc -> (
	      try
		let json = List.assoc s assoc in
		  get_global_path path json
	      with
		| Not_found -> None
	    )
	  | _ ->
	      None
      )

let get_host_part (host : Jlib.namepreped) json =
  match json with
    | `Assoc assoc -> (
	try
	  let check (name, json) =
	    if name = "hostConfig" then (
	      match json with
		| `Assoc assoc -> (
		    try
		      let h = List.assoc "host" assoc in
			(match h with
			   | `String h -> Jlib.nameprep_exn h = host
			   | _ -> false
			)
		    with
		      | _ -> false
		  )
		| _ -> false
	    ) else false
	  in
	    Some (snd (List.find check assoc))
	with
	  | Not_found -> None
      )
    | _ -> None


let rec get_path host path (json : json) =
  match get_host_part host json with
    | Some json ->
	get_global_path path json
    | None ->
	get_global_path path json

let opts : (path, (json -> unit) * bool) Hashtbl.t = Hashtbl.create 10

let register_opt_common path p is_global =
  let check json = ignore (parse p json) in
    Hashtbl.replace opts path (check, is_global)

let register_global_opt path p =
  register_opt_common path p true

let register_opt path p =
  register_opt_common path p false


let get_global_opt_common path p default map =
  let opt = {v = default; timestamp = 0.0} in
  let get () =
    let ts = !config_timestamp in
      if ts > opt.timestamp then (
	let json = !config in
	let v =
	  match get_global_path path json with
	    | Some json ->
		map (parse p json)
	    | None ->
		default
	in
	  opt.v <- v;
	  opt.timestamp <- ts;
	  v
      ) else opt.v
  in
    register_global_opt path p;
    get

let get_global_opt_with_default path p default =
  get_global_opt_common path p default (fun x -> x)

let get_global_opt path p =
  get_global_opt_common path p None (fun x -> Some x)


let get_opt_common path p default map =
  let opt = {v = Hashtbl.create 1; timestamp = 0.0} in
  let get host =
    let ts = !config_timestamp in
      if ts > opt.timestamp then (
	Hashtbl.clear opt.v;
	opt.timestamp <- ts;
      );
      try
	Hashtbl.find opt.v host
      with
	| Not_found ->
	    let json = !config in
	    let v =
	      match get_path host path json with
		| Some json ->
		    map (parse p json)
		| None ->
		    default
	    in
	      Hashtbl.replace opt.v host v;
	      v
  in
    register_opt path p;
    get

let get_opt_with_default path p default =
  get_opt_common path p default (fun x -> x)

let get_opt path p =
  get_opt_common path p None (fun x -> Some x)

let mod_path mod_name = ["modules"; mod_name]

let get_module_opt_common mod_name path p default map =
  get_opt_common (mod_path mod_name @ path) p default map

let get_module_opt_with_default mod_name path p default =
  get_module_opt_common mod_name path p default (fun x -> x)

let get_module_opt mod_name path p =
  get_module_opt_common mod_name path p None (fun x -> Some x)

let myhosts = get_global_opt_with_default ["hosts"] (list namepreped) []
let loglevel =
  get_global_opt_with_default ["loglevel"]
    (enum [("notice", `Notice);
	   ("debug", `Debug);
	   ("error", `Error);
	  ])
    `Notice

let auth_modules _host =
  ["sql"]

let modules = get_opt_with_default ["modules"] keys []

let process_config cfg =
  let host_path_to_string host path =
    let path =
      match host with
	| Some h -> h :: path
	| None -> path
    in
      String.concat "/" path
  in
  let rec traverse path json host =
    match json with
      | `Assoc assoc ->
	  List.iter
	    (fun (name, json) ->
	       if name = "hostConfig" then (
		 match host, path, json with
		   | None, [], `Assoc assoc -> (
		       try
			 let h = List.assoc "host" assoc in
			   (match h with
			      | `String h ->
				  traverse [] json (Some h)
			      | _ ->
				  raise (Error "host must be a string")
			   )
		       with
			 | Not_found ->
			     raise (Error "hostConfig for unknown host")
		     )
		   | None, [], _ ->
		       raise (Error "hostConfig entry must be a JSON object")
		   | None, _, _ ->
		       raise (Error "hostConfig entry only allowed at top level")
		   | Some _, _, _ ->
		       raise (Error (Printf.sprintf
				       "hostConfig inside hostConfig in %s"
				       (host_path_to_string host path)))
	       ) else (
		 let path = path @ [name] in
		 let opt =
		   try
		     Some (Hashtbl.find opts path)
		   with
		     | Not_found -> None
		 in
		   (match opt with
		      | Some (check, is_global) ->
			  if not (is_global && host <> None) then (
			    try
			      check json
			    with
			      | Error err ->
				  raise
				    (Error
				       (Printf.sprintf
					  "Error processing option %s: %s"
					  (host_path_to_string host path) err
				       ))
			  )
		      | None -> ()
		   );
		   traverse path json host
	       )
	    ) assoc
      | _ when path = [] ->
	  raise (Error "config must be a JSON object")
      | _ -> ()
  in
    traverse [] cfg None;
    config := cfg;
    config_timestamp := Unix.gettimeofday ();
    Lwt.return ()

let read_config filename =
  lwt () = Lwt_log.notice_f ~section
    "using config file \"%s\"" filename
  in
    try_lwt
      lwt fd = Lwt_unix.openfile filename [Unix.O_RDONLY] 0o640 in
      let ch = Lwt_io.of_fd ~mode:Lwt_io.input fd in
      lwt content = Lwt_io.read ch in
      lwt () = Lwt_io.close ch in
      let config = Yojson.Safe.from_string content in
      lwt () = process_config config in
	Lwt.return ()
    with
      | exn ->
	  lwt () = Lwt_log.fatal ~section
	    "failed to process config"
	  in
	    Lwt.fail exn
