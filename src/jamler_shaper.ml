type maxrate =
    {maxrate : float;
     mutable lastrate : float;
     mutable lasttime : float;
    }

type shaper =
  | None
  | Maxrate of maxrate

module JSON = Yojson.Safe

let parse_shaper =
  Jamler_config.(
    function
      | `Assoc assoc as json -> (
	  try
	    match List.assoc "type" assoc with
	      | `String "none" -> `None
	      | `String "maxrate" -> (
		  try
		    match List.assoc "maxrate" assoc with
		      | `Int maxrate ->
			  `Maxrate maxrate
		      | json ->
			  raise (Error
				   (Printf.sprintf
				      "expected int value, got %s"
				      (JSON.to_string json)))
		  with
		    | Not_found ->
			raise (Error
				 (Printf.sprintf
				    "maxrate value is not defined: %s"
				    (JSON.to_string json)))
		)
	      | json ->
		  raise (Error
			   (Printf.sprintf
			      "unknown shaper type: %s"
			      (JSON.to_string json)))
	  with
	    | Not_found ->
		raise (Error
			 (Printf.sprintf
			    "shaper type is not defined: %s"
			    (JSON.to_string json)))
	)
      | json ->
	  raise (Error (Printf.sprintf "expected JSON object, got %s"
			  (JSON.to_string json)))
  )

let parse_shapers =
  Jamler_config.(
    P (function
	 | `Assoc assoc ->
	     let shapers = Hashtbl.create 10 in
	       List.iter
		 (fun (name, json) ->
		    Hashtbl.replace shapers name (parse_shaper json)
		 ) assoc;
	       shapers
	 | json ->
	     raise (Error (Printf.sprintf "expected JSON object, got %s"
			     (JSON.to_string json)))
      )
  )

let get_shapers =
  Jamler_config.get_global_opt_with_default
    ["shaper"] parse_shapers (Hashtbl.create 1)



let make name =
  let shapers = get_shapers () in
    try
      match Hashtbl.find shapers name with
	| `None ->
	    None
	| `Maxrate maxrate ->
	    Maxrate {maxrate = float_of_int maxrate;
		     lastrate = 0.0;
		     lasttime = Unix.gettimeofday ()}
    with
      | Not_found ->
	  None

let update shaper size =
  match shaper with
    | None -> 0.0
    | Maxrate maxrate ->
	let size = float_of_int size in
	let min_interval =
	  size /. (2.0 *. maxrate.maxrate -. maxrate.lastrate)
	in
	let now = Unix.gettimeofday () in
	let interval = now -. maxrate.lasttime in
	  (*?DEBUG("State: ~p, Size=~p~nM=~p, I=~p~n",
            [State, Size, MinInterv, Interv]),*)
	let pause =
	  if min_interval > interval
	  then min_interval -. interval
	  else 0.0
	in
	let next_now = now +. pause in
	  maxrate.lastrate <-
	    (maxrate.lastrate +.
	       size /. (next_now -. maxrate.lasttime)) /. 2.0;
	  maxrate.lasttime <- next_now;
	  pause

