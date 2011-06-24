let sort_hooks hs =
  let hcompare ((s1 : int), _) (s2, _) = compare s1 s2 in
    List.sort hcompare hs

type result =
  | Stop
  | OK

type 'a hook = (Jlib.namepreped, (int * ('a -> result Lwt.t)) list) Hashtbl.t

let create () = Hashtbl.create 1

let add hook host f seq =
  let h =
    try
      Hashtbl.find hook host
    with
      | Not_found -> []
  in
  let h = sort_hooks ((seq, f) :: h) in
    Hashtbl.replace hook host h

let run hook host x =
  let rec aux x =
    function
      | (_, f) :: h ->
	  (try_lwt
	     match_lwt f x with
	       | OK -> aux x h
	       | Stop -> Lwt.return ()
	   with
	     | exn ->
		 Printf.printf "Exception %s\nrunning hook"
		   (Printexc.to_string exn);
		 aux x h
	  )
      | [] -> Lwt.return ()
  in
    try
      let h = Hashtbl.find hook host in
	aux x h
    with
      | Not_found -> Lwt.return ()


type ('a, 'b) fold_hook =
    (Jlib.namepreped, (int * ('b -> 'a -> (result * 'b) Lwt.t)) list) Hashtbl.t

let create_fold () = Hashtbl.create 1

let add_fold hook host f seq =
  let h =
    try
      Hashtbl.find hook host
    with
      | Not_found -> []
  in
  let h = sort_hooks ((seq, f) :: h) in
    Hashtbl.replace hook host h

let run_fold hook host v x =
  let rec aux v x =
    function
      | (_, f) :: h ->
	  (try_lwt
	     match_lwt f v x with
	       | (OK, v) -> aux v x h
	       | (Stop, v) -> Lwt.return v
	   with
	     | exn ->
		 Printf.printf "Exception %s\nrunning hook"
		   (Printexc.to_string exn);
		 aux v x h
	  )
      | [] -> Lwt.return v
  in
    try
      let h = Hashtbl.find hook host in
	aux v x h
    with
      | Not_found -> Lwt.return v

type 'a plain_hook = (Jlib.namepreped, (int * ('a -> result)) list) Hashtbl.t

let create_plain () = Hashtbl.create 1

let add_plain hook host f seq =
  let h =
    try
      Hashtbl.find hook host
    with
      | Not_found -> []
  in
  let h = sort_hooks ((seq, f) :: h) in
    Hashtbl.replace hook host h

let run_plain hook host x =
  let rec aux x =
    function
      | (_, f) :: h ->
	  (try
	     match f x with
	       | OK -> aux x h
	       | Stop -> ()
	   with
	     | exn ->
		 Printf.printf "Exception %s\nrunning hook"
		   (Printexc.to_string exn);
		 aux x h
	  )
      | [] -> ()
  in
    try
      let h = Hashtbl.find hook host in
	aux x h
    with
      | Not_found -> ()


type ('a, 'b) fold_plain_hook =
    (Jlib.namepreped, (int * ('b -> 'a -> result * 'b)) list) Hashtbl.t

let create_fold_plain () = Hashtbl.create 1

let add_fold_plain hook host f seq =
  let h =
    try
      Hashtbl.find hook host
    with
      | Not_found -> []
  in
  let h = sort_hooks ((seq, f) :: h) in
    Hashtbl.replace hook host h

let run_fold_plain hook host v x =
  let rec aux v x =
    function
      | (_, f) :: h ->
	  (try
	     match f v x with
	       | (OK, v) -> aux v x h
	       | (Stop, v) -> v
	   with
	     | exn ->
		 Printf.printf "Exception %s\nrunning hook"
		   (Printexc.to_string exn);
		 aux v x h
	  )
      | [] -> v
  in
    try
      let h = Hashtbl.find hook host in
	aux v x h
    with
      | Not_found -> v

