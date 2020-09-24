module Lwt_thread: PGOCaml_generic.THREAD with type 'a t = 'a Lwt.t =
struct
  type 'a t = 'a Lwt.t
  let return x = Lwt.return x
  let (>>=) = Lwt.bind
  let fail = Lwt.fail
  let catch = Lwt.catch
 
  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel
  let open_connection sockaddr = Lwt_io.open_connection sockaddr
  let output_char = Lwt_io.write_char
  let output_binary_int = Lwt_io.BE.write_int
  let output_string = Lwt_io.write
  let flush = Lwt_io.flush
  let input_char = Lwt_io.read_char
  let input_binary_int = Lwt_io.BE.read_int
  let really_input = Lwt_io.read_into_exactly
  let close_in = Lwt_io.close
end

module PG = PGOCaml_generic.Make(Lwt_thread)

let section = Jamler_log.new_section "sql"

exception Error = PG.PostgreSQL_Error

type val_type =
  | Int
  | String
  | Bool

type select
type part

type ('a, 'b) query =
    {query : string;
     params : string option list;
     handler : string option list -> 'a option;
    }

let make_select_query query params handler =
  {query; params; handler}

let make_part_query query params =
  {query; params; handler = fun _ -> None}

let concat_queries q1 q2 =
  {query = q1.query ^ " " ^ q2.query;
   params = q1.params @ q2.params;
   handler = q1.handler}

let process_results rows handler =
  List.rev
    (List.fold_left
       (fun acc row ->
	  match handler row with
	    | Some r -> r :: acc
	    | None -> []
       ) [] rows
    )

type state =
  {dbh : unit PG.t;
   queries : (string, string) Hashtbl.t}

type host = Jlib.namepreped

let pools = Hashtbl.create 10

let add_pool host =
  let pool =
    Lwt_pool.create 10
      (fun () ->
	 let%lwt dbh =
	   PG.connect
	     ~host:"localhost"
	     ~user:"ejabberd"
	     ~password:"ejabberd"
	     ~database:"ejabberd" ()
	 in
	 let%lwt () = PG.prepare dbh
	   ~query:"SET default_transaction_isolation TO SERIALIZABLE" ()
	 in
	 let%lwt _ = PG.execute dbh ~params:[] () in
	   Lwt.return {dbh; queries = Hashtbl.create 10}
      )
  in
    Hashtbl.replace pools host pool

let get_pool host =
  Hashtbl.find pools host

let state_key = Lwt.new_key ()

exception Aborted of string

let is_exn_rollback =
  function
    | Error (_, fields) -> (
	try
	  let s = List.assoc 'C' fields in
	    String.length s >= 2 && s.[0] = '4' && s.[1] = '0'
	with
	  | Not_found -> false
      )
    | _ -> false

let is_exn_unique_violation =
  function
    | Error (_, fields) -> (
	try
	  let s = List.assoc 'C' fields in
	    s = "23505"
	with
	  | Not_found -> false
      )
    | _ -> false

let qcounter = ref 0

let query' q st =
  let%lwt name =
    try%lwt
      Lwt.return (Hashtbl.find st.queries q.query)
    with
      | Not_found ->
	  incr qcounter;
	  let name = "q" ^ string_of_int !qcounter in
	    Hashtbl.add st.queries q.query name;
	    let%lwt () =
	      PG.prepare st.dbh
		~query:q.query ~name:name ()
	    in
	      Lwt.return name
  in
  let%lwt rows = PG.execute st.dbh ~name:name ~params:q.params () in
    Lwt.return (process_results rows q.handler)

let query host q =
  match Lwt.get state_key with
    | Some _ ->
	let%lwt () =
	  Lwt_log.error ~section
	    "Sql.query called inside transaction"
	in
	  raise (Aborted "Sql.query called inside transaction")
    | None ->
	let pool = get_pool host in
	  Lwt_pool.use pool (query' q)

let query_t q =
  match Lwt.get state_key with
    | Some st ->
	query' q st
    | None ->
	let%lwt () =
	  Lwt_log.error ~section
	    "Sql.query_t called outside transaction"
	in
	  raise (Aborted "Sql.query_t called outside transaction")

let max_transaction_restarts = 10

let transaction host f =
  match Lwt.get state_key with
    | Some _ ->
	f ()
    | None ->
	let transaction' f st =
	  let rec loop f n () =
	    let%lwt () = PG.begin_work st.dbh in
	      try%lwt
		let%lwt res = f () in
		let%lwt () = PG.commit st.dbh in
		  Lwt.return res
	      with
		| Error _ as exn when is_exn_rollback exn ->
		    let%lwt () = PG.rollback st.dbh in
		      if n > 0 then (
			loop f (n - 1) ()
		      ) else (
			let%lwt () =
			  Lwt_log.error ~section ~exn
			    "sql transaction restarts exceeded"
			in
			  raise (Aborted "sql transaction restarts exceeded")
		      )
		| exn ->
		    let%lwt () = PG.rollback st.dbh in
		    let%lwt () =
		      Lwt_log.error ~section ~exn "sql transaction failed"
		    in
		      Lwt.fail exn
	  in
	    Lwt.with_value state_key (Some st) (loop f max_transaction_restarts)
	in
	let pool = get_pool host in
	  Lwt_pool.use pool (transaction' f)

let update_t insert_query update_query =
  match Lwt.get state_key with
    | Some st -> (
	let%lwt () = PG.prepare st.dbh ~query:"SAVEPOINT update_sp" () in
	let%lwt _ = PG.execute st.dbh ~params:[] () in
	  try%lwt
	    let%lwt _ = query_t insert_query in
	      Lwt.return ()
	  with
	    | exn when is_exn_unique_violation exn ->
		let%lwt () =
		  PG.prepare st.dbh ~query:"ROLLBACK TO SAVEPOINT update_sp" ()
		in
		let%lwt _ = PG.execute st.dbh ~params:[] () in
		let%lwt _ = query_t update_query in
		  Lwt.return ()
      )
    | None ->
	let%lwt () =
	  Lwt_log.error ~section
	    "Sql.update_t called outside transaction"
	in
	  raise (Aborted "Sql.update_t called outside transaction")

let string_of_bool = PG.string_of_bool
let bool_of_string = PG.bool_of_string


(*
let _ =
  lwt dbh =
    PG.connect
      ~host:"localhost"
      ~user:"ejabberd"
      ~password:"ejabberd"
      ~database:"ejabberd" ()
  in
  lwt () =
    PG.prepare dbh ~query:"select * from users where username=$1" ~name:"four" ();
  in
  lwt rows =
    PG.execute dbh ~name:"four" ~params:[Some "test10"] ();
  in
    List.iter
      (function row ->
	 List.iter
	   (function
	      | None ->
		  Printf.printf "NULL "
	      | Some s ->
		  Printf.printf "%s " s
	   ) row;
	 Printf.printf "\n"
      ) rows;
    flush stdout;
    Lwt.return ()

*)


