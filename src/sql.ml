module Lwt_thread: PGOCaml_generic.THREAD with type 'a t = 'a Lwt.t =
struct
  type 'a t = 'a Lwt.t
  let return x = Lwt.return x
  let (>>=) = Lwt.bind
  let fail = Lwt.fail
 
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
	 lwt dbh =
	   PG.connect
	     ~host:"localhost"
	     ~user:"ejabberd"
	     ~password:"ejabberd"
	     ~database:"ejabberd" ()
	 in
	   Lwt.return {dbh; queries = Hashtbl.create 10}
      )
  in
    Hashtbl.replace pools host pool

let get_pool host =
  Hashtbl.find pools host

let qcounter = ref 0

let query host q =
  let pool = get_pool host in
    Lwt_pool.use pool
      (fun st ->
	 lwt name =
	   try_lwt
	     Lwt.return (Hashtbl.find st.queries q.query)
	   with
	     | Not_found ->
		 incr qcounter;
		 let name = "q" ^ string_of_int !qcounter in
		   Hashtbl.add st.queries q.query name;
		   lwt () =
		     PG.prepare st.dbh
		       ~query:q.query ~name:name ()
		   in
		     Lwt.return name
	 in
	 lwt rows = PG.execute st.dbh ~name:name ~params:q.params () in
	   Lwt.return (process_results rows q.handler)
      )


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


