module Eio_thread: PGOCaml_generic.THREAD with type 'a t = 'a =
struct
  type 'a t = 'a
  let return x = x
  let (>>=) v f = f v
  let fail = raise
  let catch f fexn = try f () with e -> fexn e

  type in_channel = Eio_unix.Net.stream_socket_ty Eio.Resource.t * Eio.Buf_read.t
  type out_channel = Eio_unix.Net.stream_socket_ty Eio.Resource.t * Buffer.t
  let open_connection sockaddr =
    match sockaddr with
    | Unix.ADDR_UNIX _ -> assert false
    | Unix.ADDR_INET (addr, port) ->
       let addr = Eio_unix.Net.Ipaddr.of_unix addr in
       let flow =
         Eio.Net.connect
           ~sw:(Process.get_global_switch ())
           (Process.get_global_env ())#net
           (`Tcp (addr, port))
       in
       ((flow, Eio.Buf_read.of_flow ~max_size:1000000 flow),
        (flow, Buffer.create 100))
  let output_char (_flow, buf) c =
    Buffer.add_char buf c
  let output_binary_int (_flow, buf) x =
    Buffer.add_int32_be buf (Int32.of_int x)
  let output_string (_flow, buf) s =
    Buffer.add_string buf s
  let flush (flow, buf) =
    if Buffer.length buf > 0 then (
      let s = Buffer.contents buf in
      Buffer.clear buf;
      Eio.Flow.copy_string s flow
    )
  let input_char (_flow, buf) =
    Eio.Buf_read.any_char buf
  let input_binary_int (_flow, buf) =
    let s = Eio.Buf_read.take 4 buf in
    let s = Bytes.of_string s in
    Int32.to_int (Bytes.get_int32_be s 0)
  let really_input (_flow, buf) bs pos len =
    let s = Eio.Buf_read.take len buf in
    Bytes.blit_string s 0 bs pos len
  let close_in (flow, _buf) =
    Eio.Flow.close flow
end

module PG = PGOCaml_generic.Make(Eio_thread)

let src = Jamler_log.new_src "sql"

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

let get_sql_server =
  Jamler_config.(get_global_opt_with_default
                   ["sql_server"] string "localhost")
let get_sql_database =
  Jamler_config.(get_global_opt_with_default
                   ["sql_database"] string "ejabberd")
let get_sql_username =
  Jamler_config.(get_global_opt_with_default
                   ["sql_username"] string "ejabberd")
let get_sql_password =
  Jamler_config.(get_global_opt_with_default
                   ["sql_password"] string "ejabberd")

let pools = Hashtbl.create 10

let add_pool host =
  let pool =
    Eio.Pool.create 10
      (fun () ->
        let dbh =
	  PG.connect
	    ~host:(get_sql_server ())
	    ~user:(get_sql_username ())
	    ~password:(get_sql_password ())
	    ~database:(get_sql_database ()) ()
        in
        ignore (PG.prepare dbh
	          ~query:"SET default_transaction_isolation TO SERIALIZABLE" ());
        ignore (PG.execute dbh ~params:[] ());
        {dbh; queries = Hashtbl.create 10}
      )
  in
    Hashtbl.replace pools host pool

let get_pool host =
  Hashtbl.find pools host

open Eio.Std

let state_key = Fiber.create_key ()

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
  let name =
    try
      Hashtbl.find st.queries q.query
    with
    | Not_found ->
       incr qcounter;
       let name = "q" ^ string_of_int !qcounter in
       Hashtbl.add st.queries q.query name;
       PG.prepare st.dbh ~query:q.query ~name:name ();
       name
  in
  let rows = PG.execute st.dbh ~name:name ~params:q.params () in
  process_results rows q.handler

let query host q =
  match Fiber.get state_key with
  | Some _ ->
     Logs.err ~src
       (fun m ->
         m "Sql.query called inside transaction");
     raise (Aborted "Sql.query called inside transaction")
  | None ->
     let pool = get_pool host in
     Eio.Pool.use pool (query' q)

let query_t q =
  match Fiber.get state_key with
  | Some st ->
     query' q st
  | None ->
     Logs.err ~src
       (fun m ->
         m "Sql.query_t called outside transaction");
     raise (Aborted "Sql.query_t called outside transaction")

let max_transaction_restarts = 10

let transaction host f =
  match Fiber.get state_key with
  | Some _ ->
     f ()
  | None ->
     let transaction' f st =
       let rec loop f n () =
         PG.begin_work st.dbh;
	 try
	   let res = f () in
           PG.commit st.dbh;
           res
	 with
	 | Error _ as exn when is_exn_rollback exn ->
            PG.rollback st.dbh;
	    if n > 0 then (
	      loop f (n - 1) ()
	    ) else (
              Logs.err ~src
	        (fun m ->
                  m "sql transaction restarts exceeded: %a"
                    Jamler_log.pp_exn exn);
	      raise (Aborted "sql transaction restarts exceeded")
            )
	 | exn ->
            PG.rollback st.dbh;
            Logs.err ~src
	      (fun m ->
                m "sql transaction failed: %a"
                  Jamler_log.pp_exn exn);
            raise exn
       in
       Fiber.with_binding state_key st (loop f max_transaction_restarts)
     in
     let pool = get_pool host in
     Eio.Pool.use pool (transaction' f)

let update_t insert_query update_query =
  match Fiber.get state_key with
  | Some st -> (
    PG.prepare st.dbh ~query:"SAVEPOINT update_sp" ();
    ignore (PG.execute st.dbh ~params:[] ());
    try
      let _ = query_t insert_query in
      ()
    with
    | exn when is_exn_unique_violation exn ->
       PG.prepare st.dbh ~query:"ROLLBACK TO SAVEPOINT update_sp" ();
       ignore (PG.execute st.dbh ~params:[] ());
       let _ = query_t update_query in
       ()
  )
  | None ->
     Logs.err ~src
       (fun m ->
         m "Sql.update_t called outside transaction");
     raise (Aborted "Sql.update_t called outside transaction")

let string_of_bool = PG.string_of_bool
let bool_of_string = PG.bool_of_string
