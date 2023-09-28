module SQLAuth : Jamler_auth.Auth =
struct
  let name = "sql"

  let src = Jamler_log.new_src "auth_sql"

  let get_password user server =
    let suser = (user : Jlib.nodepreped :> string) in
    let query =
      [%sql {|SELECT @(password)s from users where username = %(suser)s|}]
    in
    try
      let res = Sql.query server query in
      match res with
      | [password] -> Some password
      | _ -> None
    with
    | exn ->
       Logs.err ~src
	 (fun m ->
           m "get_password failed: %a"
             Jamler_log.pp_exn exn);
       raise exn

  let check_password user server password =
    if password <> "" then (
      match get_password user server with
      | Some passwd -> password = passwd
      | None -> false
    ) else false

  let check_password_digest user server password digest digest_gen =
    match get_password user server with
    | Some passwd ->
       let dig_res = digest <> "" && digest = digest_gen passwd in
       if dig_res
       then true
       else passwd = password && password <> ""
    | None -> false

  let plain_password_required = false

  let does_user_exist user server =
    match get_password user server with
    | Some _passwd -> true
    | None -> false

  let remove user server =
    let username = (user : Jlib.nodepreped :> string) in
    let query =
      [%sql {|delete from users where username = %(username)s|}]
    in
    try
      let _ = Sql.query server query in
      ()
    with
    | _ ->
       ()

  let remove' user server password =
    let username = (user : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	     delete from users
	     where username = %(username)s and password = %(password)s
             |}] in
    try
      let _ = Sql.query server query in
      ()
    with
    | _ ->
       ()

  let try_register user server password =
    let username = (user : Jlib.nodepreped :> string) in
    let query =
      [%sql {|
	     insert into users(username, password)
	     values (%(username)s, %(password)s)
             |}] in
    try
      (* TODO: process insert failures *)
      let _ = Sql.query server query in
      Jamler_auth.OK
    with
    | _ ->
       Jamler_auth.Server_error

  let set_password user server password =
    let username = (user : Jlib.nodepreped :> string) in
    let insert_query =
      [%sql {|
	     insert into users(username, password)
	     values (%(username)s, %(password)s)
             |}] in
    let update_query =
      [%sql {|
	     update users set
	     password = %(password)s
             where username = %(username)s
             |}] in
    try
      Sql.transaction server
	(fun () -> Sql.update_t insert_query update_query);
      Jamler_auth.OK
    with
    | _ ->
       Jamler_auth.Server_error

end

let () =
  Jamler_auth.register_mod (module SQLAuth : Jamler_auth.Auth)
