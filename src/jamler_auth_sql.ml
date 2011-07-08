module SQLAuth : Jamler_auth.Auth =
struct
  let name = "sql"

  let section = Jamler_log.new_section "auth_sql"

  let get_password user server =
    let suser = (user : Jlib.nodepreped :> string) in
    let query =
      <:sql< SELECT @(password)s from users where username = %(suser)s >>
    in
      try_lwt
	lwt res = Sql.query server query in
	  match res with
	    | [password] -> Lwt.return (Some password)
	    | _ -> Lwt.return None
      with
	| exn ->
	    lwt () =
	      Lwt_log.error
		~section
		~exn:exn
		"get_password failed"
	    in
	      Lwt.fail exn

  let check_password user server password =
    if password <> "" then (
      match_lwt get_password user server with
	| Some passwd -> Lwt.return (password = passwd)
	| None -> Lwt.return false
    ) else Lwt.return false

  let check_password_digest user server password digest digest_gen =
    match_lwt get_password user server with
      | Some passwd ->
	  let dig_res = digest <> "" && digest = digest_gen passwd in
	    if dig_res
	    then Lwt.return true
	    else Lwt.return (passwd = password && password <> "")
      | None -> Lwt.return false

  let does_user_exist user server =
    match_lwt get_password user server with
      | Some passwd -> Lwt.return true
      | None -> Lwt.return false

  let remove_user _user _server =
    (* TODO *)
    Lwt.return ()

  let remove_user' _user _server _password =
    Lwt.return ()

end

let () =
  Jamler_auth.register_mod (module SQLAuth : Jamler_auth.Auth)
