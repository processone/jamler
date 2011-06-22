let check_password_with_authmodule _user _server _password =
  Some "none"

let check_password_digest_with_authmodule _user _server _password
    _digest _digest_gen =
  Some "none"

let get_password_with_authmodule _user _server =
  Some ("test", "none")

let does_user_exist _user _server =
  true
