let myhosts () =
  List.map Jlib.nameprep_exn
    ["localhost"; "e.localhost"; "hoi.jabber.ru"; "zinid.ru"] (* TODO *)

let auth_modules _host =
  ["sql"]

let modules _host =
  [("mod_roster_sql", []);
   ("mod_version", []);
   ("mod_time", []);
  ]

