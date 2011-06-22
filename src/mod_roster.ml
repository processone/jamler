open Gen_roster

module RosterMemoryStorage : RosterStorage =
struct
  let rosters = (Hashtbl.create 100
		   : (Jlib.nodepreped * Jlib.namepreped,
		      (LJID.t, subscription roster_item) Hashtbl.t) Hashtbl.t)

  let read_roster' u s =
    try
      Some (Hashtbl.find rosters (u, s))
    with
      | Not_found -> None

  let read_roster u s =
    try
      let roster = Hashtbl.find rosters (u, s) in
	Hashtbl.fold
	  (fun jid item acc -> (jid, item) :: acc) roster []
    with
      | Not_found -> []

  let delete_roster u s =
    Hashtbl.remove rosters (u, s)

  let read_roster_item u s jid =
    try
      let roster = Hashtbl.find rosters (u, s) in
	Some (Hashtbl.find roster jid)
    with
      | Not_found -> None

  let write_roster_item u s jid item =
    let us = (u, s) in
    let roster =
      try
	Hashtbl.find rosters us
      with
	| Not_found ->
	    let roster = Hashtbl.create 1 in
	      Hashtbl.add rosters us roster;
	      roster
    in
      Hashtbl.replace roster jid item

  let delete_roster_item u s jid =
    let us = (u, s) in
      try
	let roster = Hashtbl.find rosters us in
	  Hashtbl.remove roster jid;
	  if Hashtbl.length roster = 0
	  then Hashtbl.remove rosters us
      with
	| Not_found -> ()

end

module ModRoster = Gen_roster.Make(RosterMemoryStorage)

