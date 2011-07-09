type result =
  | Stop
  | OK

type 'a hook

val create : unit -> 'a hook
val add : 'a hook -> Jlib.namepreped -> ('a -> result Lwt.t) -> int -> unit
val run : 'a hook -> Jlib.namepreped -> 'a -> unit Lwt.t
val delete : 'a hook -> Jlib.namepreped -> ('a -> result Lwt.t) -> int -> unit

type ('a, 'b) fold_hook

val create_fold : unit -> ('a, 'b) fold_hook
val add_fold :
  ('a, 'b) fold_hook -> Jlib.namepreped -> ('b -> 'a -> (result * 'b) Lwt.t) ->
  int -> unit
val run_fold : ('a, 'b) fold_hook -> Jlib.namepreped -> 'b -> 'a -> 'b Lwt.t
val delete_fold :
  ('a, 'b) fold_hook -> Jlib.namepreped -> ('b -> 'a -> (result * 'b) Lwt.t) ->
  int -> unit

type 'a plain_hook

val create_plain : unit -> 'a plain_hook
val add_plain :
  'a plain_hook -> Jlib.namepreped -> ('a -> result) -> int -> unit
val run_plain : 'a plain_hook -> Jlib.namepreped -> 'a -> unit
val delete_plain :
  'a plain_hook -> Jlib.namepreped -> ('a -> result) -> int -> unit

type ('a, 'b) fold_plain_hook

val create_fold_plain : unit -> ('a, 'b) fold_plain_hook
val add_fold_plain :
  ('a, 'b) fold_plain_hook -> Jlib.namepreped -> ('b -> 'a -> result * 'b) ->
  int -> unit
val run_fold_plain :
  ('a, 'b) fold_plain_hook -> Jlib.namepreped -> 'b -> 'a -> 'b
val delete_fold_plain :
  ('a, 'b) fold_plain_hook -> Jlib.namepreped -> ('b -> 'a -> result * 'b) ->
  int -> unit
