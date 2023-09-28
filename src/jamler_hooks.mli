type result =
  | Stop
  | OK

type 'a hook

val create : unit -> 'a hook
val add : 'a hook -> Jlib.namepreped -> ('a -> result) -> int -> unit
val run : 'a hook -> Jlib.namepreped -> 'a -> unit
val delete : 'a hook -> Jlib.namepreped -> ('a -> result) -> int -> unit

type ('a, 'b) fold_hook

val create_fold : unit -> ('a, 'b) fold_hook
val add_fold :
  ('a, 'b) fold_hook -> Jlib.namepreped -> ('b -> 'a -> (result * 'b)) ->
  int -> unit
val run_fold : ('a, 'b) fold_hook -> Jlib.namepreped -> 'b -> 'a -> 'b
val delete_fold :
  ('a, 'b) fold_hook -> Jlib.namepreped -> ('b -> 'a -> (result * 'b)) ->
  int -> unit
