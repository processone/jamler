type val_type =
  | Int
  | String
  | Bool

type select
type part

type ('a, 'b) query

val make_select_query :
  string ->
  string option list ->
  (string option list -> 'a option) ->
  ('a, select) query

val make_part_query :
  string ->
  string option list ->
  (unit, part) query

val concat_queries :
  ('a, select) query -> (unit, part) query -> ('a, select) query

type host = Jlib.namepreped

val add_pool : host -> unit
val query : host -> ('a, select) query -> 'a list Lwt.t

val string_of_bool : bool -> string
val bool_of_string : string -> bool
