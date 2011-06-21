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

val add_pool : string -> unit
val query : string -> ('a, select) query -> 'a list Lwt.t

val string_of_bool : bool -> string
val bool_of_string : string -> bool
