type t = {
  mutable buf : bytes;
  mutable start : int;
  mutable len : int;
  mutable size : int;
  initial_size : int;
}
val create : int -> t
val contents : t -> string
val reset : t -> unit
val add_char : t -> char -> unit
val add_substring : t -> string -> int -> int -> unit
val add_string : t -> string -> unit
val remove : t -> int -> unit
val length : t -> int
val sub : t -> int -> int -> string
