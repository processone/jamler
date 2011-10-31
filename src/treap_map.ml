module type S =
sig
  type key 
  type +'a t 
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val next : key -> 'a t -> (key * 'a) option
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val min_elt : 'a t -> key * 'a
end

module Make (Ord : Map.OrderedType) : S  with type key = Ord.t =
struct
  type key = Ord.t
  type +'a t =
    | Node of int * key * int * 'a * 'a t * 'a t
    | Leaf

  let empty = Leaf

  let is_empty =
    function
      | Leaf -> true
      | Node _ -> false

  let tree_size =
    function
      | Node (size, _key, _priority, _value, _left, _right) ->
	  size
      | Leaf -> 0

  let mknode key priority value left right =
    Node (tree_size left + tree_size right + 1,
	  key, priority, value, left, right)

  let balance =
    function
      | Node (size, key, priority, value,
	      Node (lsize, lkey, lpriority, lvalue, lleft, lright),
	      (Node (rsize, rkey, rpriority, rvalue, rleft, rright) as right))
	  when lpriority > priority && lpriority > rpriority ->
	  mknode lkey lpriority lvalue
	    lleft
	    (mknode key priority value lright right)
      | Node (size, key, priority, value,
	      (Node (lsize, lkey, lpriority, lvalue, lleft, lright) as left),
	      Node (rsize, rkey, rpriority, rvalue, rleft, rright))
	  when rpriority > priority && rpriority > lpriority ->
	  mknode rkey rpriority rvalue
	    (mknode key priority value left rleft)
	    rright
      | Node (size, key, priority, value,
	      Node (lsize, lkey, lpriority, lvalue, lleft, lright),
	      (Leaf as right))
	  when lpriority > priority ->
	  mknode lkey lpriority lvalue
	    lleft
	    (mknode key priority value lright right)
      | Node (size, key, priority, value,
	      (Leaf as left),
	      Node (rsize, rkey, rpriority, rvalue, rleft, rright))
	  when rpriority > priority ->
	  mknode rkey rpriority rvalue
	    (mknode key priority value left rleft)
	    rright
      | t -> t

  let rec insert t new_key new_value =
    match t with
      | Node (size, key, priority, value, left, right) ->
	  let cmp = Ord.compare new_key key in
	    if cmp = 0
	    then t
	    else if cmp < 0
	    then
	      let left = insert left new_key new_value in
		balance (mknode key priority value left right)
	    else
	      let right = insert right new_key new_value in
		balance (mknode key priority value left right)
      | Leaf ->
	  Node (1, new_key, Random.bits (), new_value, Leaf, Leaf)

  let add key value t = insert t key value

  let rec root_delete t =
    match t with
      | Node (_size, key, priority, value,
	      (Node (lsize, lkey, lpriority, lvalue, lleft, lright) as left),
	      (Node (rsize, rkey, rpriority, rvalue, rleft, rright) as right)) ->
	  if lpriority > rpriority
	  then
	    mknode lkey lpriority lvalue
	      lleft
	      (root_delete (mknode key priority value lright right))
	  else
	    mknode rkey rpriority rvalue
	      (root_delete (mknode key priority value left rleft))
	      rright
      | Node (_size, _key, _priority, _value, left, Leaf) ->
	  left
      | Node (_size, _key, _priority, _value, Leaf, right) ->
	  right
      | Leaf ->
	  Leaf


  let rec delete t new_key =
    match t with
      | Node (size, key, priority, value, left, right) ->
	  let cmp = Ord.compare new_key key in
	    if cmp = 0
	    then root_delete t
	    else if cmp < 0
	    then
	      let left = delete left new_key in
		mknode key priority value left right
	  else
	    let right = delete right new_key in
	      mknode key priority value left right
      | Leaf ->
	  t

  let remove key t = delete t key

  let rec find k t =
    match t with
      | Node (size, key, priority, value, left, right) ->
	  let cmp = Ord.compare k key in
	    if cmp < 0
	    then find k left
	    else if cmp = 0
	    then value
	    else find k right
      | Leaf ->
	  raise Not_found

  let rec mem k t =
    match t with
      | Node (size, key, priority, _value, left, right) ->
	  let cmp = Ord.compare k key in
	    if cmp < 0
	    then mem k left
	    else if cmp = 0
	    then true
	    else mem k right
      | Leaf ->
	  false

  let rec kth_key t k =
    match t with
      | Node (size, key, priority, value, left, right) ->
	  let left_size = tree_size left in
	    if k <= left_size
	    then kth_key left k
	    else if k = left_size + 1
	    then key
	    else kth_key right (k - left_size - 1)
      | Leaf ->
	  raise Not_found

  let rec count t x =
    match t with
      | Node (size, key, priority, value, left, right) ->
	  let left_size = tree_size left in
	  let cmp = Ord.compare x key in
	    if cmp = 0
	    then left_size
	    else if cmp < 0
	    then count left x
	    else left_size + 1 + count right x
      | Leaf ->
	  0

  let rec next k t =
    let rec aux k t n =
      match t with
	| Node (_size, key, _priority, value, left, right) as n' ->
	    let cmp = Ord.compare k key in
	      if cmp < 0
	      then aux k left n'
	      else aux k right n
	| Leaf -> (
	    match n with
	      | Node (_size, key, _priority, value, _left, _right) ->
		  Some (key, value)
	      | Leaf ->
		  assert false
	  )
    in
      match t with
	| Node (_size, key, _priority, value, left, right) as n ->
	    let cmp = Ord.compare k key in
	      if cmp < 0
	      then aux k left n
	      else next k right
	| Leaf ->
	    None

  let rec iter f t =
    match t with
      | Node (_size, key, _priority, value, left, right) ->
	  iter f left;
	  f key value;
	  iter f right
      | Leaf ->
	  ()

  let rec min_elt t =
    match t with
      | Node (_size, key, _priority, value, Leaf, _right) ->
	  (key, value)
      | Node (_size, _key, _priority, _value, left, _right) ->
	  min_elt left
      | Leaf ->
	  raise Not_found

end

