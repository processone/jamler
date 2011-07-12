module type HashedOrderedType =
sig
  include Hashtbl.HashedType
  include Set.OrderedType with type t := t
end


module Make
  (K : HashedOrderedType)
  (P : Set.OrderedType) :
sig
  type 'a t
  type key = K.t
  type prio = P.t

  val empty : 'a t
  val insert : key -> prio -> 'a -> 'a t -> 'a t
  val delete : key -> 'a t -> 'a t
  val delete_root : 'a t -> 'a t
  val get_root : 'a t -> key * prio * 'a
  val lookup : key -> 'a t -> (prio * 'a) option
  val is_empty : 'a t -> bool
  val fold : (key * prio * 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  val from_list : (key * prio * 'a) list -> 'a t
  val to_list : 'a t -> (key * prio * 'a) list
end
  =
struct
  type key = K.t
  type prio = P.t

  type 'a t =
    | Leaf
    | Node of int * key * prio * 'a * 'a t * 'a t

  let empty = Leaf

  let compare (h1 : int) (k1 : key) h2 k2 =
    if h1 < h2
    then -1
    else if h1 > h2
    then 1
    else K.compare k1 k2

  let heapify tree =
    match tree with
      | Node (_hash, _key, _priority, _value, Leaf, Leaf) ->
	  tree
      | Node (hash, key, priority, value,
	      (Leaf as left),
	      Node (hashr, keyr, priorityr, valuer, leftr, rightr)) ->
	  if P.compare priorityr priority > 0
	  then Node (hashr, keyr, priorityr, valuer,
		     Node (hash, key, priority, value, left, leftr),
		     rightr)
	  else tree
      | Node (hash, key, priority, value,
	      Node (hashl, keyl, priorityl, valuel, leftl, rightl),
	      (Leaf as right)) ->
	  if P.compare priorityl priority > 0
	  then Node (hashl, keyl, priorityl, valuel,
		     leftl,
		     Node (hash, key, priority, value, rightl, right))
	  else tree
      | Node (hash, key, priority, value,
	      (Node (hashl, keyl, priorityl, valuel, leftl, rightl) as left),
	      (Node (hashr, keyr, priorityr, valuer, leftr, rightr) as right)) ->
	  if P.compare priorityr priority > 0
	  then Node (hashr, keyr, priorityr, valuer,
		     Node (hash, key, priority, value, left, leftr),
		     rightr)
	  else if P.compare priorityl priority > 0
	  then Node (hashl, keyl, priorityl, valuel,
		     leftl,
		     Node (hash, key, priority, value, rightl, right))
	  else tree
      | Leaf -> assert false

  let rec delete_root tree =
    match tree with
      | Node (hash, key, priority, value, left, right) -> (
	  match left, right with
	    | Leaf, Leaf -> Leaf
	    | _, Leaf -> left
	    | Leaf, _ -> right
	    | Node (hashl, keyl, priorityl, valuel, leftl, rightl),
		Node (hashr, keyr, priorityr, valuer, leftr, rightr) ->
		if P.compare priorityl priorityr > 0
		then Node (hashl, keyl, priorityl, valuel,
			   leftl,
			   delete_root
			     (Node (hash, key, priority, value, rightl, right)))
		else Node (hashr, keyr, priorityr, valuer,
			   delete_root
			     (Node (hash, key, priority, value, left, leftr)),
			   rightr)
	)
      | Leaf -> assert false

  let rec insert1 tree hash key priority value =
    match tree with
      | Leaf ->
	  Node (hash, key, priority, value, Leaf, Leaf)
      | Node (hash1, key1, priority1, value1, left, right) ->
	  let c = compare hash key hash1 key1 in
	    if c < 0
	    then heapify (Node (hash1, key1, priority1, value1,
				insert1 left hash key priority value,
				right))
	    else if c > 0
	    then heapify (Node (hash1, key1, priority1, value1,
				left,
				insert1 right hash key priority value))
	    else if P.compare priority priority1 = 0
	    then Node (hash, key, priority, value, left, right)
	    else insert1 (delete_root tree) hash key priority value

  let insert key priority value tree =
    let hash = K.hash key in
      insert1 tree hash key priority value

  let rec delete1 hash key =
    function
      | Leaf -> Leaf
      | Node (hash1, key1, priority1, value1, left, right) as tree ->
	  let c = compare hash key hash1 key1 in
	    if c < 0
	    then Node (hash1, key1, priority1, value1,
		       delete1 hash key left, right)
	    else if c > 0
	    then Node (hash1, key1, priority1, value1, left,
		       delete1 hash key right)
	    else delete_root tree

  let delete key tree =
    let hash = K.hash key in
      delete1 hash key tree

  let is_empty =
    function
      | Leaf -> true
      | Node _ -> false

  let get_root =
    function
      | Leaf -> assert false
      | Node (_hash, key, priority, value, _left, _right) ->
	  (key, priority, value)


  let rec lookup1 tree hash key =
    match tree with
      | Leaf -> None
      | Node (hash1, key1, priority1, value1, left, right) ->
	  let c = compare hash key hash1 key1 in
	    if c < 0
	    then lookup1 left hash key
	    else if c > 0
	    then lookup1 right hash key
	    else Some (priority1, value1)

  let lookup key tree =
    let hash = K.hash key in
      lookup1 tree hash key

  let rec fold f acc =
    function
      | Leaf -> acc
      | Node (_hash, key, priority, value, left, right) ->
	  let acc = f (key, priority, value) acc in
	  let acc = fold f acc left in
	    fold f acc right

  let to_list tree =
    let rec aux tree acc =
      match tree with
	| Leaf -> acc
	| tree ->
	    let root = get_root tree in
	      aux (delete_root tree) (root :: acc)
    in
      aux tree []

  let from_list list =
    let rec aux list tree =
      match list with
	| (key, priority, value) :: tail ->
	    aux tail (insert key priority value tree)
	| [] -> tree
    in
      aux list Leaf

end
