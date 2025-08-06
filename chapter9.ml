(********************************************************************
 * exercise: hash insert
 ********************************************************************)

(*
 * +-----+-----+-----+-----+-----+-----+-----+
 * |  0  |  1  |  2  |  3  |  4  |  5  |  6  |
 * +-----+-----+-----+-----+-----+-----+-----+
 *    |     |     |           |
 *    V     V     V           V
 *  +---+ +---+ +---+       +---+
 *  | 42| | 15| | 23|       |  4|
 *  | ()| | ()| | ()|       | ()|
 *  +---+ +---+ +---+       +---+
 *          |     |
 *          V     V
 *        +---+ +---+
 *        |  8| | 16|
 *        | ()| | ()|
 *        +---+ +---+
 *
 * The picture above shows the keys on the first line and the value
 * they bind to on the second line.  Here, we've chosen the unit
 * value, but that is unimportant to the exercise.
 *)


(********************************************************************
 * exercise: relax bucket RI
 ********************************************************************)

(* If no rehash is needed, [insert] is a little better than before, because it
   can just cons the new binding onto the front of the bucket instead of scanning
   the entire bucket for duplicates. That is, it becomes worst-case constant time
   instead of expected constant time.

   But of course sometimes rehashes will be needed. Then, [insert] is stilll
   worst-case linear time. But we need to say linear in *what*. It's still linear
   in the number of bindings, but the number of bindings can now be bigger than the
   number of keys, since some keys might have shadowed bindings.

   So [insert] remains worst-case linear in the number of bindings.

   [remove] stays worst-case linear time, although it will in practice be slightly
   slower than the original RI, because we cannot terminate early upon finding the
   key. Or, we could change the specification of [remove] and have it remove only
   the first binding it finds. That's what [Hashtbl] does.

   The complexity of [find] remains the same. *)


(********************************************************************
 * exercise: strengthen bucket RI
 ********************************************************************)

(* Complexity of all operations remain the same. [insert] stays
 * worst-case linear time since we might have to go through the
 * entire list to find the correct place. There are no changes to
 * [find] or [remove].
*)

(********************************************************************
 * exercise: hash values
 ********************************************************************)

(* At least in the current implementation, hashing [()], [false], [0],
   and [[]] all result in the same [int]. Likewise [true] and [1] hash
   to the same [int]. But [""] hashes just to [0].

   As for long lists, [Hashtbl.hash], to ensure constant time performance,
   looks only at the first 10 "meaningful" elements of a list. For more,
   see the documentation of [Hashtbl.hash_param]. *)

(********************************************************************
 * exercise: hashtbl usage
 ********************************************************************)

 let ( -- ) i j =
 let rec from i j l =
   if i > j then l
   else from i (j - 1) (j :: l)
 in
 from i j []

let tab = Hashtbl.create 16

let ints = List.map (fun x -> (x, string_of_int x)) (1 -- 31)

let () = List.iter (fun (k, v) -> Hashtbl.add tab k v) ints

let () = assert (Hashtbl.find tab 1 = "1")
let () = assert ((try Hashtbl.find tab 0 with Not_found -> "") = "")


(********************************************************************
* exercise: hashtbl stats
********************************************************************)

(* In some of the code below, we prefix field names with the name of
  the module they are defined in.  This eliminates compiler warning
  #40.  That warning gets triggered whenever the field name being
  used to select from a record is not sufficiently well defined in
  the current scope.  *)

let buckets h =
 (Hashtbl.stats h).num_buckets

let () = assert (buckets tab = 16)

let single_binding h =
 (Hashtbl.stats h).bucket_histogram.(1)

let () = assert (single_binding tab = 3)


(********************************************************************
* exercise: hashtbl bindings
********************************************************************)

(* [bindings h] is a list of key-value pairs that are in [h] *)
let bindings h =
 Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []


(********************************************************************
* exercise: hashtbl load factor
********************************************************************)

(* [x /.. y] is the floating-point division of two ints [x] and [y].
* The ints are first converted to floating-point before doing the division. *)
let (/..) x y =
 float_of_int x /. float_of_int y

(* [load_factor h] is the number of bindings in [h] divided by the number
* of buckets.
*)
let load_factor h =
 let stats = Hashtbl.stats h in
 stats.num_bindings /.. stats.num_buckets

let epsilon = 0.1

(* [close_to x y] is [true] if [x] and [y] are within [epsilon] of
* each other, and [false] otherwise. *)
let close_to x y =
 abs_float (x -. y) <= epsilon

(* Adding 2 bindings causes the hash table to resize and lowers the
* load factor down close to 1.
*)

let () = Hashtbl.add tab 0 "0"; assert (not (close_to (load_factor tab) 1.0))
let () = Hashtbl.add tab ~-1 "-1"; assert (close_to (load_factor tab) 1.0)

(********************************************************************
* exercise: functorial interface
********************************************************************)

module CaseInsensitiveHashtbl =
 Hashtbl.Make (struct
   type t = string

   let equal s1 s2 =
     String.lowercase_ascii s1 = String.lowercase_ascii s2

   let hash s =
     Hashtbl.hash (String.lowercase_ascii s)
 end)


(********************************************************************
* exercise: equals and hash
********************************************************************)

(* The reason [equals] and [hash] must agree is so that equal-but-not-
* identical keys are stored in the same bucket.
*
* Suppose that [equal k1 k2] is [true], and that we insert a binding into [h]
* from [k1] to [v1].  Then it ought to be the case that [find k1 h
* = find k2 h].  But suppose that [hash k1 <> hash k2].  Then with high
* probability, [k1] and [k2] would be stored in different buckets. So
* if we also inserted a binding from [k2] to [v2], where [v1 <> v2],
* there would be two bindings in the hash table, when there should be
* only one.   And now [find k1 h <> find k2 h], which contradicts
* what we said above.
*)

(********************************************************************
* exercise: bad hash
********************************************************************)

module BadHashtbl =
 Hashtbl.Make (struct
   type t = int
   let equal = (=)
   let hash _ = 0
 end)

let bad = BadHashtbl.create 16

let () =
 1--100
 |> List.map (fun x -> x, string_of_int x)
 |> List.iter (fun (k,v) -> BadHashtbl.add bad k v)

(* there is now one bucket that has 100 bindings *)
let () = assert ((BadHashtbl.stats bad).bucket_histogram.(100) = 1)


(********************************************************************
* exercise: linear probing
********************************************************************)

(* We don't have a solution on file for this exercise. If you'd like
  to contribute one, please contact the professor! *)

(********************************************************************
* exercise: draw BST
********************************************************************)

(*
* Height 3:
*        10
*      /    \
*     4     17
*   /  \   /  \
*  1   5  16  21
*
* Height 4:
*      5
*     / \
*    /   \
*   4    10
*  /       \
* 1        17
*          / \
*         /   \
*        16   21
*
* Height 5:
*      5
*     / \
*    /   \
*   4    10
*  /       \
* 1        16
*            \
*            17
*              \
*              21
*
* Height 6:
*   4
*  / \
* 1   5
*      \
*      10
*        \
*        16
*          \
*          17
*            \
*            21
*
* Height 7:
* 1
*  \
*   4
*    \
*     5
*      \
*      10
*        \
*        16
*          \
*          17
*            \
*            21
*)


(********************************************************************
* exercise: search sequence
********************************************************************)

(* (C) cannot be a search sequence because we see 912 after we go
* left at 911. All nodes in the left subtree of 911 must be less
* than 911.
*)


(********************************************************************
* exercise: functorized BST
********************************************************************)

module type Set = sig
  (* [elt] is the type of the set elements. *)
  type elt

  (* [t] is the type of sets whose elements have type [elt]. *)
  type t

  (* [empty] is the empty set *)
  val empty    : t

  (* [insert x s] is the set ${x} \union s$. *)
  val insert   : elt -> t -> t

  (* [mem x s] is whether $x \in s$. *)
  val mem      : elt -> t -> bool

  (* [of_list lst] is the smallest set containing all the elements of [lst]. *)
  val of_list  : elt list -> t

  (* [elements s] is the list containing the same elements as [s]. *)
  val elements : t -> elt list
 end

 module type Ordered = sig
  type t
  val compare : t -> t -> int
 end

 module BstSet (Ord : Ordered) : Set with type elt = Ord.t = struct
  (* AF:  [Leaf] represents the empty set.  [Node (l, v, r)] represents
   *   the set $AF(l) \union {v} \union AF(r)$. *)
  (* RI:  for every [Node (l, v, r)], all the values in [l] are strictly
   *   less than [v], and all the values in [r] are strictly greater
   *   than [v]. *)

  type elt = Ord.t

  type t = Leaf | Node of t * elt * t

  let empty = Leaf

  let rec mem x = function
    | Leaf -> false
    | Node (l, v, r) ->
      begin
        match compare x v with
        | ord when ord < 0 -> mem x l
        | ord when ord > 0 -> mem x r
        | _                -> true
      end

  let rec insert x = function
    | Leaf -> Node (Leaf, x, Leaf)
    | Node (l, v, r) ->
      begin
        match compare x v with
        | ord when ord < 0 -> Node(insert x l, v, r         )
        | ord when ord > 0 -> Node(l,          v, insert x r)
        | _                -> Node(l,          x, r         )
      end

  let of_list lst =
    List.fold_left (fun s x -> insert x s) empty lst

  let rec elements = function
    | Leaf -> []
    | Node (l, v, r) -> (elements l) @ [v] @ (elements r)
 end

(* An example usage of the functor: *)

module IntSet = BstSet (Int)
let example_set = IntSet.(empty |> insert 1)


(********************************************************************
* exercise: efficient traversals
********************************************************************)

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec preorder t =
 (* [go acc t] is equivalent to [List.rev (preorder t) @ acc] *)
 let rec go acc = function
   | Leaf -> acc
   | Node (l,v,r) -> go (go (v :: acc) l) r
 in
 List.rev (go [] t)

let rec inorder t =
 (* [go acc t] is equivalent to [List.rev (inorder t) @ acc] *)
 let rec go acc = function
   | Leaf -> acc
   | Node (l,v,r) -> go (v :: go acc l) r
 in
 List.rev (go [] t)

let rec postorder t =
 (* [go acc t] is equivalent to [List.rev (postorder t) @ acc] *)
 let rec go acc = function
   | Leaf -> acc
   | Node (l,v,r) -> v :: go (go acc l) r
 in
 List.rev (go [] t)

let t =
 Node(Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
      4,
      Node(Node(Leaf, 5, Leaf), 6, Node(Leaf, 7, Leaf)))

(*
 t is
       4
     /   \
    2     6
   / \   / \
  1   3 5   7
*)

let () = assert (preorder t  = [4;2;1;3;6;5;7])
let () = assert (inorder t   = [1;2;3;4;5;6;7])
let () = assert (postorder t = [1;3;2;5;7;6;4])


(********************************************************************
* exercise: RB draw complete
********************************************************************)

(*
* Black height 2:
*               B8
*              / \
*             /   \
*            /     \
*           /       \
*          /         \
*         /           \
*        /             \
*       R4             R12
*      / \             / \
*     /   \           /   \
*    /     \         /     \
*   B2     B6      B10     B14
*  / \     / \     / \     / \
* R1 R3   R5 R7  R9 R11  R13 R15
*
* Black height 3:
*               B8
*              / \
*             /   \
*            /     \
*           /       \
*          /         \
*         /           \
*        /             \
*       B4             B12
*      / \             / \
*     /   \           /   \
*    /     \         /     \
*   R2     R6      R10    R14
*  / \     / \     / \     / \
* B1 B3   B5 B7   B9 B11 B13 B15
*
* Black height 4:
*               B8
*              / \
*             /   \
*            /     \
*           /       \
*          /         \
*         /           \
*        /             \
*       B4             B12
*      / \             / \
*     /   \           /   \
*    /     \         /     \
*   B2     B6      B10    B14
*  / \     / \     / \     / \
* B1 B3   B5 B7   B9 B11 B13 B15
*)


(********************************************************************
* exercise: RB draw insert
********************************************************************)

(*
*              B 'E'
*              / \
*             /   \
*            /     \
*           /       \
*          /         \
*         /           \
*        /             \
*      B 'C'          B 'S'
*      / \             / \
*     /   \           /   \
*    /     \         /     \
*  B 'A'  B 'D'    B 'R'  B 'T'
*                            \
*                           R 'U'
*)

type 'a sequence =
    Cons of 'a * (unit -> 'a sequence)

(**
 * [from n] is the sequence <n; n+1; n+2; ...>.
*)
let rec from n =
  Cons (n, fun () -> from (n+1))

(**
 * [nats] is the sequence <0; 1; 2; ...>
*)
let nats = from 0

(* [hd s] is the head of [s] *)
let hd (Cons (h, _)) = h

(* [tl s] is the tail of [s] *)
let tl (Cons (_, tf)) = tf ()

(* [take n s] is the list of the first [n] elements of [s] *)
let rec take n s =
  if n=0 then []
  else hd s :: take (n-1) (tl s)

(* [drop n s] is all but the first [n] elements of [s] *)
let rec drop n s =
  if n = 0 then s
  else drop (n-1) (tl s)

(********************************************************************
 * exercise: pow2
 ********************************************************************)

(**
 * [pow2from n] is the sequence of powers of 2 starting with [n].
 * example:  [pow2from 4] is <4;8;16;32;...>.
 * requires: [n] is a power of 2
*)
let rec pow2from n =
  Cons (n, fun () -> pow2from (2 * n))

(**
 * [pow2] is the sequence <1; 2; 4; 8; 16; ...>.
*)
let pow2 = pow2from 1

(********************************************************************
 * exercise: more sequences
 ********************************************************************)

(** Even naturals **)

(* [from_skip n k] is the sequnce <n; n + 1*k, n + 2*k; ...>*)
let rec from_skip n k =
  Cons (n, fun () -> from_skip (n + k) k)

let evens = from_skip 0 2

(** Lower case alphabet **)

let rec alphabet_gen n  =
  Cons (Char.chr ((n mod 26) + Char.code 'a'),
        fun () -> alphabet_gen (n + 1))

let alphabet = alphabet_gen 0

(** Coin flip **)
let rec flips_gen next = Cons (next, fun () -> flips_gen (Random.bool ()))
let flips = Random.self_init (); flips_gen (Random.bool ())

(********************************************************************
 * exercise: nth
 ********************************************************************)

(**
 * [nth s n] is the element at zero-based position [n] in sequence [s].
 * requires: [n >= 0]
*)
let rec nth (Cons (h, tf)) n =
  if n=0 then
    h
  else
    nth (tf ()) (n - 1)


(********************************************************************
 * exercise: filter
 ********************************************************************)

(**
 * [filter p s] is the sub-sequence of s whose elements satisfy the
 * predicate [p]. If there are no elements of [s] that satisfies [p],
 * then [filter p s] does not terminate.
*)
let rec filter p (Cons (h, tf)) =
  if p h then
    Cons (h, fun () -> filter p (tf ()))
  else
    filter p (tf ())


(********************************************************************
 * exercise: interleave
 ********************************************************************)

(**
 * [interleave <a1; a2; a3; ...> <b1; b2; b3; ...>] is the sequence
 * <a1; b1; a2; b2; a3; b3; ...>.
*)
let rec interleave (Cons (h1, tf1)) (Cons (h2, tf2)) =
  Cons (h1, fun () ->
      Cons (h2, fun () ->
          interleave (tf1 ()) (tf2 ())))

(* The solution above is not quite as lazy as it could be,
   because it forces tf2 even though its value might never
   be needed.  The solution below avoids that by swapping
   the two sequences at each recursive call. *)

let rec interleave_more_lazy (Cons (h1, tf1)) s2 =
  Cons (h1, fun () ->
      interleave_more_lazy s2 (tf1 ()))

(********************************************************************
 * exercise: sift
 ********************************************************************)

(**
 * [sift n s] is the sub-sequence of [s] where all multiples of [n] are
 * removed.
*)
let rec sift n =
  filter (fun x -> x mod n <> 0)

(********************************************************************
 * exercise: primes
 ********************************************************************)

(**
 * [sieve s] computes the Sieve of Eratosthenes starting from the head of [s].
 *  That is, it returns the same sequence as [s], except it sifts out
 *  all multiples of the head of [s] from its tail.
 * requires: all values less than the head of [s] have already been sifted.
 * example:  [sieve <3;5;7;9;11;13;...>] is <3;5;7;11;13;...>]
*)
let rec sieve (Cons (h, tf)) =
  Cons (h, fun () -> tf () |> sift h |> sieve)

(**
 * [primes] is the sequence containing all (and only) prime numbers.
*)
let primes =
  sieve (from 2)


(********************************************************************
 * exercise: approximately e
 ********************************************************************)

(**
  *  [fact n] is n-factorial.
  *   Requires:
   - [n] >= 0
*)
let rec fact (n: int) : int =
  if (n = 0 || n = 1) then 1
  else n * (fact (n-1))

(**
  *  [kth_term x k] calculates the kth term of the e^x approximation sum.
  *  Requires:
  *- [k] >= 0
*)
let kth_term (x: float) (k: int) : float =
  (x ** (float_of_int k)) /. (float_of_int (fact k))

(**
  *  [e_terms_from x k] is a sequence representing the sum approximation of e^x
  *   starting from the kth term onwards.
  *   Requires:
  *     - [k] >= 0
*)
let rec e_terms_from (x: float) (k: int): float sequence =
  Cons(kth_term x k, fun () ->
      e_terms_from x (k+1))

(**
  *  [e_terms x] is a sequence representing the infinite sum approximation of e^x
*)
let e_terms (x: float) : float sequence = e_terms_from x 0

(**
  *  [running_total s r] adds [r] to the head of sequence [s], then updates [r] to
  *  become [r] + head of s. When evaluating the kth term of s, [r] is
  *  effectively the cumulative sum of the first (k-1) terms in [s].
*)
let rec running_total (s: float sequence) (r: float) : float sequence =
  match s with
  | Cons(h, tf) -> Cons (h +. r, fun () -> running_total (tf ()) (h +. r))

(**
  *  [total <a; b; c; ...>] is a running total of the input elements,
  *  i.e., <a; a+.b; a+.b+.c; ...>.
*)
let total (s: float sequence): float sequence = running_total s 0.0

(**
  *  [within_helper eps prev s] checks the head of sequence [s] with the previous
  *  value [prev] in sequence [s].
  *  Requires:
  *    - [eps]: must be strictly positive.
*)
let rec within_helper (eps: float) (prev: float) (s: float sequence): float =
  match s with
  | Cons(h, tf) -> (
      if (abs_float (prev -. h)) < eps then h
      else within_helper eps h (tf ())
    )

(**
  *  [within eps s] is the first element of s for which the absolute difference
  *  between that element and the element before it is strictly less than [eps].
  *  If there is no such element, [within] is permitted not to terminate
  *  (i.e., go into an "infinite loop").
  *  Requires:
  *    - [eps]: must be strictly positive.
*)
let within (eps: float) (s: float sequence) : float =
  within_helper eps max_float s

(** [e x eps] is $e^x$ computed using a finite prefix of the infinite summation
  * formula given in the exercise. The computation halts when the absolute
  * difference between successive approximations is below [eps].
  * Requires: [eps] > 0.
*)
let e (x: float) (eps: float) : float =
  e_terms x |> total |> within eps

(********************************************************************
 * exercise: different sequence rep
 ********************************************************************)

(* We isolate this solution in a module to isolate it from the rest of the
   file. This representation is even lazier than our original because
   the head of the sequence won't be produced until the thunk is forced,
   whereas before the head was always available and only the tail
   was a thunk. *)
module DifferentsequenceRep = struct
  type 'a sequence = Cons of (unit -> 'a * 'a sequence)

  let hd (Cons th) =
    th () |> fst

  let tl (Cons th) =
    th () |> snd

  let rec from n =
    Cons (fun () -> (n, from (n + 1)))

  let rec nats =
    from 0

  let rec map f (Cons th) =
    Cons begin fun () ->
      let h, t = th () in
      (f h, map f t)
    end
end


(********************************************************************
 * exercise: lazy hello
 ********************************************************************)

(**
 * Lazy value that prints a greeting the first time it is forced
 * but not thereafter.
*)
let hello =
  lazy (print_endline "Hello lazy world")


(********************************************************************
 * exercise: lazy and
 ********************************************************************)

(**
 * [b1 &&& b2] is the logical OR of [b1] and [b2]. It evaluates from
 * left to right, and only forces its arguments if necessary.
*)
let (&&&) b1 b2 =
  if Lazy.force b1 then
    Lazy.force b2
  else
    false

(********************************************************************
 * exercise: lazy sequence
 ********************************************************************)

module Lazysequence = struct
  type 'a lazysequence =
    | Cons of 'a * 'a lazysequence Lazy.t

  let rec map f (Cons (h, t)) =
    Cons (f h, lazy (map f (Lazy.force t)))

  let rec filter p (Cons (h, t)) =
    let tl = lazy (filter p (Lazy.force t)) in
    if p h then
      Cons (h, tl)
    else
      Lazy.force tl
end

(********************************************************************
 * exercise: lazy list
 ********************************************************************)

module type LazyList = sig
  type 'a lazylist

  val hd : 'a lazylist -> 'a

  val tl : 'a lazylist -> 'a lazylist

  val take : int -> 'a lazylist -> 'a list

  val from : int -> int lazylist

  val map : ('a -> 'b) -> 'a lazylist -> 'b lazylist

  val filter : ('a -> bool) -> 'a lazylist -> 'a lazylist
end


module LazyListImpl : LazyList = struct
  type 'a lazylist = Cons of 'a * 'a lazylist Lazy.t

  let hd (Cons (h, _)) =
    h

  let tl (Cons (_, t)) =
    Lazy.force t

  let rec take n (Cons (h, t)) =
    if n = 0 then
      []
    else
      h :: take (n-1) (Lazy.force t)

  let rec from n =
    Cons (n, lazy (from (n+1)))

  let rec map f (Cons (h, t)) =
    Cons (f h, lazy (map f (Lazy.force t)))

  let rec filter p (Cons (h, t)) =
    if p h then
      Cons (h, lazy (filter p (Lazy.force t)))
    else
      filter p (Lazy.force t)
end
