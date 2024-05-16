(********************************************************************
 * exercise: list expressions
 ********************************************************************)

let lst1 = [1; 2; 3; 4; 5]
let lst2 = 1 :: 2 :: 3 :: 4 :: 5 :: []
let lst3 = [1] @ [2; 3; 4] @ [5]

(********************************************************************
 * exercise: product
 ********************************************************************)

(* returns: the product of all the elements of [lst], or [1] if [lst]
 *   is empty. 
*)
let rec product lst =
  match lst with
  | [] ->  1
  | h :: t -> h * product t

(* Here's a simpler way to write that using the [function] syntactic sugar
 *   discussed in the notes...
*)
let rec product' = function
  | [] ->  1
  | h :: t -> h * product' t

(********************************************************************
 * exercise: concat
 ********************************************************************)

(* returns: the concatenation of all the strings in list, or the
 *   empty string if list is empty.
*)
let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t

(********************************************************************
 * exercise: patterns
 ********************************************************************)

(* returns:  whether the first element of the input is ["bigred"] 
*)
let first_is_bigred = function
  | [] -> false
  | h :: _ -> h = "bigred"

(* returns:  whether the input has exactly two or four elements 
*)
let two_or_four_elements = function
  | _ :: _ :: [] -> true
  | _ :: _ :: _ :: _ :: [] -> true
  | _ -> false

(* returns: whether the first two elements of the input are equal 
*)
let eq_first_two = function
  | a :: b :: _ -> a = b
  | _ -> false

(********************************************************************
 * exercise: library
 ********************************************************************)

(* returns:  the fifth element of the input list, or zero if the 
 *   list is empty 
*)
let fifth_element lst =
  if (List.length lst) >= 5 then List.nth lst 4 else 0

(* returns: the input list, sorted in descending order 
*)
let sort_list_descending lst = 
  List.rev (List.sort Stdlib.compare lst)

(* A more idiomatic way to write the above function is 
 *   with the pipeline operator.  This makes use of 
 *   partial application with List.sort. 
*)
let sort_list_descending' lst = 
  lst |> List.sort Stdlib.compare |> List.rev 

(********************************************************************
 * exercise: library puzzle
 ********************************************************************)

(* returns: the last element of [lst]
 * requires: [lst] is nonempty
*)
let last_element lst = 
  List.nth lst (List.length lst - 1)

(* another solution... 
*)
let last_element' lst = 
  lst |> List.rev |> List.hd

(* returns: whether [lst] contains any zeros
*)
let any_zeros lst = 
  List.exists (fun x -> x = 0) lst

(********************************************************************
 * exercise: take drop
 ********************************************************************)

(* returns:  [take n lst] is the first [n] elements of [lst], or
 *   just [lst] if [lst] has fewer than [n] elements. 
 * requires: [n >= 0]
*)
let rec take n lst =
  if n = 0 then [] else match lst with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs

(* returns:  [drop n lst] is all but the first [n] elements of [lst],
 *   or just [[]] if [lst] has fewer than [n] elements.
 * requires: [n >= 0]
*)
let rec drop n lst =
  if n = 0 then lst else match lst with
    | [] -> []
    | _ :: xs -> drop (n - 1) xs

(********************************************************************
 * exercise: take drop tail
 ********************************************************************)

(* returns: [take_rev n xs acc] is [lst1 @ acc], where [lst] is
 *   the first [n] elements of [xs] (or just [xs] if [xs] has 
 *   fewer than [n] elements) in reverse order. 
 * requires: [n >= 0] *)
let rec take_rev n xs acc = 
  if n = 0 then acc else match xs with
    | [] -> acc
    | x :: xs' -> take_rev (n - 1) xs' (x :: acc)

(* returns:  [take n lst] is the first [n] elements of [lst], or
 *   just [lst] if [lst] has fewer than [n] elements. 
 * requires: [n >= 0]
*)
let take_tr n lst =
  take_rev n lst [] |> List.rev

(* In the solution above, we factored out a helper function called
 * [take_rev].  It is tail recursive.  The helper function takes
 * the same arguments as the original function, and one additional
 * argument [acc] called an *accumulator*.  We accumulate the answer
 * in it, eventually returning it when either [xs] is empty
 * or [n] is 0.  This is a general recipe one can follow in making
 * any recursive function be tail recursive. That same recipe was
 * followed in creating the definition of [(--)] below. *)

(* the "natural" solution to [drop] above is already tail recursive *)
let drop_tr = drop

(* returns:  [from i j l] is the list containing the integers from
 *   [i] to [j], inclusive, followed by the list [l].
 * example:  [from 1 3 [0] = [1;2;3;0]] *)
let rec from i j l =
  if i > j then l
  else from i (j - 1) (j :: l)

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
*) 
let (--) i j =
  from i j []

let longlist = 0 -- 1_000_000

(* [take 1_000_000 longlist] should produce a stack overflow,
 * but [take_tr 1_000_000 longlist] should not. *)

(********************************************************************
 * exercise: unimodal
 ********************************************************************)

(** returns: whether the input list is monotonically decreasing *)
let rec is_mon_dec = function
  | [] | [_] -> true
  | h1 :: (h2 :: t2 as t) -> 
    h1 >= h2 && is_mon_dec t

(** returns: whether the input list is monotonically increasing
    then monotonically decreasing *)
let rec is_mon_inc_then_dec = function
  | [] | [_] -> true
  | h1 :: (h2 :: t2 as t) as lst -> 
    if h1 <= h2 then is_mon_inc_then_dec t else is_mon_dec lst

let is_unimodal lst = 
  is_mon_inc_then_dec lst

(********************************************************************
 * exercise: powerset
 ********************************************************************)

(* returns: [powerset s] is the list representing the set of all subsets of s
 * requires: [s] is a set-like list (no duplicate elements) *)
let rec powerset = function
  | [] -> [ [] ]
  | x :: s -> let p = powerset s in
    List.map (List.cons x) p @ p

(********************************************************************
 * exercise: print int list rec
 ********************************************************************)

(* effects: prints all elements in the given list on a separate line *)
let rec print_int_list = function
  | [] -> ()
  | hd :: tl -> print_endline (string_of_int hd); print_int_list tl

(********************************************************************
 * exercise: print int list iter
 ********************************************************************)

(* effects: prints all elements in [lst] on a separate line *)
let print_int_list' lst =
  List.iter (fun x -> print_endline (string_of_int x)) lst

(********************************************************************
 * exercise: student
 ********************************************************************)

(* we are given this type *)
type student = { first_name : string ; last_name : string ; gpa : float }

(* expression with type [student] *)
let s =
  { first_name = "Ezra"; last_name = "Cornell"; gpa = 4.3 }

(* expression with type [student -> string * string] *)
let get_full_name student =
  student.first_name, student.last_name

(* expression with type [string -> string -> float -> student] *)
let make_stud first last g =
  { first_name = first; last_name = last; gpa=g }


(********************************************************************
 * exercise: pokerecord
 ********************************************************************)

type poketype = Normal | Fire | Water

(* define the type pokemon record *)
type pokemon = { name : string ; hp : int ; ptype : poketype }

let charizard = { name = "charizard"; hp = 78; ptype = Fire }

let squirtle = { name = "squirtle"; hp = 44; ptype = Water }

(********************************************************************
 * exercise: safe hd and tl
 ********************************************************************)

(* returns: [Some x] if the head of [lst] is [x], or [None]
 *   if [lst] is empty.
*)
let safe_hd = function
  | [] -> None
  | h::_ -> Some h

(* returns: [Some x] if the tail of [lst] is [x], or [None]
 *   if [lst] is empty.
*)
let safe_tl = function
  | [] -> None
  | _::t -> Some t


(********************************************************************
 * exercise: pokefun
 ********************************************************************)

(* returns: [Some pokemon] if [pokemon] has the highest [hp] in the
 *   given list, or [None] if the list is empty.
*)
let rec max_hp = function
  | [] -> None
  | poke1::t -> begin
      match max_hp t with
      | None -> Some poke1
      | Some poke2 -> Some (if poke1.hp >= poke2.hp then poke1 else poke2)
    end


(********************************************************************
 * exercise: date before
 ********************************************************************)

type date = int * int * int

(* returns: true if [date1] comes strictly before [date2];
 *   false otherwise
 * requires: [date1] and [date2] are valid date triples
*)
let is_before date1 date2 =
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
  y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)


(********************************************************************
 * exercise: earliest date
 ********************************************************************)

(* returns: [Some d] if [d] is the earliest date in the given list,
 *   or [None] if the list is empty.
*)
let rec earliest = function
  | [] -> None
  | d1::t -> begin
      match earliest t with
      | None -> Some d1
      | Some d2 -> Some (if is_before d1 d2 then d1 else d2)
    end


(********************************************************************
 * exercise: assoc list
 ********************************************************************)

(* insert a binding from key k to value v in association list d *)
let insert k v d = (k,v)::d

(* find the value v to which key k is bound, if any, in the assocation list *)
let rec lookup k = function
  | [] -> None
  | (k',v)::t -> if k=k' then Some v else lookup k t

let dict = insert 3 "three" (insert 2 "two" (insert 1 "one" []))
let some_two = lookup 2 dict
let none = lookup 4 dict

(********************************************************************
 * exercise: cards
 ********************************************************************)

type suit = Hearts | Spades | Clubs | Diamonds

type rank = Number of int | Ace | Jack | Queen | King

type card = { suit: suit; rank: rank }

let ace_of_clubs : card = { suit = Clubs; rank = Ace }
let queen_of_hearts : card = { suit = Hearts; rank = Queen }
let two_of_diamonds : card = { suit = Diamonds; rank = Number 2 }
let seven_of_spades : card = { suit = Spades; rank = Number 7 }


(********************************************************************
 * exercise: matching
 ********************************************************************)

(* (Some x)::tl *)
let lst1 : int option list = [None; Some 0]
(* [Some 3110; None] *)
let lst2 : int option list = [Some 3410; None]
(* [Some x; _] *)
let lst3 : int option list = [None; Some 0]
(* h1::h2::tl *)
let lst4 : int option list = [Some 0]
(* h :: tl *)
(* Impossible. This matches any list of non-zero length regardless of type.
 * The only way to NOT match this expression is the empty list []. *)


(********************************************************************
 * exercise: quadrant
 ********************************************************************)

type quad = I | II | III | IV
type sign = Neg | Zero | Pos

(** [sign x] is [Zero] if [x = 0]; [Pos] if [x > 0]; and [Neg] if [x < 0]. *)
let sign x =
  if x = 0 then Zero
  else if x > 0 then Pos
  else Neg

(** [quadrant (x,y)] is [Some q] if [(x, y)] lies in quadrant [q], or [None] if
    it lies on an axis. *)
let quadrant (x,y) =
  match sign x, sign y with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _ -> None


(********************************************************************
 * exercise: quadrant when
 ********************************************************************)

(** [quadrant_when (x,y)] is [Some q] if [(x, y)] lies in quadrant [q], 
    or [None] if it lies on an axis. *)
let quadrant_when = function
  | x,y when x > 0 && y > 0 -> Some I
  | x,y when x < 0 && y > 0 -> Some II
  | x,y when x < 0 && y < 0 -> Some III
  | x,y when x > 0 && y < 0 -> Some IV
  | _ -> None


(********************************************************************
 * exercise: depth
 ********************************************************************)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

(** [depth t] is the number of nodes in any longest path from the root to a 
    leaf in tree [t]. *)
let rec depth = function
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)


(********************************************************************
 * exercise: shape
 ********************************************************************)

(** [same_shape t1 t2] is [true] if and only if [t1] and [t2] have the same
    shape. *)
let rec same_shape t1 t2 =
  match t1, t2 with
  | Leaf, Leaf -> true
  | Node (_,l1,r1), Node (_,l2,r2) -> (same_shape l1 l2) && (same_shape r1 r2)
  | _ -> false


(********************************************************************
 * exercise: list max exn
 ********************************************************************)

(** [list_max_safe x xs] is [x] if [xs] is empty, and otherwise
    is the maximum element of [x::xs]. *)
let rec list_max_safe x = function
  | [] -> x
  | h::t -> list_max_safe (Stdlib.max x h) t

(** [list_max lst] is the maximum element of [lst] as determined by
    [Stdlib.max].
    Raises:  [Failure "empty"] if [lst] is empty. *)
let list_max = function
  | [] -> failwith "list_max"
  | h::t -> list_max_safe h t

(********************************************************************
 * exercise: list max exn string
 ********************************************************************)

(* returns: [list_max_string lst] is the string representing the maximum
 *   integer in [lst].
 * raises: [Failure "list_max_string"] if [lst] is empty.
*)
let list_max_string lst =
  try string_of_int (list_max lst) with
  | Failure _ -> "empty"

(********************************************************************
 * exercise: list max exn ounit
 ********************************************************************)

(* you'll need to put this code in a separate file, as usual, to run the tests *)
(*
open OUnit2

let tests = "suite" >::: [
  "empty" >:: (fun _ -> assert_raises (Failure "list_max") (fun () -> list_max []));
  "nonempty" >:: (fun _ -> assert_equal 8 (list_max [3; 1; 4; 8]))
]

let _ = run_test_tt_main tests
*)

(********************************************************************
 * exercise: quadrant poly
 ********************************************************************)

(* although the compiler will infer the output types manually
 * annotated below, we include them anyway in the solution just
 * so that you can see them here. *)

let sign_poly x : [> `Neg | `Pos | `Zero] =
  if x < 0 then `Neg
  else if x = 0 then `Zero
  else `Pos

let quadrant (x,y) : [> `I | `II | `III | `IV ] option =
  match sign_poly x, sign_poly y with
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IV
  | _ -> None

(********************************************************************
 * exercise: is_bst
 ********************************************************************)

(* we leave this solution unstated as a challenge. *)
