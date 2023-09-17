(********************************************************************
 * exercise: twice, no arguments
 ********************************************************************)
(* val quad : int -> int
 * val fourth : int -> int
 * quad is a function because twice is a higher-order function that takes
 * a function as argument and returns a function.
*)

(********************************************************************
 * exercise: mystery operator 1
 ********************************************************************)
(* [($)] is simply function application: it applies its first argument
 * (which needs to be a function) to its second. Writing [f $ x] seems
 * unnecessary since we could have written [f x], but [($)] is useful
 * for changing the precedence of operators. For example,
 * [square $ 2 + 2] is interpreted as [square (2 + 2) = 16] whereas
 * [square 2 + 2] is interpreted as [(square 2) + 2 = 6].
 *
 * Of course, it's much better to write [2 + 2 |> square], since this
 * makes data flow more explicit.
*)


(********************************************************************
 * exercise: mystery operator 2
 ********************************************************************)
(* [(@@)] is function composition: it chains two functions together
 * into a new function which applies its argument first to the right
 * side of [(@@)], and feeds the output to the left.
*)


(********************************************************************
 * exercise: repeat
 ********************************************************************)

(* returns: [f] applied to [x], [n] times.
 * requires: [n >= 0]
*)
let rec repeat f n x =
  if n = 0 then x else repeat f (n - 1) (f x)


(********************************************************************
 * exercise: products
 ********************************************************************)

(* returns: product of all elements in [lst]
*)
let product_left lst = List.fold_left ( *. ) 1.0 lst

(* returns: product of all elements in [lst]
*)
let product_right lst = List.fold_right ( *. ) lst 1.0

(********************************************************************
 * exercise: sum_cube_odd
 ********************************************************************)

(* returns:  [from i j l] is the list containing the integers from
 *   [i] to [j], inclusive, followed by the list [l].
 * example:  [from 1 3 [0] = [1;2;3;0]] *)
let rec from i j l =
  if i>j then l
  else from i (j-1) (j::l)

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
*)
let (--) i j =
  from i j []

(* returns: sum of the cubes of all odd numbers up to and including [n].
*)
let sum_cube_odd n =
  let l = 0 -- n in
  let odds_only = List.filter (fun i -> i mod 2 = 1) l in
  let odd_cubes = List.map (fun i -> i * i * i) odds_only in
  List.fold_left (+) 0 odd_cubes

(********************************************************************
 * exercise: sum_cube_odd pipeline
 ********************************************************************)

(* returns: sum of the cubes of all odd numbers up to and including [n].
*)
let sum_cube_odd_p n =
  0 -- n
  |> List.filter (fun i -> i mod 2 = 1)
  |> List.map (fun i -> i * i * i)
  |> List.fold_left (+) 0


(********************************************************************
 * exercise: exists
 ********************************************************************)

(* returns: [true] if and only if some element of the given list
 *  satisfies [p]
*)
let rec exists_rec p = function
  | [] -> false
  | h :: t -> p h || exists_rec p t

(* returns: [true] if and only if some element of the given list
 *  satisfies [p]
*)
let exists_fold p l = List.fold_left (fun acc elt -> acc || p elt) false l

(* something to consider:  why is it better to write [acc || p elt]
 * rather than [p elt || acc] above? *)

(* returns: [true] if and only if some element of the given list
 *  satisfies [p]
*)
let exists_lib = List.exists

(* We could also write [let exists_lib p lst = List.exists p lst], but
 * that is overly verbose. *)

(********************************************************************
 * exercise: library uncurried
 ********************************************************************)

let uncurried_append (lst,e) = List.append lst e

let uncurried_compare (c1,c2) = Char.compare c1 c2

let uncurried_max (n1,n2) = Stdlib.max n1 n2


(********************************************************************
 * exercise: terse product
 ********************************************************************)

let terse_product_left = List.fold_left ( *. ) 1.0

let terse_product_right = ListLabels.fold_right ~f:( *. ) ~init:1.0


(********************************************************************
 * exercise: map composition
 ********************************************************************)
(*   List.map f (List.map g lst)
 * = List.map (fun elt -> f (g elt)) lst
 * = List.map (f @@ g) lst  (* using [(@@)] defined in the lab *)
*)


(********************************************************************
 * exercise: more list fun
 ********************************************************************)

(* returns: all elements in [lst] that have length greater than 3.
*)
let at_least_three lst =
  List.filter (fun s -> String.length s > 3) lst

(* returns: [lst] with each element incremented by [1.0].
*)
let add_one lst =
  List.map (fun x -> x +. 1.0) lst

(* returns: [s] where [s] is the concatenation of all elements in
 *   [strs] seperated by [sep].
*)
let join_with strs sep =
  match strs with
  | [] -> ""
  | x :: xs ->
    List.fold_left (fun combined s -> combined ^ sep ^ s) x xs


(********************************************************************
 * exercise: association list keys
 ********************************************************************)

(* here are a few solutions of varying efficiency *)

(* returns: a list of the unique keys in [lst] in no particular order.
 * efficiency:  O(n^2) time, where n is the number of elements in [lst],
 *  and O(n) stack space.
*)
let keys1 lst =
  List.fold_right
    (fun (k, _) acc -> k :: List.filter (fun k2 -> k <> k2) acc)
    lst
    []

(* returns: a list of the unique keys in [lst] in no particular order.
 * efficiency:  O(n^2) time, where n is the number of elements in [lst],
 *  and O(1) stack space.
*)
let keys2 lst =
  List.fold_left
    (fun acc (k, _) -> if List.exists ((=) k) acc then acc else k::acc)
    []
    lst

(* returns: a list of the unique keys in [lst] in no particular order.
 * efficiency:  O(n log n) time, where n is the number of elements in [lst],
 *  and O(log n) stack space.
*)
let keys3 lst =
  lst
  |> List.rev_map fst
  |> List.sort_uniq Stdlib.compare

(* the above code would fit on one line:
     lst |> List.rev_map fst |> List.sort_uniq Stdlib.compare
   but it's easier to read on three. *)

(********************************************************************
 * exercise: valid matrix
 ********************************************************************)

(* returns: [true] iff the matrix has at least one row and column, and
 * each row has the same number of columns.
*)
let is_valid_matrix = function
  | [] -> false
  | r :: rows ->
    let m = List.length r in
    m > 0 && List.for_all (fun r' -> m = List.length r') rows


(********************************************************************
 * exercise: row vector add
 ********************************************************************)

(* returns: element-wise summation of two lists
 * requires: [v1] and [v2] has the same length
*)
let add_row_vectors =
  List.map2 (+)

(********************************************************************
 * exercise: matrix add
 ********************************************************************)

let add_matrices =
  List.map2 add_row_vectors

(********************************************************************
 * exercise: matrix multiply
 ********************************************************************)

let rec transpose ls =
  let rec transpose' acc = function
    | [] | [] :: _ -> List.rev acc
    | ls -> transpose' (List.map List.hd ls :: acc) (List.map List.tl ls)
  in transpose' [] ls

let dot = List.fold_left2 (fun acc x y -> acc + x * y) 0

let multiply_matrices m1 m2 =
  List.map (fun row -> List.map (dot row) (transpose m2)) m1

(* another solution *)

let inner matrix row = List.map (dot row) (transpose matrix)

let multiply_matrices' m1 m2 =
  List.map (inner m2) m1