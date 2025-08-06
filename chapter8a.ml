(********************************************************************
 * exercise: spec game
 ********************************************************************)

(* Here are some specs that you might end up with;
   they are not the only possibilities. *)

module type S = sig
  (* [num_vowels s] is the number of occurences of characters
   *   'A', 'a', 'E', 'e', 'I', 'i', 'O', 'o', 'U', or 'u' in [s].
   *   We ignore uses of 'y' as a vowel.
  *)
  val num_vowels : string -> int

  (* [is_sorted l = true] if and only if for all [i, j] such that
   *   [0 <= i <= j < length l], it is the case that [nth l i <= nth l j].
  *)
  val is_sorted : 'a list -> bool

  (* [sort l] returns [l'] satisfying
   *    - [l'] is a permutation of [l], and
   *    - [is_sorted l' = true].
  *)
  val sort : 'a list -> 'a list

  (* [max l] returns [x] satisfying:
   *   - [List.mem x l = true], and
   *   - for all [i] such that [0 <= i < length l], [nth l i <= x]
   * requires: [l] is not empty.
  *)
  val max : 'a list -> 'a

  (* [is_prime n = true] if and only if:
   *   - n > 1
   *   - for all [m] such that [1 < m < n], [n mod m <> 0]
  *)
  val is_prime : int -> bool
end

(********************************************************************
 * exercise: poly spec
 ********************************************************************)

(* this is a sample solution; others are possible *)

(* [Poly] represents immutable polynomials with integer coefficients. *)
module type Poly = sig
  (* [t] is the type of polynomials *)
  type t

  (* [eval x p] is [p] evaluated at [x].  For example, if [p] represents
   * $3x^3 + x^2 + x$, then evaluating [p] at [10] would yield [3110]. *)
  val eval : int -> t -> int


  (* # Construction *)

  (* [from_coefficients [c0, ..., cn]] constructs a new polynomial [p] where
   *  [p] represents the polynomial $c_n x^n + ... + c_1 x + c_0$.
  *)
  val from_coefficients : int list -> t


  (* # Querying *)

  (* [coefficient i p] returns the coefficient [c_i] of the term with power
   *   [i]. That is, if $p = c_n x^n + ... + c_i x^i + ... + c_0$, then
   *   [coefficient i p = c_i]. If $i > degree p$, then [coefficient i p = 0].
   * requires: i >= 0
  *)
  val coefficient : int -> t -> int

  (* [degree p] returns the largest [i] such that [coefficient i p <> 0].
   * The result is [-1] if there is no such [i].
  *)
  val degree : t -> int


  (* # Operations *)

  (* [scale c p] returns [p'] such that
   * [coefficient i p' = c * coefficient i p] for all [i >= 0].
  *)
  val scale : int -> t -> t

  (* [add p1 p2] returns [p] such that
   * [coefficient i p = coefficient i p1 + coefficient i p2] for all [i >= 0].
  *)
  val add : t -> t -> t

  (* [multiply p1 p2] returns [p] such that
   * [coefficient i p = sum [ coefficient j p1 * coefficient (i - j) p2 | 0 <= j <= i]]
   * for all [i >= 0].
  *)
  val multiply : t -> t -> t
end


(********************************************************************
 * exercise: poly impl
 ********************************************************************)

module PolyImpl : Poly = struct
  (* [c0; c1; ...; cn] represents the polynomial
   *  $c0 + c1 x + ... + cn x^n$. The empty list represents 0,
   *  as does [0], and [0;0], etc. *)
  type t = int list

  let rec eval x = function
    | [] -> 0
    | c :: rest -> c + x * eval x rest


  let from_coefficients p =
    p


  let coefficient i p =
    if i < List.length p then List.nth p i else 0

  let degree p =
    (* [drop_while p l] is [l] with its longest prefix satisfying
     * [p] is removed. We use this to remove leading zeros from the
     * coefficient list.
    *)
    let rec drop_while f = function
      | [] -> []
      | x :: xs -> if f x then drop_while f xs else x :: xs
    in
    List.length (drop_while (fun x -> x = 0) (List.rev p)) - 1


  (* [scale c p] returns [p'] such that
   * [coefficient i p' = c * coefficient i p] for all [i >= 0].
  *)
  let scale c =
    List.map (fun c' -> c * c')

  let rec add p1 p2 =
    match p1, p2 with
    | [], _ -> p2 (* empty list is the 0 polynomial *)
    | _, [] -> p1 (* empty list is the 0 polynomial *)
    | c1 :: p1, c2 :: p2 -> c1 + c2 :: add p1 p2

(*
 * Multiplying a polynomial [p1] with degree [d1] and [p2] with
 * degree [d2] will generate a polynomial with degree at most
 * [d1 + d2], that is, the highest power of $x$ will be at most
 * $x^(d1 + d2)$ (zero polynomial multiplied with any polynomial
 * is still the zero polynomial with degree -1; that's why we say
 * at most). Rather than thinking about the entire polynomial that
 * is the results of the multiplication, we will think about how we can
 * generate a specific coefficient [c_i] for the [i]th power of
 * [x]. Then it is easy to generate all coefficients
 * [c_0, ..., c_(d1 + s2)] one at a time.
 *
 * To get [i] copies of [x] (that is, [x^i]) we need to select
 * all coefficients [c_j] from [p1] and [c_k] from [p2] such that
 * [j + k = i]. For example, if we want [x^3], we can get it by
 * multiplying [x] with [x^2] or [1] with [x^3] and so on. The problem
 * then becomes identifying all such pairs of coefficients, multiplying
 * them together and adding up the results.
 *)
  let rec multiply p1 p2 =
    (* [tabulate f s n] returns a new list of length [n] where the [i]-th
     * element in the list is [f (s + i)]. [s] serves as a starting value.
     * If we give [tabulate] the function that generates the [i]th coefficient
     * of the result, and specify a suitable bound, we will get all the
     * coefficients that correspond to the multiplication.
     * requires: [s >= 0]
    *)
    let rec tabulate f s = function
      | n when n <= 0 -> []
      | n -> f s :: tabulate f (s + 1) (n - 1)
    in

    (* [take l n] returns the first [n] elements of [l]. If [l] doesn't
     * have enough elements, the tail of the result is padded with [0]s.
    *)
    let rec take l n =
      match l, n with
      | _      , 0 -> []
      | []     , n -> 0 :: take [] (n - 1)
      | x :: xs, n -> x :: take xs (n - 1)
    in

    (* [dotProduct [x_1; ...; x_n] [y_1; ...; y_n]] is
     * [sum [x_1 * y_1; ...; x_n * y_n]].
    *)
    let rec dotProduct l1 l2 =
      match l1, l2 with
      | [], _ -> 0
      | _, [] -> 0
      | x1 :: l1, x2 :: l2 -> x1 * x2 + dotProduct l1 l2
    in

    (* [ith] is the [i]th coefficient of the resulting polynomial.
       requires: [i >= 0]
    *)
    let ith i =
      (* All coefficients of [p1] (respectively p2) up to and including
       * the coefficient of [x^i].
      *)
      let p1_up_to_ith_power = take p1 (i + 1) in
      let p2_up_to_ith_power = take p2 (i + 1) in
      (* Match each coefficient [c_j] in [p1] with coefficient
       * [c_k = c_(i - j)] in [p2]. By definition, this ensures
       * [j + k = i] and these are the only such coefficients.
       * Multiply the pairs together and the results up (i.e.
       * take the [dotProduct].) This is the [i]th coefficient
       * by the specification of multiplication.
      *)
      dotProduct p1_up_to_ith_power (List.rev p2_up_to_ith_power)
    in
    tabulate ith 0 (degree p1 + degree p2 + 1)
end

(********************************************************************
 * exercise: function maps
 ********************************************************************)

module FnMap = struct

  (* AF: The function [f] represents a map that binds [k] to [v]
   *   iff [f k] is [v]. If [k] is unbound in the map then [f k]
   *   raises [Not_found].
   * RI: none
  *)
  type ('k,'v) t = 'k -> 'v

  let empty _ = raise Not_found

  let mem k m =
    try
      let _ = m k in true
    with
      Not_found -> false

  let find = (|>)

  let add k v m =
    fun key -> if key=k then v else m key

  let remove k m =
    fun key -> if key=k then raise Not_found else m key

  let rep_ok m = m

end

(********************************************************************
 * exercise: set black box
 ********************************************************************)

(* We don't have a solution on file for this.  If you'd like
   to contribute one, please contact the professor. *)

(********************************************************************
 * exercise: set glass box
 ********************************************************************)

(* We don't have a solution on file for this.  If you'd like
   to contribute one, please contact the professor. *)

(********************************************************************
 * exercise: random lists
 ********************************************************************)

(* We don't have a solution on file for this.  If you'd like
   to contribute one, please contact the professor. *)

(********************************************************************
 * exercise: qcheck odd divisor
 ********************************************************************)

(** [odd_divisor x] is an odd divisor of [x].
    Requires: [x >= 0]. *)
    let odd_divisor x =
      if x < 3 then 1 else
        let rec search y =
          if y >= x then y  (* exceeded upper bound *)
          else if x mod y = 0 then y  (* found a divisor! *)
          else search (y + 2) (* skip evens *)
        in search 3

let is_odd n = n mod 2 = 1
let is_divisor_of d n = n mod d = 0
let t = QCheck.Test.make QCheck.small_int
    (fun i ->
       let d = odd_divisor i
       in is_odd d && is_divisor_of d i)

(********************************************************************
 * exercise: qcheck avg
 ********************************************************************)

(** [avg [x1; ...; xn]] is [(x1 + ... + xn) / n].
     Requires: the input list is not empty. *)
let avg lst =
let rec loop (s, n) = function
  | [] -> (s, n)
  | [ h ] -> (s + h, n + 1)
  | h1 :: h2 :: t -> if h1 = h2 then loop (s + h1, n + 1) t
    else loop (s + h1 + h2, n + 2) t
in
let (s, n) = loop (0, 0) lst
in float_of_int s /. float_of_int n

let ref_avg lst =
  (float_of_int (List.fold_left (+) 0 lst))
  /. (float_of_int (List.length lst))

let t = QCheck.Test.make
    (QCheck.list_of_size (QCheck.Gen.int_range 1 10) QCheck.small_int)
    (fun lst -> avg lst = ref_avg lst)

let _ = QCheck_runner.run_tests_main [t]
