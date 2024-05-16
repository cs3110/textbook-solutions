(********************************************************************
 * exercise: complex synonym
 ********************************************************************)

module type ComplexSig = sig
  type t = float * float
  val zero : t
  val add : t -> t -> t
end

(********************************************************************
 * exercise: complex encapsulation
 ********************************************************************)

module Complex : ComplexSig = struct
  type t = float * float
  let zero = (0., 0.)
  let add (r1,i1) (r2,i2) = r1 +. r2, i1 +. i2
end

(*
- remove `zero` from the structure:
  Signature mismatch: The value `zero' is required but not provided.
- remove `add` from the signature:
  No error, but now clients can't add complex numbers.
- change `zero` in the structure to `let zero = 0, 0`:
  Signature mismatch: Values do not match:
    val zero : int * int
    is not included in
    val zero : t
*)

(********************************************************************
 * exercise: big list queue
 ********************************************************************)

module ListQueue = struct
  type 'a queue = 'a list

  let empty = []

  let is_empty q = (q = [])

  let enqueue x q = q @ [x]

  let peek = function
    | []   -> failwith "Empty"
    | x::_ -> x

  let dequeue = function
    | []   -> failwith "Empty"
    | _::q -> q
end

(* returns: a [ListQueue] filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n=0 then q
    else loop (n-1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty

(* on one TA's machine, about 20000 elements will bring a delay of
 * at least 10 seconds *)


(********************************************************************
 * exercise: big batched queue
 ********************************************************************)

module BatchedQueue = struct
  type 'a t = {outbox:'a list; inbox:'a list}

  let empty = {outbox=[]; inbox=[]}

  let is_empty = function
    | {outbox=[]; inbox=[]} -> true
    | _ -> false

  let norm = function
    | {outbox=[]; inbox} -> {outbox=List.rev inbox; inbox=[]}
    | q -> q

  let enqueue x q = norm {q with inbox=x::q.inbox}

  let peek = function
    | {outbox=[]; _} -> None
    | {outbox=x::_; _} -> Some x

  let dequeue = function
    | {outbox=[]; _} -> None
    | {outbox=_::xs; inbox} -> Some (norm {outbox=xs; inbox})
end

(* returns: a [BatchedQueue] filled with [n] elements. *)
let fill_BatchedQueue n =
  let rec loop n q =
    if n=0
    then q
    else loop (n-1) (BatchedQueue.enqueue n q)
  in
  loop n BatchedQueue.empty

(* on one TA's machine, about 50_000_000 elements will bring a delay of
 * at least 10 seconds*)


(********************************************************************
 * exercise: queue efficiency
 ********************************************************************)

(* [ListQueue.enqueue] insert each new element at the end of a list,
 * which has to walk down the entire list. This naturally takes time
 * that is linear in the length of the queue. Doing this [n] times,
 * even starting with an empth queue, will take time [1 + 2 + ... + n],
 * which is in [O(n^2)].
 *
 * On the other hand, [BatchedQueue.enqueue] simply uses (::) which runs
 * in constant time (like all constructors) irregardless of the size of
 * the list. Repeating this [n] times will take [O(n)] time.
*)

(********************************************************************
 * exercise: binary search tree map
 ********************************************************************)

module type Map = sig
  type ('k, 'v) t
  val empty  : ('k, 'v) t
  val insert : 'k -> 'v -> ('k,'v) t -> ('k,'v) t
  val lookup  : 'k -> ('k,'v) t -> 'v
end

module BstMap : Map = struct
  type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a tree

  type ('k, 'v) t = ('k * 'v) tree

  let empty =
    Leaf

  let rec insert k v = function
    | Leaf -> Node((k, v), Leaf, Leaf)
    | Node ((k',v'), l, r) ->
      if (k = k') then Node ((k, v), l, r)
      else if (k < k') then Node ((k',v'), insert k v l, r)
      else Node ((k',v'), l, insert k v r)

  let rec lookup k = function
    | Leaf -> failwith "Not_found"
    | Node ((k',v'), l, r) ->
      if (k = k') then v'
      else if (k < k') then lookup k l
      else lookup k r
end

(********************************************************************
 * exercise: fraction
 ********************************************************************)

module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  (* [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

module PairFraction = struct
  type t = int * int
  let make n d =
    assert (d != 0);
    (n,d)
  let numerator (n,d) = n
  let denominator (n,d) = d
  let to_string (n,d) =
    string_of_int n ^ " / " ^ string_of_int d
  let to_float (n,d) =
    float_of_int n /. float_of_int d
  let add (n1,d1) (n2,d2) =
    let d' = d1 * d2 in
    (n1 * d2 + n2 * d1, d')
  let mul (n1,d1) (n2,d2) =
    (n1 * n2, d1 * d2)
end

(********************************************************************
 * exercise: make char map
 ********************************************************************)

(*
 * In the CharMap module, [key] is the type of keys, which
 *   is synonymous with [char], ['a] is the type of values
 *   and [t] is the abstract type of the character map.
 * [empty] is an empty character map. Its type is ['a t] because it is
 *   a map whose value type is not yet known.
 * [add] takes a key of type [key], a value of type ['a], an existing map
 *   of type ['a t], and returns a new map with a binding from that
 *   key to that value.
 * [remove] takes a key of type [key] and an existing map of type ['a t],
 *   and returns a new map without a binding for [key].
 *)

(********************************************************************
 * exercise: char ordered
 ********************************************************************)

(*
 * [Char] matches the [Map.OrderedType] signature because it contains a
 *   type named [t] as well as an ordering function [compare : t -> t -> int].
 *)


(********************************************************************
 * exercise: use char map
 ********************************************************************)
module CharMap = Map.Make(Char)
let map = CharMap.(
    empty
    |> add 'A' "Alpha"
    |> add 'E' "Echo"
    |> add 'S' "Sierra"
    |> add 'V' "Victor"
  )
let echo = CharMap.find 'E' map (* "Echo" *)
let map' = CharMap.remove 'A' map
let a_exists = CharMap.mem 'A' map' (* false *)
let bindings = CharMap.bindings map' (* [('E', "Echo"); ('S', "Sierra");
                                         ('V', "Victor")] *)

(********************************************************************
 * exercise:  bindings
 ********************************************************************)
(*
 * All three expressions will return the same association list since
 * the association list is sorted based on the keys, not based on
 * insertion order.
 *)


(********************************************************************
 * exercise:  date order
 ********************************************************************)

type date = {
  month : int;
  day : int
}

module Date = struct
  type t = date

  let compare d1 d2 =
    if d1.month = d2.month then d1.day - d2.day
    else d1.month - d2.month
end


(********************************************************************
 * exercise: calendar
 ********************************************************************)

module DateMap = Map.Make(Date)

type calendar = string DateMap.t

let my_calendar =
  DateMap.(empty |>
           add { month = 2; day = 7 } "e day" |>
           add { month = 3; day = 14 } "pi day" |>
           add { month = 6; day = 18 } "phi day" (* according to some *) |>
           add { month = 10; day = 23 } "mole day" |>
           add { month = 11; day = 23 } "fibonacci day"
          )

(********************************************************************
 * exercise: print calendar
 ********************************************************************)

(* effects: prints each entry in a [calendar]; one entry per line *)
let print_calendar cal =
  DateMap.iter
    (fun date event -> Printf.printf "%d/%d: %s\n" date.month date.day event)
    cal

(********************************************************************
 * exercise: is for
 ********************************************************************)

(* returns: a [CharMap] [m'] that has the same keys as [m], but where each
 * value [v] is replaced by [key ^ " is for " ^ v].
*)
let is_for m =
  CharMap.mapi (fun key v -> Printf.sprintf "%c is for %s" key v) m

(********************************************************************
 * exercise: first after
 ********************************************************************)

let thd (_,_,x) = x

(* returns: the first event in [cal] that comes strictly after [date].
 * raises: [Not_found] is there is no such event
*)
let first_after date cal =
  DateMap.(split date cal |> thd |> min_binding |> snd)

(* another solution *)
let first_after' date cal = 
  DateMap.find_first (fun k -> (Date.compare k date) > 0) cal |> snd


(********************************************************************
 * exercise: sets
 ********************************************************************)

module CisSet = Set.Make(struct
    type t = string
    let compare s1 s2 =
      String.compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)
  end)

let _ = CisSet.(equal (of_list ["grr"; "argh"]) (of_list ["GRR"; "aRgh"]))


(********************************************************************
 * exercise: ToString
 ********************************************************************)

module type ToString = sig
  type t
  val to_string: t -> string
end


(********************************************************************
 * exercise: Print
 ********************************************************************)

module Print (M : ToString) = struct
  (* effects: print a string representation of [M.t] *)
  let print v = print_string (M.to_string v)
end


(********************************************************************
 * exercise: Print Int
 ********************************************************************)
module Int = struct
  type t = int
  let to_string = string_of_int
end

module PrintInt = Print(Int)
let _ = PrintInt.print 5

(********************************************************************
 * exercise: Print String
 ********************************************************************)

module MyString = struct
  type t = string
  let to_string s = s
end

module PrintString = Print(MyString)
let _ = PrintString.print "Harambe"


(********************************************************************
 * exercise: Print reuse
 ********************************************************************)

(* Functor [Print] wraps the details of printing for us, so each module
 * [M] only has to specify how to turn [M.t] into a string. Specifically,
 * the application of `print_string` has been factored out. That is
 * admittedly a tiny piece of code to factor out!  But if printing
 * required a lot more code to implement, we'd have felt good about this.
*)


(********************************************************************
 * exercise: Print String reuse revisited
 ********************************************************************)
module StringWithPrint = struct
  include String
  include Print(MyString)
end

(********************************************************************
 * exercise: printer for date
 ********************************************************************)

(* put this definition in date.ml:
     let format fmt d = Format.fprintf fmt "%s" (to_string d)
   now instead of printing <abstr> as the response to [make_date],
   utop will print the string representation of the date.
*)

(********************************************************************
 * exercise: refactor arith
 ********************************************************************)

module type PreRing = sig
  type t
  val zero  : t
  val one   : t
  val (+)   : t -> t -> t
  val (~-)  : t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
end

module type OfInt = sig
  type t
  val of_int : int -> t
end

module type Ring = sig
  include PreRing
  include OfInt with type t := t
end

module type PreField = sig
  include PreRing
  val (/) : t -> t -> t
end

module type Field = sig
  include PreField
  include OfInt with type t := t
end

module RingOfPreRing (R:PreRing) = (struct
  include R
  let of_int n =
    let two = one + one in
    (* [loop n b x] is [nb + x] *)
    let rec loop n b x =
      if n=0 then x
      else loop Stdlib.(n/2) (b*two)
          (if n mod 2 = 0 then x else x+b)
    in
    let m = loop (abs n) one zero in
    if n<0 then -m else m
end : Ring with type t = R.t)

module FieldOfPreField (F:PreField) = (struct
  module R : (OfInt with type t := F.t) = RingOfPreRing(F)
  include F
  include R
end : Field)

module IntPreRing = struct
  type t = int
  let zero = 0
  let one = 1
  let (+) = (+)
  let (~-) = (~-)
  let ( * ) = ( * )
  let to_string = string_of_int
end

module IntRing : Ring = RingOfPreRing(IntPreRing)

module IntPreField = struct
  include IntPreRing
  let (/) = (/)
end

module IntField : Field = FieldOfPreField(IntPreField)

module FloatPreRing = struct
  type t = float
  let zero = 0.
  let one = 1.
  let (+) = (+.)
  let (~-) = (~-.)
  let ( * ) = ( *. )
  let to_string = string_of_float
end

module FloatRing : Ring = RingOfPreRing(FloatPreRing)

module FloatPreField = struct
  include FloatPreRing
  let (/) = (/.)
end

module FloatField : Field = FieldOfPreField(FloatPreField)

module Fraction (R:Ring) = struct
  type t = R.t * R.t
  let zero = (R.zero, R.one)
  let one = (R.one, R.one)
  let (+) (a,b) (c,d) = R.(a*d + c*b, b*d)
  let (~-) (a,b) = R.(-a,b)
  let ( * ) (a,b) (c,d) = R.(a*c, b*d)
  let (/) (a,b) (c,d) = (a,b) * (d,c)
  let to_string (a,b) = R.((to_string a) ^ "/" ^ (to_string b))
end

module IntRational : Field = FieldOfPreField(Fraction(IntField))

module FloatRational : Field = FieldOfPreField(Fraction(FloatField))
