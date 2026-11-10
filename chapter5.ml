(********************************************************************
 * exercise: stack option
 ********************************************************************)

 module Stack = struct
  type 'a t = 'a list

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false

  let push item stack = item :: stack

  let peek = function
    | [] -> None
    | item :: _ -> Some item

  let pop = function
    | [] -> None
    | item :: rest -> Some rest
end

(********************************************************************
 * exercise: queue option
 ********************************************************************)

 module Queue = struct
  type 'a t = 'a list

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false

  let enqueue item queue = queue @ [item]

  let peek = function
    | [] -> None
    | item :: _ -> Some item

  let dequeue = function
    | [] -> None
    | item :: rest -> Some rest
end

(********************************************************************
 * exercise: browser
 ********************************************************************)

module Browser = struct
  type t = string Stack.t * string * string Stack.t

  let start url = (Stack.empty, url, Stack.empty)

  let current (_, page, _) = page

  let visit url (back, page, _) =
    (Stack.push page back, url, Stack.empty)

  let go_back (back, page, forward) =
    match Stack.peek back with
    | None -> None
    | Some previous ->
      Some (Stack.pop back, previous, Stack.push page forward)

  let go_forward (back, page, forward) =
    match Stack.peek forward with
    | None -> None
    | Some next ->
      Some (Stack.push page back, next, Stack.pop forward)
end

(********************************************************************
 * exercise: helpdesk
 ********************************************************************)

module HelpDesk = struct
  type t = string Queue.t * string Stack.t

  let empty = (Queue.empty, Stack.empty)

  let arrive name (waiting, helped) =
    (Queue.enqueue name waiting, helped)

  let serve (waiting, helped) =
    match Queue.peek waiting with
    | None -> None
    | Some name ->
        Some (name, (Queue.dequeue waiting, Stack.push name helped))

  let completed (_, helped) =
    let rec collect stack acc =
      match Stack.peek stack with
      | None -> acc
      | Some name -> collect (stack |> Stack.pop |> Option.get) (name :: acc)
    in
    collect helped []
end

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
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val lookup : 'k -> ('k, 'v) t -> 'v
  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module BstMap : Map = struct
  type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a tree

  type ('k, 'v) t = ('k * 'v) tree

  let empty = Leaf

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

  let bindings m =
    let rec aux acc = function
      | Leaf -> acc
      | Node (kv, l, r) -> aux (kv :: aux acc r) l
    in
    aux [] m
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
 * exercise: cached pair list
 ********************************************************************)

module type CachedList = sig
  type 'a t

  val empty : 'a t
  val cons : 'a -> 'a t -> 'a t
  val tail : 'a t -> 'a t option
  val append : 'a t -> 'a t -> 'a t
  val length : 'a t -> int
  val to_list : 'a t -> 'a list
end

module PairCachedList : CachedList = struct
  type 'a t = int * 'a list
  (* AF: (n, xs) represents the list xs. *)
  (* RI: n = List.length xs. *)

  (* The empty list has length zero. *)
  let empty = (0, [])

  (* Adding one element increases both lengths by one. *)
  let cons x (n, xs) = (n + 1, x :: xs)

  (* Removing one element decreases both lengths by one. *)
  let tail = function
    | _, [] -> None
    | n, _ :: xs -> Some (n - 1, xs)

  (* The length of a concatenation is the sum of its input lengths. *)
  let append (n1, xs1) (n2, xs2) = (n1 + n2, xs1 @ xs2)

  let length (n, _) = n
  let to_list (_, xs) = xs
end

(********************************************************************
 * exercise: cached list nodes
 ********************************************************************)

module NodeCachedList : CachedList = struct
  type 'a t = Empty | Node of int * 'a * 'a t
  (* AF: Empty represents []. Node (_, x, xs) represents x :: AF(xs). *)
  (* RI: Empty is valid. Node (n, _, xs) is valid exactly when xs is
     valid and n is one plus the number of nodes in xs. Thus the RI
     must hold at every node, not merely at the first node. *)

  let empty = Empty

  let length = function
    | Empty -> 0
    | Node (n, _, _) -> n

  (* A valid xs has the correct root count, so this new node is valid. *)
  let cons x xs = Node (1 + length xs, x, xs)

  (* The returned suffix already satisfies the recursive RI. *)
  let tail = function
    | Empty -> None
    | Node (_, _, xs) -> Some xs

  (* Rebuild the first input with cons, which calculates each new count
     from the already valid suffix. The second input is shared. *)
  let rec append xs ys =
    match xs with
    | Empty -> ys
    | Node (_, x, rest) -> cons x (append rest ys)

  let rec to_list = function
    | Empty -> []
    | Node (_, x, xs) -> x :: to_list xs

  (* The first count in this invalid representation is correct:
     Node (2, 1, Node (7, 2, Empty))
     There are two elements, but the second count should be 1. *)
end

(* Exercise: cached list tradeoffs

   1. Worst-case time:

      Operation   PairCachedList   NodeCachedList
      empty       O(1)             O(1)
      cons        O(1)             O(1)
      tail        O(1)             O(1)
      append      O(m)             O(m)
      length      O(1)             O(1)
      to_list     O(1)             O(n)

      PairCachedList.to_list can return its underlying immutable OCaml
      list directly. NodeCachedList.to_list constructs an ordinary list.

   2. A single PairCachedList value stores one cached integer. A
      NodeCachedList value of length n stores n cached integers. Both
      representations use O(n) total space for a list of length n.

   3. For a nonempty list, PairCachedList.tail allocates a new pair
      containing the decremented count and the existing list tail.
      NodeCachedList.tail returns its existing tail without allocating
      a new list node. Both operations take O(1) time.

   4. No operation in this interface becomes asymptotically faster
      with a count at every node. In particular, both implementations
      have constant-time length, cons, and tail, and both traverse the
      first list when appending. *)

(********************************************************************
 * exercise: first-class generators
 ********************************************************************)

 module type Generator = sig
  type state
  val initial : state
  val next : state -> int * state
end

module Counter : Generator = struct
  type state = int
  let initial = 0
  let next n = (n, n + 1)
end

module Fibonacci : Generator = struct
  type state = int * int
  let initial = (0, 1)
  let next (a, b) = (a, (b, a + b))
end

let take generator n =
  let module G = (val generator : Generator) in
  let rec loop n state acc =
    if n = 0 then List.rev acc
    else
      let value, next_state = G.next state in
      loop (n - 1) next_state (value :: acc)
  in
  loop n G.initial []

let generators : (string * (module Generator)) list =
  [
    ("counter", (module Counter : Generator));
    ("fibonacci", (module Fibonacci : Generator));
  ]

let sample name n =
  match List.assoc_opt name generators with
  | None -> None
  | Some generator -> Some (take generator n)

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
