(********************************************************************
 * exercise: values
 ********************************************************************)

(* The first expression has type int, and the second has type string.
   We can demonstrate that by providing a manual *type annotation*
   as shown below. *)

let a:int = 7*(1+2+3)
let b:string = "CS " ^ string_of_int 3110

(* If we don't want to bind an expression to a name, we can instead bind
   it to the pseudo-name _.  That is, underscore.  *)

let _:int = 7*(1+2+3)
let _:string = "CS " ^ string_of_int 3110

(* Of course, in utop we could simply type the expression instead of
   having to bind it to a name.  But we can't write just bare expressions
   inside a .ml file; instead, we have to bind the expressions to
   names using let-definitions. *)

(********************************************************************
 * exercise: operators
 ********************************************************************)

let _ = 42*10
let _ = 3.14 /. 2.
let _ = 4.2 ** 7.

(********************************************************************
 * exercise: equality
 ********************************************************************)

(* These next three might be a little difficult to parse for a human.
   The equality test on the right-hand side binds more tightly than
   the equals sign that is part of the let-definition. *)

let _ = 42 = 42  (* another way to write that would be [let _ = (42 = 42)] *)
let _ = "hi" = "hi"  (* result is true *)
let _ = "hi" == "hi" (* result is false *)

(********************************************************************
 * exercise: assert
 ********************************************************************)

(* here are two solutions to the third part *)

let _ = assert (not (2110 = 3110))
let _ = assert (2110 <> 3110)

(********************************************************************
 * exercise: if
 ********************************************************************)

let _ = if 2 > 1 then 42 else 7

(********************************************************************
 * exercise: double fun
 ********************************************************************)

let double x = 2 * x

let _ = assert (double 7 = 14)
let _ = assert (double 0 = 0)
let _ = assert (double (-1) = -2)

(********************************************************************
 * exercise: more fun
 ********************************************************************)

let cube x =
  x *. x *. x

let _ = assert (cube 0. = 0.)
let _ = assert (cube 1. = 1.)
let _ = assert (cube 3. = 27.)

let sign x =
  if x > 0 then 1
  else if x < 0 then -1
  else 0

let _ = assert (sign 0 = 0)
let _ = assert (sign 10 = 1)
let _ = assert (sign (-10) = -1)

let area r =
  Float.pi *. r ** 2.

(* Comparing floating point numbers for equality is fraught with 
   difficulty.  See this article, for example:
     https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
   For more details, take a scientific computing class.
   Here, we'll use a simple but not-very-good comparison: 
   absolute difference. *)
let close_enough a b =
  Float.abs (a -. b) < 1e-5

let _ = assert (close_enough (area 1.0) Float.pi)
let _ = assert (close_enough (area (Float.sqrt (1. /. Float.pi))) 1.)

(********************************************************************
 * exercise: RMS
 ********************************************************************)

let rms x y = sqrt ((x *. x +. y *. y) /. 2.)
let _ = assert (close_enough (rms 2. 2.) 2.)
let _ = assert (close_enough (rms 7. 42.) 30.10813)

(********************************************************************
 * exercise: date fun
 ********************************************************************)

(* The solution below uses only the language features we've seen so far,
   but because of that it's not as elegant as it could be.  Later we'll
   be able to do better with *pattern matching*. *)

let valid_date d m =
  if m = "Jan" || m = "Mar" || m = "May" || m = "Jul"
     || m = "Aug" || m = "Oct" || m = "Dec"
  then 1 <= d && d <= 31
  else if m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov"
  then 1 <= d && d <= 30
  else if m = "Feb"
  then 1 <= d && d <= 28
  else false

(********************************************************************
 * exercise: fib
 ********************************************************************)

(* returns: element [n] of the Fibonacci sequence *)
(* requires: [n >= 0] *)
let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else fib (n-1) + fib (n-2)

(********************************************************************
 * exercise: fib fast
 ********************************************************************)

(* requires: n > 0 *)
(* returns: element [n] of the Fibonacci-like sequence, assuming
 *   the first two elements are [pp] and [p]. *)
let rec h n pp p =
  if n = 1 then p
  else h (n-1) p (pp+p)

(* returns: element [n] of the Fibonacci sequence
 * requires: [n >= 0] *)
let fib_fast n =
  if n=0 then 0
  else h n 0 1

(* on a 64-bit OCaml implementation, fib_fast 91 overflows. *)

(********************************************************************
 * exercise: poly types
 ********************************************************************)

let f x = if x then x else x
(* bool -> bool *)
(* x must be a bool to to be used as the conditional in the if expression *)

let g x y = if y then x else x
(* 'a -> bool -> 'a *)
(* x could have any type *)

let h x y z = if x then y else z
(* bool -> 'a -> 'a -> 'a *)
(* both branches of the if expression must have the same type,
 *  so y and z must have the same type (which could be anything) *)

let i x y z = if x then y else y
(* bool -> 'a -> 'b -> 'a *)
(* z could have any type, and moreover, that type could be different
 *  than the type of y *)

(********************************************************************
 * exercise: divide
 ********************************************************************)

let divide ~numerator:x ~denominator:y = x /. y

(********************************************************************
 * exercise: associativity 
 ********************************************************************)

let add x y = x + y

(* add 5 1: produces an int
 * add 5: produces a function
 * (add 5) 1: produces an int
 * add (5 1): produces an error; 5 cannot be applied to 1, because
     5 is not a function. *)

(********************************************************************
 * exercise: average 
 ********************************************************************)

let (+/.) a b =
  (a +. b) /. 2.

