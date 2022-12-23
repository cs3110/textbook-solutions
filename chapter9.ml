(********************************************************************
 * exercise: parse
 ********************************************************************)

(*
# parse "22";;
- : expr = Int 22

# parse "1+2+3";;
- : expr = Binop (Add, Binop (Add, Int 1, Int 2), Int 3)

# parse "let x = 2 in 20+x";;
- : expr = Let ("x", Int 2, Binop (Add, Int 20, Var "x"))

# parse "3.14";;
Exception: Failure "lexing: empty token".
  --> This is a lexing error.  The "." token is meaningless to the lexer,
      because there is no regular expression that matches it.

# parse "3+";;
Exception: Parser.MenhirBasics.Error.
  --> This is a parsing error.  There is an expression missing on the
      right-hand side of the "+" operator.

Obviously both of the error messages are terrible.  A good interpreter
would need to do some additional engineering to produce good error
messages for programmers.
*)

(********************************************************************
 * exercise: simpl ids
 ********************************************************************)

(*
- OCaml identifiers may contain underscores and single quotes and digits; SimPL may not.
- SimPL identifiers may begin with uppercase letters.  Some OCaml identifiers
  (e.g., module names and constructor names) may not.
*)

(********************************************************************
 * exercise: times parsing
 ********************************************************************)

(*

with %left:

# parse "1*2*3";;
- : expr = Binop (Mult, Binop (Mult, Int 1, Int 2), Int 3)

with %right:

# parse "1*2*3";;
- : expr = Binop (Mult, Int 1, Binop (Mult, Int 2, Int 3))

with PLUS above TIMES:

# parse "1+2*3";;
- : expr = Binop (Add, Int 1, Binop (Mult, Int 2, Int 3))

with TIMES above PLUS:

# parse "1+2*3";;
- : expr = Binop (Mult, Binop (Add, Int 1, Int 2), Int 3)

*)

(********************************************************************
 * exercise: infer
 ********************************************************************)

(*
 Enter in utop:

 # let infer s = s |> parse |> typeof Context.empty;;
 val infer : string -> typ = <fun>

 # infer "3110";;
 - : typ = TInt

 # infer "1 <= 2";;
 - : typ = TBool

 # infer "let x = 2 in 20 + x";;
 - : typ = TInt
*)

(********************************************************************
 * exercise: subexpression types
 ********************************************************************)

(*
Even if an expression is well typed in `ctx`, some of its subexpressions
might not be.  For example, `let x = 42 in x` is well typed in the
empty context, but `x` (a subexpression) is not.

But, if an expression is well typed in `ctx`, then all of its
subexpressions are also well typed in *some* context, which might
depend on the subexpression.

A careful, formal proof of that requires induction.  A more informal
argument goes as follows:  If [ctx |- e : t], then depending on
the syntax of [e], exactly one of the typing rules applies.  Every
SimPL typing rule recurses on all of the subexpressions [e']
of [e], and determines a type [t'] and context [ctx'] such that
[ctx' |- e' : t'].  So just keep following the typing rules until
you get to the subexpression [e'] in which you are interested, and
you'll find the context that makes that subexpression well typed.
*)

(********************************************************************
 * exercise: typing
 ********************************************************************)

(*
We just have to apply the typing rules, instantiating them appropriately:

{} |- let x = 0 in if x <= 1 then 22 else 42 : int
  because {} |- 0 : int
  and {x:int} |- if x <= 1 then 22 else 42 : int
    because {x:int} |- x <= 1 : bool
      because {x:int} |- x : int
      and {x:int} |- 1 : int
    and {x:int} |- 22 : int
    and {x:int} |- 42 : int
*)

(********************************************************************
 * exercise: substitution
 ********************************************************************)

(*

(x+1){2/x} = 2+1
(x+y){2/x}{3/y} = 2+3
(x+y){1/z} = x+y
(let x=1 in x+1){2/x} = let x=1 in x+1
(x + (let x=1 in x+1)){2/x} = 2 + (let x=1 in x+1)
((let x=1 in x+1) + x){2/x} = (let x=1 in x+1) + 2
(let x=y in x+1){2/y} = let x=2 in x+1
(let x=x in x+1){2/x} = let x=2 in x+1

*)

(********************************************************************
 * exercise: step expressions
 ********************************************************************)

(*
    (3 + 5) * 2
--> (step + operation)
    8 * 2
--> (step * operation)
    16


    if 2 + 3 <= 4 then 1 + 1 else 2 + 2
--> (step + operation)
    if 5 <= 4 then 1 + 1 else 2 + 2
--> (step <= operation)
    if false then 1 + 1 else 2 + 2
--> (if: false)
    2 + 2
--> (step + operation)
    4
*)

(********************************************************************
 * exercise: step let expressions
 ********************************************************************)

(*
    let x = 2 + 2 in x + x
--> (step + operation)
    let x = 4 in x + x
--> (step let expression)
    (x + x){4/x} = 4 + 4
--> (step + operation)
    8


    let x = 5 in ((let x = 6 in x) + x)
--> (step let expression)
    ((let x = 6 in x) + x){5/x}
  = (let x = 6 in x){5/x} + x{5/x}
  = (let x = 6{5/x} in x) + 5
  = (let x = 6 in x) + 5
--> (step let expression)
    x{6/x} + 5
  = 6 + 5
--> (step + operation)
    11


    let x = 1 in (let x = x + x in x + x)
--> (step let expression)
    (let x = x + x in x + x){1/x}
  = let x = (x + x){1/x} in x + x
  = let x = 1 + 1 in x + x
--> (step + operation)
    let x = 2 in x + x
--> (step let expression)
    (x + x){2/x}
  = 2 + 2
--> (step + operation)
    4
*)

(********************************************************************
 * exercise: variants
 ********************************************************************)

(*
    Left (1 + 2)
--> (step + operation)
    Left 3


    match Left 42 with
      Left x -> x + 1
    | Right y -> y - 1
--> (match: Left)
    (x + 1){42/x}
  = 42 + 1
--> (step + operation)
    43

*)

(********************************************************************
 * exercise: application
 ********************************************************************)

(*
    (fun x -> 3 + x) 2
--> (app)
    (3 + x){2/x}
  = 3 + 2
--> (step + operation)
    5


    let f = (fun x -> x + x) in (f 3) + (f 3)
--> (step let expression)
    ((f 3) + (f 3)){(fun x -> x + x)/f}
  = (f 3){(fun x -> x + x)/f} + (f 3){(fun x -> x + x)/f}
  = (f{(fun x -> x + x)/f} 3{(fun x -> x + x)/f}) +
    (f{(fun x -> x + x)/f} 3{(fun x -> x + x)/f})
  = (fun x -> x + x) 3 + (fun x -> x + x) 3
--> (app)
    (x + x){3/x} + (fun x -> x + x) 3
  = (x{3/x} + x{3/x}) + (fun x -> x + x) 3
  = (3 + 3) + (fun x -> x + x) 3
--> (step + operation)
    6 + (fun x -> x + x) 3
--> (app)
    6 + (x + x){3/x}
  = 6 + (x{3/x} + x{3/x})
  = 6 + (3 + 3)
--> (step + operation)
    6 + 6
--> (step + operation)
    12


    let f = fun x -> x + x in
    let x = 1 in
    let g = fun y -> x + f y in
    g 3
--> (step let expression)
    (let x = 1 in
    let g = fun y -> x + f y in
    g 3){(fun x -> x + x)/f}
  = let x = 1 in
    let g = fun y -> x + (fun x -> x + x) y in
    g 3
--> (step let expression)
    (let g = fun y -> x + (fun x -> x + x) y in g 3){1/x}
  = let g = fun y -> 1 + (fun x -> x + x) y in g 3
--> (step let expression)
    (g 3){(fun y -> 1 + (fun x -> x + x) y)/g}
  = (fun y -> 1 + (fun x -> x + x) y) 3
--> (app)
    (1 + (fun x -> x + x) y){3/y}
  = 1 + (fun x -> x + x) 3
--> (app)
    1 + (x + x){3/x}
  = 1 + (3 + 3)
--> (step + operation)
    1 + 6
--> (step + operation)
    7


    let f = (fun x -> fun y -> x + y) in
    let g = f 3 in
    (g 1) + (f 2 3)
--> (step let expression)
    (let g = f 3 in (g 1) + (f 2 3)){{fun x -> fun y -> x + y)/f}
  = let g = (fun x -> fun y -> x + y) 3 in
    g 1 + (fun x -> fun y -> x + y) 2 3
--> (app)
    let g = (fun y -> x + y){3/x} in
    g 1 + (fun x -> fun y -> x + y) 2 3
  = let g = fun y -> 3 + y in
    g 1 + (fun x -> fun y -> x + y) 2 3
--> (step let expression)
    (g 1 + (fun x -> fun y -> x + y) 2 3){(fun y -> 3 + y)/g}
  = (fun y -> 3 + y) 1 + (fun x -> fun y -> x + y) 2 3
--> (app)
    (3 + y){1/y} + (fun x -> fun y -> x + y) 2 3
  = (3 + 1) + (fun x -> fun y -> x + y) 2 3
--> (step + operator)
    4 + (fun x -> fun y -> x + y) 2 3
--> (app)
    4 + ((fun y -> x + y){2/x}) 3
  = 4 + (fun y -> 2 + y) 3
--> (app)
    4 + (2 + y){3/y}
  = 4 + (2 + 3)
--> (step + operator)
    4 + 5
--> (step + operator)
    9

*)

(********************************************************************
 * exercise: omega
 ********************************************************************)

(*
  It doesn't terminate.  It just keeps returning over and over
  again to the original expression.
*)

(********************************************************************
 * exercise: pair parsing
 ********************************************************************)

(*

In ast.ml:

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Pair of expr * expr

In lexer.mll, add to the read rule:

  | "," { COMMA }

In parser.mly, add a declaration:

  %token COMMA

And add a production to the expr rule:

  | LPAREN; e1=expr; COMMA; e2=expr; RPAREN { Pair (e1, e2) }

*)

(********************************************************************
 * exercise: pair type checking
 ********************************************************************)

(*
  The new typing rule is:

  ctx |- (e1, e2) : t1 * t2
    if ctx |- e1 : t1
    and ctx |- e2 : t2

  Add this to [typ]:
    | TPair of typ * typ

  Add this to [typeof]:
    | Pair (e1, e2) -> typeof_pair ctx e1 e2
    ...
    and typeof_pair ctx e1 e2 =
      let t1, t2 = typeof ctx e1, typeof ctx e2 in
      TPair (t1, t2)
*)

(********************************************************************
 * exercise: pair evaluation
 ********************************************************************)

(*
  Make [is_value] recursive and add this line:
    | Pair (e1, e2) -> is_value e1 && is_value e2

  Add to [subst]:
    | Pair (e1, e2) -> Pair (subst e1 v x, subst e2 v x)

  Add to [step]:
    | Pair (e1, e2) when is_value e1 ->
      Pair (e1, step e2)
    | Pair (e1, e2) -> Pair (step e1, e2)

  Add to [eval_big]:
    | Pair (e1, e2) -> Pair (eval_big e1, eval_big e2)
*)


(********************************************************************
 * exercise: desugar list
 ********************************************************************)

(*

[1;2;3] desugars to Right(1, Right(2, Right(3, Left 0)))

*)

(********************************************************************
 * exercise: list not empty
 ********************************************************************)

(*

let non_empty =
  fun l -> match l with
      Left x -> false
    | Right y -> true


    non_empty []
  = non_empty (Left 0)
--> (match: Left)
    false{0/x}
  = false


    non_empty [1]
  = non_empty (Right (1, (Left 0)))
--> (match: Right)
    true{(1, (Left 0))/y}
  = true

*)

(********************************************************************
 * exercise: pattern matching
 ********************************************************************)

(*

match e with | p1 -> e1 | p2 -> e2 | ... | pn -> en
--> match e' with | p1 -> e1 | p2 -> e2 | ... | pn -> en
  if e --> e'

match v with | p1 -> e1 | p2 -> e2 | ... | pn -> en
--> match v with | p2 -> e2 | ... | pn -> en
  if there does not exist an s such that v =~ p1 // s

match v with | p1 -> e1 | p2 -> e2 | ... | pn -> en
--> e1 s
  if v =~ p1 // s


    match (1 + 2, 3) with
    | (1, 0) -> 4
    | (1, x) -> x
    | (x, y) -> x + y
--> (step + operation)
    match (3, 3) with
    | (1, 0) -> 4
    | (1, x) -> x
    | (x, y) -> x + y
--> (doesn't match)
    match (3, 3) with
    | (1, x) -> x
    | (x, y) -> x + y
--> (doesn't match)
    match (3, 3) with
    | (x, y) -> x + y
--> (match)
    (x + y){3/x}{3/y}
  = 3 + 3
--> (step + operation)
    6

*)

(********************************************************************
 * exercise: let rec
 ********************************************************************)

(*

We write F for (rec fact -> fun x -> if x <= 1 then 1 else x * (fact (x-1)))

  let rec fact = fun x ->
      if x <= 1 then 1 else x * (fact (x - 1)) in
    fact 3
  = (desugaring)
    let fact = rec fact -> fun x ->
      if x <= 1 then 1 else x * (fact (x - 1)) in
    fact 3
--> (step rec)
    let fact = (fun x ->
      if x <= 1 then 1 else x * (fact (x - 1))){F/fact} in
    fact 3
  = let fact = fun x ->
      if x <= 1 then 1 else x * (F (x - 1)) in
    fact 3
--> (step let)
  = (fact 3){fun x -> if x <= 1 then 1 else x * (F (x - 1))/fact}
  = (fun x -> if x <= 1 then 1 else x * (F (x - 1))) 3
--> (app)
    (if x <= 1 then 1 else x * (F (x - 1))){3/x}
  = if 3 <= 1 then 1 else 3 * (F (3 - 1))
--> (step <= operation)
    if false then 1 else 3 * (F (3 - 1))
--> (if false)
    3 * (F (3 - 1))
--> (step rec)
    3 * (((fun x -> if x <= 1 then 1 else x * (fact (x - 1))){F/fact}) (3 - 1))
  = 3 * ((fun x -> if x <= 1 then 1 else x * (F (x - 1))) (3 - 1))
--> (step - operation)
    3 * ((fun x -> if x <= 1 then 1 else x * (F (x - 1))) 2)
--> (app)
    3 * ((if x <= 1 then 1 else x * (F (x - 1))){2/x})
  = 3 * (if 2 <= 1 then 1 else 2 * (F (2 - 1)))
--> (step <= operation)
    3 * (if false then 1 else 2 * (F (2 - 1)))
--> (if false)
    3 * (2 * F (2 - 1))
--> (step rec)
    3 * (2 * ((fun x -> if x <= 1 then 1 else x * (fact (x - 1))){F/fact}) (2 - 1))
  = 3 * (2 * (fun x -> if x <= 1 then 1 else x * (F (x - 1))) (2 - 1))
--> (step - operation)
    3 * (2 * (fun x -> if x <= 1 then 1 else x * (F (x - 1))) 1)
--> (app)
    3 * (2 * (if x <= 1 then 1 else x * (F (x - 1))){1/x})
  = 3 * (2 * (if 1 <= 1 then 1 else 1 * (F (1 - 1))))
--> (step <= operation)
    3 * (2 * (if true then 1 else 1 * (F (1 - 1))))
--> (if true)
    3 * (2 * 1)
--> (step * operation)
    3 * 2
--> (step * operation)
    6

*)

(*
*******************************************************************
 exercise: simple expressions
*******************************************************************


 <[], 110 + 3*1000> ==> 3110                                 (op: plus)
   because <[], 110> ==> 110                                 (const)
   and <[], 3*1000> ==> 3000                                 (op: times)
     because <[], 3> ==> 3                                   (const)
     and     <[], 1000> ==> 1000                             (const)
     and 3*1000 is 3000
   and 110 + 3000 is 3110



 <[], if 2 + 3 < 4 then 1 + 1 else 2 + 2> ==> 4              (if: else)
   because <[], 2 + 3 < 4> ==> false                         (op: <)
     because <[], 2 + 3> ==> 5                               (op: plus)
       because <[], 2> ==> 2                                 (const)
       and     <[], 3> ==> 3                                 (const)
       and 2 + 3 is 5
     and <[], 4> ==> 4                                       (const)
     and 5 < 4 is false
   and <[], 2 + 2> ==> 4                                     (op: plus)
     because <[], 2> ==> 2                                   (const)
     and     <[], 2> ==> 2                                   (const)
     and 2 + 2 is 4



*******************************************************************
 exercise: let and match expressions
*******************************************************************


 <[], let x = 0 in 1> ==> 1                                  (let)
   because <[], 0> ==> 0                                     (const)
   and     <[x->0], 1> ==> 1                                 (const)



 <[], let x = 2 in x + 1> ==> 3                              (let)
   because <[], 2> ==> 2                                     (const)
   and     <[x->2], x + 1> ==> 3                             (op: plus)
     because <[x->2], x> ==> 2                               (var)
     and     <[x->2], 1> ==> 1                               (const)
     and 2 + 1 is 3



 <[], match Left 2
      , Left  x -> x + 1
      , Right x -> x - 1> ==> 3                              (match: Left)
   because <[], Left 2> ==> Left 2                           (const)
   and <[x->2], x + 1> ==> 3                                 (op: plus)
     because <[x->2], x> ==> 2                               (var)
     and     <[x->2], 1> ==> 1                               (const)
     and 2 + 1 is 3



*******************************************************************
 exercise: closures
*******************************************************************


 <[], (fun x -> x + 1) 2> ==> 3                              (app)
   because <[], fun x -> x + 1> ==> (|fun x -> x + 1 , []|)  (closure)
   and     <[], 2> ==> 2                                     (const)
   and     <[x->2], x + 1> ==> 3                             (op: plus)
     because <[x->2], x> ==> 2                               (var)
     and     <[x->2], 1> ==> 1                               (const)
     and 2 + 1 is 3



 <[], let f = fun x -> x + 1 in f 2> ==> 3                   (let)
   because <[], fun x -> x + 1> ==> (|fun x -> x + 1 , []|)  (closure)
   and     <[f->(|fun x -> x + 1 , []|)], f 2> ==> 3         (app)
     because <[f->(|fun x -> x + 1 , []|)], f>
              ==> (|fun x -> x + 1 , []|)                    (var)
     and     <[f->(|fun x -> x + 1 , []|)], 2> ==> 2         (const)
     and     <[x->2], x + 1> ==> 3                           (op: plus)
       because <[x->2], x> ==> 2                             (var)
       and     <[x->2], 1> ==> 1                             (const)
       and 2 + 1 is 3



*******************************************************************
 exercise: lexical scope and shadowing
*******************************************************************


 <[], let x = 0 in x + (let x = 1 in x)> ==> 1               (let)
   because <[], 0> ==> 0                                     (const)
   and     <[x->0], x + (let x = 1 in x)> ==> 1              (op: plus)
     because <[x->0], x> ==> 0                               (var)
     and     <[x->0], let x = 1 in x> ==> 1                  (let)
       because <[x->0], 1> ==> 1                             (const)
       and     <[x->1], x> ==> 1                             (var)
     and 0 + 1 is 1



 <[], let x = 1 in
      let f = fun y -> x in
      let x = 2 in f 0> ==> 1                                (let)
   because <[], 1> ==> 1                                     (const)
   and     <[x->1], let f = fun y -> x in
                    let x = 2 in f 0> ==> 1                  (let)
     because <[x->1], fun y -> x> ==> (|fun y -> x , [x->1]|)(closure)
     and     <[f->(|fun y -> x , [x->1]|);x->1],
                let x = 2 in f 0> ==> 1                      (let)
       because <[f->(|fun y -> x , [x->1]|);x->1], 2> ==> 2  (const)
       and     <[f->(|fun y -> x , [x->1]|);x->2], f 0> ==> 1(app)
         because <[f->(|fun y -> x , [x->1]|);x->2], f> ==>
                   (|fun y -> x , [x->1]|)                   (var)
         and     <[f->(|fun y -> x , [x->1]|);x->2], 0> ==> 0(const)
         and     <[y->0;x->1], x> ==> 1                      (var)



*******************************************************************
 exercise: dynamic scope
*******************************************************************


 <[], let x = 5 in let f = fun y -> x + y in let x = 4 in f 3> ==> 7
   because <[], 5> ==> 5
   and     <[x->5], let f = fun y -> x + y in let x = 4 in f 3> ==> 7
     because <[x->5], fun y -> x + y> ==> fun y -> x + y
     and     <[f->(fun y -> x + y);x->5], let x = 4 in f 3> ==> 7
       because <[f->(fun y -> x + y);x->5], 4> ==> 4
       and     <[f->(fun y -> x + y);x->4], f 3> ==> 7
         because <[f->(fun y -> x + y);x->4], f> ==> fun y -> x + y
         and     <[f->(fun y -> x + y);x->4], 3> ==> 3
         and     <[y->3;f->(fun y -> x + y);x->4], x + y> ==> 7
           because <[y->3;x->4], x> ==> 4
           and     <[y->3;x->4], y> ==> 3
           and 4 + 3 is 7

 Lexical scope expected answer: 8.



*******************************************************************
 exercise: more dynamic scope
*******************************************************************


 <[], let x = 5 in
      let f = fun y -> x + y in
      let g = fun x -> f x in
      let x = 4 in g 3> ==> 7
   because <[], 5> ==> 5
   and     <[x->5], let f = fun y -> x + y in
                    let g = fun x -> f x in
                    let x = 4 in g 3> ==> 7
     because <[x->5], fun y -> x + y> ==> fun y -> x + y
     and     <[f->(fun y -> x + y);x->5], let g = fun x -> f x in
                                          let x = 4 in g 3> ==> 7
       because <[f->(fun y -> x + y);x->5], fun x -> f x> ==>
         fun x -> f x
       and <[g->(fun x -> f x);f->(fun y -> x + y);x->5],
              let x = 4 in g 3> ==> 7
         because <[g->(fun x -> f x);f->(fun y -> x + y);x->5], 4> ==> 4
         and <[g->(fun x -> f x);f->(fun y -> x + y);x->4], g 3> ==> 7
           because <[g->(fun x -> f x);f->(fun y -> x + y);x->4], g> ==>
             fun x -> f x
           and <[g->(fun x -> f x);f->(fun y -> x + y);x->4], 3> ==> 3
           and <[x->3;f->(fun y -> x + y);x->4], f x> ==> 7
             because <[f->(fun y -> x + y);x->4], f> ==> fun y -> x + y
             and     <[f->(fun y -> x + y);x->4], 3> ==> 3
             and     <[y->3;x->4], x + y> ==> 7
               because <[y->3;x->4], x> ==> 4
               and     <[y->3;x->4], y> ==> 3
               and 4 + 3 is 7

 Lexical scope expected answer: 8.



 <[], let f = fun y -> x + y in let x = 3 in let y = 4 in f 2> ==> 5
   because <[], fun y -> x + y> ==> fun y -> x + y
   and     <[f->(fun y -> x + y)], let x = 3 in let y = 4 in f 2> ==> 5
     because <[f->(fun y -> x + y)], 3> ==> 3
     and     <[x->3;f->(fun y -> x + y)], let y = 4 in f 2> ==> 5
       because <[x->3;f->(fun y -> x + y)], 4> ==> 4
       and     <[y->4;x->3;f->(fun y -> x + y)], f 2> ==> 5
         because <[y->4;x->3;f->(fun y -> x + y)], f> ==> fun y -> x + y
         and     <[y->4;x->3;f->(fun y -> x + y)], 2> ==> 2
         and     <[y->2;x->3;f->(fun y -> x + y)], x + y> ==> 5
           because <[y->2;x->3], x> ==> 3
           and     <[y->2;x->3], y> ==> 2
           and 3 + 2 is 5

 Lexical scope expected answer: Unbound variable `x`.


*******************************************************************
 exercise: constraints
*******************************************************************


No solution available at this time.  Email the professor yours
if you'd like to contribute to the community!



*******************************************************************
 exercise: unify
*******************************************************************


 - Here, I write a composed substitution like this:
    {| sa1/sb1, ..., san/sbn |}
   where sa1/sb2 is the first substitution to perform, and san/sbn is
   the nth.
 - Substition application looks like this:
    SC
   where S is the substitution, and C is the constraints.

 Our result, which we'll call U, is:
  unify ({ X = int, Y = X -> X }) = {| int/X, (int -> int)/Y |}
   because it's equal to U';{| int/X |},
    where U' is:
     unify ({| int/X |}{ Y = X -> X })
     = unify ({ Y = int -> int })
     = {| (int -> int)/Y |}
      because U' equals U'';{| (int -> int)/Y |},
       where U'' is
        unify ({| (int -> int)/Y |}{})
        = {| |} (the empty substitution).


*******************************************************************
 exercise: unify more
*******************************************************************

 (See "unify" exercise for definition of `{| ... |}`.)

 Our result, which we'll call U, is:
  unify ({ X -> Y = Y -> Z, Z = U -> W })
  = {| Y/X, Z/Y, (U -> W)/Z |}
   because it's equal to unify ({ X = Y, Y = Z } ∪ { Z = U -> W })
    which is equal to U';{| Y/X |}, where U' is
     unify({| Y/X |}{ Y = Z, Z = U -> W })
     = unify({ Y = Z, Z = U -> W })
      which is equal to to U'';{| Z/Y |}, where U'' is
       unify({| Z/Y |}{ Z = U -> W })
       = unify({ Z = U -> W })
        which is equal to U''';{| (U -> W)/Z |}, where U''' is
         unify({| (U -> W)/Z |}{ Z = U -> W})
         = unify({ U -> W = U -> W })
          which is equal to unify({ U = U, W = W } ∪ {})
          = {| |} (last few steps omitted for brevity; trivial cases)

*******************************************************************
 exercise: infer apply
*******************************************************************

let apply f x = f x

let apply = fun f -> fun x -> f x

I |- fun f -> fun x -> f x : ‘a -> ‘b -> ‘c -| ‘a = ‘b -> ‘c
	I, f: ‘a |- fun x -> f x : ‘b -> ‘c -| ‘a = ‘b -> ‘c
		I, f: ‘a, x: ‘b |- f x : ‘c -| ‘a = ‘b -> ‘c
			I, f: ‘a, x: ‘b |- f : ‘a -| {}
			I, f: ‘a, x: ‘b |- x : ‘b -| {}

unify(‘a = ‘b -> ‘c) 
=
{‘b -> ‘c / ‘a} unify()
= 
{‘b -> ‘c / ‘a}

‘a -> ‘b -> ‘c becomes (‘b -> ‘c) -> ‘b -> ‘c
				   
Inferred Type: (‘a -> ‘b) -> ‘a -> ‘b

*******************************************************************
 exercise: infer double
*******************************************************************

let double f x = f (f x)

let double = fun f -> fun x -> f (f x)

I |- fun f -> fun x -> f (f x) : ‘a -> ‘b -> ‘d -| ‘a = ‘c -> ‘d, ‘a = ‘b -> ‘c
	I, f: ‘a |- fun x -> f (f x) : ‘b -> ‘d -| ‘a = ‘c -> ‘d, ‘a = ‘b -> ‘c
		I, f: ‘a, x: ‘b |- f (f x) : ‘d -| ‘a = ‘c -> ‘d, ‘a = ‘b -> ‘c
			I, f: ‘a, x: ‘b |- f : ‘a -| {}
			I, f: ‘a, x: ‘b |- f x : ‘c -| ‘a = ‘b -> ‘c
				I, f: ‘a, x: ‘b |- f : ‘a -| {}
				I, f: ‘a, x: ‘b |- x : ‘b -| {}

unify(a = ‘c -> ‘d, ‘a = ‘b -> ‘c)
= 
{‘c -> ‘d / ‘a}; unify(‘a = ‘b -> ‘c) {‘c -> ‘d / ‘a}
= 
{‘c -> ‘d / ‘a}; unify(‘c -> ‘d = ‘b -> ‘c)
= 
{‘c -> ‘d / ‘a}; unify(‘c = ‘b, ‘d = ‘c)
=
{‘c -> ‘d / ‘a}; {‘b / ‘c}; unify(‘d = ‘c) {‘b / ‘c}
= 
{‘c -> ‘d / ‘a}; {‘b / ‘c}; unify(‘d = ‘b)
= 
{‘c -> ‘d / ‘a}; {‘b / ‘c}; {‘b / ‘d}

‘a -> ‘b -> ‘d becomes (‘c -> ‘d) -> ‘b -> ‘d
			becomes (‘b -> ‘d) -> ‘b -> ‘d
			becomes (‘b -> ‘b) -> ‘b -> ‘b

Inferred Type: (‘a -> ‘a) -> ‘a -> ‘a

*******************************************************************
 exercise: infer S
*******************************************************************

let s x y z = (x z) (y z)

let s = fun x -> fun y -> fun z -> (x z) (y z)

I |- fun x -> fun y -> fun z -> (x z) (y z) : ‘a -> ‘b -> ‘c -> ‘f -| ‘e = ‘d -> ‘f, ‘a = ‘c -> ‘e, ‘b = ‘c -> ‘d
	I, x: ‘a |- fun y -> fun z -> (x z) (y z) : ‘b -> ‘c -> ‘f -| ‘e = ‘d -> ‘f, ‘a = ‘c -> ‘e, ‘b = ‘c -> ‘d
		I, x: ‘a, y: ‘b |- fun z -> (x z) (y z) : ‘c -> ‘f -| ‘e = ‘d -> ‘f, ‘a = ‘c -> ‘e, ‘b = ‘c -> ‘d
			I, x: ‘a, y: ‘b, z: ‘c |- (x z) (y z) : ‘f -| ‘e = ‘d -> ‘f, ‘a = ‘c -> ‘e, ‘b = ‘c -> ‘d
				I, x: ‘a, y: ‘b, z: ‘c |- (x z) : ‘e -| ‘a = ‘c -> ‘e
					I, x: ‘a, y: ‘b, z: ‘c |- x : ‘a -| {}
					I, x: ‘a, y: ‘b, z: ‘c |- z : ‘c -| {}
				I, x: ‘a, y: ‘b, z: ‘c |- (y z) : ‘d -| ‘b = ‘c -> ‘d
					I, x: ‘a, y: ‘b, z: ‘c |- y : ‘b -| {}
					I, x: ‘a, y: ‘b, z: ‘c |- z : ‘c -| {}

unify(‘e = ‘d -> ‘f, ‘a = ‘c -> ‘e, ‘b = ‘c -> ‘d)
= 
{‘d -> ‘f / ‘e}; unify(‘a = ‘c -> ‘e, ‘b = ‘c -> ‘d){‘d -> ‘f / ‘e}
=
{‘d -> ‘f / ‘e}; unify(‘a = ‘c -> (‘d -> ‘f), ‘b = ‘c -> ‘d)
= 
{‘d -> ‘f / ‘e}; {‘c -> (‘d -> ‘f) / ‘a}; unify(‘b = ‘c -> ‘d){‘c -> (‘d -> ‘f) / ‘a}
=
{‘d -> ‘f / ‘e}; {‘c -> (‘d -> ‘f) / ‘a}; {‘c -> ‘d / ‘b}


(‘a -> ‘b -> ‘c -> ‘f) becomes (‘c -> (‘d -> ‘f)) -> ‘b -> ‘c -> ‘f
				   becomes (‘c -> ‘d -> ‘f) -> (‘c -> ‘d) -> ‘c -> ‘f

Inferred Type: (‘a -> ‘b -> ‘c) -> (‘a -> ‘b) -> ‘a -> ‘c



*)
