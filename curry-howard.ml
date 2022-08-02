(********************************************************************
 * exercise: propositions as types
 ********************************************************************)

(*
- `true -> p` corresponds to `unit -> 'p`
- `p /\ (q /\ r)` corresponds to `'p * ('q *  'r)`
- `(p \/ q) \/ r` corresponds to `(('p, 'q) disj, 'r) disj`
- `false -> p` corresponds to `empty -> 'p`
*)

(********************************************************************
 * exercise: programs as proofs
 ********************************************************************)

(*
(p /\ q) -> (q /\ p)
type: ('p * 'q) -> ('q * 'p)
definition:
let e1 (p, q) = (q, p)

(p \/ q) -> (q \/ p)
type: ('p, 'q) disj -> ('q * 'p) disj
definition
let e2 = function
  | Left p -> Right p
  | Right q -> Left q
*)

(********************************************************************
 * exercise: evaluation as simplification
 ********************************************************************)

(*
```
let f x = snd ((fun x -> x,x) (fst x))
```

- What is the type of that program? `'a * 'b -> 'a`
- What is the proposition corresponding to that type? `A /\ B -> A`
- How would `f (1,2)` evaluate in the small-step semantics?
  ```
  f (1,2)
  --> snd ((fun x -> x,x) (fst (1,2)))
  --> snd ((fun x -> x,x) 1)
  --> snd (1,1)
  --> 1
  ```

- What simplified implementation of `f` does that evaluation suggest?
  `let f = fst`

- Does your simplified `f` still have the same type as the original?
  `Yes`
*)
