(********************************************************************
 * exercise: mutable fields
 ********************************************************************)

 type student = {name: string; mutable gpa: float}

 let alice = {name = "Alice"; gpa = 3.7}

 let () = alice.gpa <- 4.0


 (********************************************************************
  * exercise: refs
  ********************************************************************)

 (* Exercise: refs *)
 let (_ : bool ref) = ref true
 let (_ : int list ref) = ref [5;3]
 let (_ : int ref list) = [ref 5; ref 3]


 (********************************************************************
  * exercise: inc fun
  ********************************************************************)

 let cs3110 =
   let inc = ref (fun x -> x + 1) in
   !inc 3109


 (********************************************************************
  * exercise: addition assignment
  ********************************************************************)

 let (+:=) x y =
   x := !x + y


 (********************************************************************
  * exercise: physical equality
  ********************************************************************)

 let _ =
   let x = ref 0 in
   let y = x in
   let z = ref 0 in

   assert (x == y);
   assert (not (x == z));
   assert (x = y);
   assert (x = z);
   x := 1;
   assert (x = y);
   assert (not (x = z))


 (********************************************************************
  * exercise: norm
  ********************************************************************)

 (* AF: the float array [| x1; ...; xn |] represents the
  *     vector (x1, ..., xn)
  * RI: the array is non-empty *)
 type vector = float array

 (** [norm v] is the Euclidean norm of [v]. *)
 let norm v =
   sqrt (Array.fold_left (fun acc x -> acc +. x ** 2.) 0. v)

 (* another solution:  same asymptotic complexity but
    less efficient.  Perhaps more readable. *)
 let norm' v =
   v
   |> Array.map (fun x -> x ** 2.)  (* square each element *)
   |> Array.fold_left (+.) 0.       (* sum all elements *)
   |> sqrt

 (********************************************************************
  * exercise: normalize
  ********************************************************************)

 (* effects: [normalize v] modifies [v] to be its normalized form. *)
 let normalize v =
   let n = norm v in (* Must calculate norm before iteration *)
   Array.iteri (fun i x -> v.(i) <- x /. n) v


 (********************************************************************
  * exercise: norm loop
  ********************************************************************)

 (** [norm_loop v] is the Euclidean norm of [v]. *)
 let norm_loop v =
   let n = ref 0.0 in
   for i = 0 to Array.length v - 1 do
     n := !n +. (v.(i) ** 2.)
   done;
   sqrt !n


 (********************************************************************
  * exercise: normalize loop
  ********************************************************************)

 (* effects: [normalize_loop v] modifies [v] to be its normalized form. *)
 let normalize_loop v =
   let n = norm v in
   for i = 0 to Array.length v - 1 do
     v.(i) <- v.(i) /. n
   done


 (********************************************************************
  * exercise: imperative factorial
  ********************************************************************)

 (** [fact_loop n] is the factorial of [n].
  * requires: [n >= 0]
 *)
 let fact_loop n =
   let ans = ref 1 in
   for i = 1 to n do
     ans := !ans * i
   done;
   !ans


 (********************************************************************
  * exercise: init matrix
  ********************************************************************)

 (* [init_matrix n o f] creates and returns an [n] by [o] matrix [m]
  * with [m.(i).(j) = f i j] for all [i] and [j] in bounds.
  * requires: [n, o >= 0]
 *)
 let init_matrix n o f =
   Array.init n (fun i -> Array.init o (fun j -> f i j))

 
