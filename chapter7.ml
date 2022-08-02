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
  * exercise: normalize loop
  ********************************************************************)

 (* effects: [normalize_loop v] modifies [v] to be its normalized form. *)
 let normalize_loop v =
   let n = norm v in
   for i = 0 to Array.length v - 1 do
     v.(i) <- v.(i) /. n
   done


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

 (********************************************************************
  * exercise: doubly linked list
  ********************************************************************)

 (* An ['a node] is a node of a mutable doubly-linked list.
  * It contains a value of type ['a] and optionally has
  * pointers to previous and/or next nodes. *)
 type 'a node = {
   mutable prev : 'a node option;
   mutable next : 'a node option;
   value : 'a
 }

 (* An ['a dlist] is a mutable doubly-linked list with elements
  * of type ['a].  It is possible to access the first and
  * last elements in constant time.
  * RI: The list does not contain any cycles. *)
 type 'a dlist = {
   mutable first : 'a node option;
   mutable last : 'a node option;
 }

 (* [create_node v] is a node containing value [v] with
  * no links to other nodes. *)
 let create_node v = {prev=None; next=None; value=v}

 (* [empty_dlist ()] is an empty doubly-linked list. *)
 let empty_dlist () = {first=None; last=None}

 (* [create_dlist n] is a doubly-linked list containing
  * exactly one node, [n]. *)
 let create_dlist (n: 'a node) : 'a dlist = {first=Some n; last=Some n}

 (* [insert_first d n] mutates dlist [d] by
  * inserting node [n] as the first node. *)
 let insert_first (d: 'a dlist) (n: 'a node) : unit =
   let () =
     match d.first with
     | None -> ()
     | Some f -> f.prev <- Some n in
   n.next <- d.first;
   d.first <- Some n

 (* [insert_last d n] mutates dlist [d] by
  * inserting node [n] as the last node. *)
 let insert_last (d: 'a dlist) (n: 'a node) : unit =
   n.prev <- d.last;
   d.last <- Some n

 (* [insert_after d n1 n2] mutates dlist [d] by
  * inserting node [n2] after node [n1]. *)
 let insert_after (d: 'a dlist) (n1: 'a node) (n2: 'a node) : unit =
   let m = ref d.first in
   while !m <> None do
     match !m with
     | None -> raise Not_found
     | Some x -> if x = n1 then
         begin match x.next with
           | None -> x.next <- Some n2; d.last <- Some n2
           | Some y -> y.prev <- Some n2; x.next <- Some n2
         end
       else (); m := x.next
   done

 (* [insert_before d n1 n2] mutates dlist [d] by
  * inserting node [n2] before node [n1]. *)
 let insert_before (d: 'a dlist) (n1: 'a node) (n2: 'a node) : unit =
   let m = ref d.first in
   while !m <> None do
     match !m with
     | None -> raise Not_found
     | Some x -> if x = n1 then
         begin match x.prev with
           | None -> x.prev <- Some n2; d.first <- Some n2
           | Some y -> y.next <- Some n2; x.prev <- Some n2
         end
       else (); m := x.next
   done

 (* [remove d n] mutates dlist [d] by removing node [n].
  * requires: [n] is a node of [d]. *)
 let remove (d: 'a dlist) (n: 'a node) : unit =
   let m = ref d.first in
   while !m <> None do
     match !m with
     | None -> raise Not_found
     | Some x -> if x = n then
         begin match x.prev, x.next with
           | None, None -> d.first <- None; d.last <- None
           | Some y, None -> d.last <- Some y; y.next <- None
           | None, Some z -> d.first <- Some z; z.prev <- None
           | Some y, Some z -> y.next <- Some z; z.prev <- Some y
         end
       else (); m := x.next
   done

 (* [iter_forward d f] on a dlist [d] which has
  * elements n1; n2; ... is (f n1); (f n2); ... *)
 let iter_forward (d: 'a dlist) (f: 'a -> unit) : unit =
   let m = ref d.first in
   while !m <> None do
     match !m with
     | None -> raise Not_found
     | Some x -> f x.value;
       m := x.next
   done

 (* [iter_backward d f] on a dlist [d] which has
  * elements n1; n2; ... is ...; (f n2); (f n1) *)
 let iter_backward (d: 'a dlist) (f: 'a -> unit) : unit =
   let m = ref d.last in
   while !m <> None do
     match !m with
     | None -> raise Not_found
     | Some x -> f x.value;
       m := x.prev
   done
