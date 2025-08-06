(********************************************************************
 * exercise: promise and resolve
 ********************************************************************)

module type Promise = sig

  type 'a state = Pending | Resolved of 'a | Rejected of exn
  type 'a promise
  type 'a resolver

  (** [make ()] is a new promise and resolver. The promise is pending. *)
  val make : unit -> 'a promise * 'a resolver

  (** [return x] is a new promise that is already resolved with value [x]. *)
  val return : 'a -> 'a promise

  (** [state p] is the state of the promise *)
  val state : 'a promise -> 'a state

  (** [resolve r x] resolves the promise [p] associated with [r]
      with value [x], meaning that [state p] will become
      [Resolved x].
      Requires:  [p] is pending. *)
  val resolve : 'a resolver -> 'a -> unit

  (** [reject r x] rejects the promise [p] associated with [r]
      with exception [x], meaning that [state p] will become
      [Rejected x].
      Requires:  [p] is pending. *)
  val reject : 'a resolver -> exn -> unit

  (** [p >>= c] registers callback [c] with promise [p].
      When the promise is resolved, the callback will be run
      on the promises's contents.  If the promise is never
      resolved, the callback will never run. *)
  val (>>=) : 'a promise -> ('a -> 'b promise) -> 'b promise
end

module Promise : Promise = struct
  type 'a state = Pending | Resolved of 'a | Rejected of exn

  (* RI: if [state <> Pending] then [callbacks = []]. *)
  type 'a promise = {
    mutable state : 'a state;
    mutable callbacks : ('a -> unit) list
  }

  type 'a resolver = 'a promise

  (** [write_once p s] changes the state of [p] to be [s].  If [p] and [s]
      are both pending, that has no effect.
      Raises: [Invalid_arg] if the state of [p] is not pending. *)
  let write_once p s =
    if p.state = Pending
    then p.state <- s
    else invalid_arg "cannot write twice"

  let make () =
    let p = {state = Pending; callbacks = []} in
    p, p

  let return x =
    {state = Resolved x; callbacks = []}

  let state p = p.state

  let reject r x =
    write_once r (Rejected x);
    r.callbacks <- []

  let run_callbacks callbacks x =
    List.iter (fun f -> f x) callbacks

  let resolve r x =
    write_once r (Resolved x);
    let callbacks = r.callbacks in
    r.callbacks <- [];
    run_callbacks callbacks x

  let (>>=) (p : 'a promise) (c : 'a -> 'b promise) : 'b promise =
    match p.state with
    | Resolved x -> c x
    | Rejected x -> {state = Rejected x; callbacks = []}
    | Pending ->
      let bind_promise, bind_resolver = make () in
      let f x : unit =
        let callback_promise = c x in
        match callback_promise.state with
        | Resolved x -> resolve bind_resolver x
        | Rejected x -> reject bind_resolver x
        | Pending -> failwith "impossible"
      in
      p.callbacks <- f :: p.callbacks;
      bind_promise
end

let _ =
  let open Promise in
  let p, r = make () in
  let _ = p >>= (fun i -> Printf.printf "%i\n" i; return ()) in
  resolve r 42

module LwtExercises = struct

  open Lwt.Infix  (* for [>>=] *)

  (********************************************************************
   * exercise: promise and resolve lwt
   ********************************************************************)

  (* to run this in utop, first [#require "lwt.unix";;] *)

  let _ =
    let p, r = Lwt.wait () in
    let _ = p >>= (fun i -> Lwt_io.printf "%i\n" i) in
    Lwt.wakeup r 42

  (********************************************************************
   * exercise: timing challenge 1
   ********************************************************************)

  (** [delay s] is a promise that resolves after about [s] seconds. *)
  let delay (sec : float) : unit Lwt.t =
    Lwt_unix.sleep sec

  (** prints ["done"] after about 3 seconds. *)
  let delay_then_print () =
    delay 3. >>= fun () ->
    Lwt_io.printl "done"

  (********************************************************************
   * exercise: timing challenge 2
   ********************************************************************)

  let timing2 () =
    let _t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
    let _t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
    let _t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
    Lwt_io.printl "all done"

  (* Answer:
     - "all done" prints immediately.
     - about a second later, "1" prints.
     - about 9 more seconds later, "2" prints.
     - about 10 more seconds later, "3" prints.
       The total elapsed time is about 20 seconds. *)

  (********************************************************************
   * exercise: timing challenge 3
   ********************************************************************)

  let timing3 () =
    delay 1. >>= fun () ->
    Lwt_io.printl "1" >>= fun () ->
    delay 10. >>= fun () ->
    Lwt_io.printl "2" >>= fun () ->
    delay 20. >>= fun () ->
    Lwt_io.printl "3" >>= fun () ->
    Lwt_io.printl "all done"

  (* Answer:
     - after about a second, "1" prints.
     - about 10 more seconds later, "2" prints.
     - about 20 more seconds later, "3" prints.
     - then "all done" immediately prints.
       The total elapsed time is about 31 seconds. *)

  (********************************************************************
   * exercise: timing challenge 4
   ********************************************************************)

  let timing4 () =
    let t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
    let t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
    let t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
    Lwt.join [t1; t2; t3] >>= fun () ->
    Lwt_io.printl "all done"

  (* Answer:
     - after about a second, "1" prints.
     - about 9 more seconds later, "2" prints.
     - about 10 more seconds later, "3" prints.
     - then "all done" immediately prints.
       The total elapsed time is about 20 seconds. *)

  (********************************************************************
   * exercise: file monitor
   ********************************************************************)

  open Lwt_io
  open Lwt_unix

  let log () : input_channel Lwt.t =
    openfile "log" [O_RDONLY] 0 >>= fun fd ->
    Lwt.return (of_fd input fd)

  let rec loop (ic : input_channel) =
    read_line ic >>= fun str ->
    printlf "%s" str >>= fun () ->
    loop ic

  let monitor () : unit Lwt.t =
    log () >>= loop

  let handler : exn -> unit Lwt.t = function
    | End_of_file -> Lwt.return ()
    | exc -> Lwt.fail exc

  let main () : unit Lwt.t =
    Lwt.catch monitor handler

  (* uncomment to actually run the monitor:
     let _ = Lwt_main.run (main ())
  *)

end

(********************************************************************
 * Maybe monad
 ********************************************************************)

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe : Monad =
struct
  type 'a t = 'a option

  let return x = Some x

  let (>>=) m f =
    match m with
    | Some x -> f x
    | None -> None

end

open Maybe

(********************************************************************
 * exercise: add opt
 ********************************************************************)

let add (x : int t) (y : int t) : int t =
  x >>= fun a ->
  y >>= fun b ->
  return (a + b)

(********************************************************************
 * exercise: fmap and join
 ********************************************************************)

module type ExtMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
end

module Maybe : ExtMonad =
struct
  type 'a t = 'a option

  let return x = Some x

  let (>>=) m f =
    match m with
    | Some x -> f x
    | None -> None

  let (>>|) m f =
    match m with
    | Some x -> Some (f x)
    | None -> None

  let join = function
    | Some m -> m
    | None -> None

end

(********************************************************************
 * exercise: fmap and join again
 ********************************************************************)

module Maybe : ExtMonad =
struct
  type 'a t = 'a option

  let return x = Some x

  let (>>=) m f =
    match m with
    | Some x -> f x
    | None -> None

  let (>>|) x f =
    x >>= fun a ->
    return (f a)

  let join x =
    x >>= fun y ->
    y

end

(********************************************************************
 * exercise: bind from fmap+join
 ********************************************************************)

module type FmapJoinMonad = sig
  type 'a t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
  val return : 'a -> 'a t
end

module type BindMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module MakeMonad (M : FmapJoinMonad) : BindMonad = struct
  include M
  let (>>=) m f =
    m >>| f |> join
end

(********************************************************************
 * exercise: list monad
 ********************************************************************)

module ListMonad : ExtMonad = struct
  type 'a t = 'a list

  let return x =
    [x]

  let join =
    List.flatten

  let (>>|) m f =
    List.map f m

  let (>>=) m f =
    m |> List.map f |> join
    (* or, m >>| f |> join *)
end

(********************************************************************
 * exercise: trivial monad laws
 ********************************************************************)

module Trivial : Monad = struct
  type 'a t = Wrap of 'a
  let return x = Wrap x
  let (>>=) (Wrap x) f = f x
end

(*
Law 1:  [return x >>= f] behaves the same as [f x].

Proof.  By the definition of [return], [return x >>= f] evaluates
  to [Wrap x >>= f].  By the definition of [>>=], that evaluates
  to [f x].

Law 2:  [m >>= return] behaves the same as [m].

Proof.  Since [m : 'a t] it must be [Wrap x] for some [x].  By the
  definition of [>>=], [m >>= return] thus evaluates to [return x].
  By the definition of [return], that evaluates to [Wrap x],
  which is just [m].

Law 3:  [m >>= f >>= g] behaves the same as [m >>= (fun x -> f x >>= g)].

Proof.  Since [m : 'a t] it must be [Wrap z] for some [z].  By the
  definition of [>>=], [m >>= f >>= g] thus evaluates to [f z >>= g].
  Likewise, [m >>= (fun x -> f x >>= g)] thus evaluates to
  [(fun x -> f x >>= g) z], which evaluates to [f z >>= g].

*)
