
type scope = ScopeGlobal | ScopeProcess

type 'a shared_noscope =
    {
      lock: 'a -> unit;
      unlock: 'a -> unit;
      try_lock: 'a -> bool;
    }

type shared =
    {
      global: int shared_noscope;
      process: int shared_noscope;
    }

let get_scoped shared =
  function
    | ScopeGlobal -> shared.global
    | ScopeProcess -> shared.process


(* Global variable that need to be set for threads. *)
let mutex_create =
  ref (fun () ->
         let r = ref false in

         let try_lock () =
           if !r then begin
             false
           end else begin
             r := true;
             true
           end
         in

         let lock () =
           if not (try_lock ()) then
             assert false
         in

         let unlock () =
           r := false
         in

         {
           lock = lock;
           try_lock = try_lock;
           unlock = unlock;
         })

module Mutex =
struct

  type t = int * scope

  let create scope =
    (Oo.id (object end), scope)

  let lock shared (id, scope) =
    (get_scoped shared scope).lock id

  let try_lock shared (id, scope) =
    (get_scoped shared scope).try_lock id

  let unlock shared (id, scope) =
    (get_scoped shared scope).unlock id

  let with_lock shared mutex f =
    try
      let res =
        lock shared mutex;
        f ()
      in
        unlock shared mutex;
        res
    with e ->
      unlock shared mutex;
      raise e


end

(* A simple shared_noscope that works only for 1 process. *)
let noscope_create () =
  let state = Hashtbl.create 13 in
  let state_mutex = !mutex_create () in

  let get_mutex id =
    let mutex =
      state_mutex.lock ();
      try
        Hashtbl.find state id
      with Not_found ->
        let mutex = !mutex_create () in
          Hashtbl.add state id mutex;
          mutex
    in
      state_mutex.unlock ();
      mutex
  in

  let try_lock id =
    (get_mutex id).try_lock ()
  in

  let lock id =
    (get_mutex id).lock ()
  in

  let unlock id =
    (get_mutex id).unlock ()
  in
    {
      lock = lock;
      unlock = unlock;
      try_lock = try_lock;
    }

(* Create a shared, for 1 process. *)
let create () =
  let scoped = noscope_create () in
  {
    global = scoped;
    process = scoped;
  }
