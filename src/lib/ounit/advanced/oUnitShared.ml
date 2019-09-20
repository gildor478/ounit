(**************************************************************************)
(* The OUnit library                                                      *)
(*                                                                        *)
(* Copyright (C) 2002-2008 Maas-Maarten Zeeman.                           *)
(* Copyright (C) 2010 OCamlCore SARL                                      *)
(* Copyright (C) 2013 Sylvain Le Gall                                     *)
(*                                                                        *)
(* The package OUnit is copyright by Maas-Maarten Zeeman, OCamlCore SARL  *)
(* and Sylvain Le Gall.                                                   *)
(*                                                                        *)
(* Permission is hereby granted, free of charge, to any person obtaining  *)
(* a copy of this document and the OUnit software ("the Software"), to    *)
(* deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute,           *)
(* sublicense, and/or sell copies of the Software, and to permit persons  *)
(* to whom the Software is furnished to do so, subject to the following   *)
(* conditions:                                                            *)
(*                                                                        *)
(* The above copyright notice and this permission notice shall be         *)
(* included in all copies or substantial portions of the Software.        *)
(*                                                                        *)
(* The Software is provided ``as is'', without warranty of any kind,      *)
(* express or implied, including but not limited to the warranties of     *)
(* merchantability, fitness for a particular purpose and noninfringement. *)
(* In no event shall Maas-Maarten Zeeman be liable for any claim, damages *)
(* or other liability, whether in an action of contract, tort or          *)
(* otherwise, arising from, out of or in connection with the Software or  *)
(* the use or other dealings in the software.                             *)
(*                                                                        *)
(* See LICENSE.txt for details.                                           *)
(**************************************************************************)

exception Lock_failure

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
             raise Lock_failure
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
