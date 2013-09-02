
(** Property list.

   @see http://bit.ly/17mVpKv Eigenclass Article on property list.
  *)

type t = (int, unit -> unit) Hashtbl.t

let create () = Hashtbl.create 13

exception Not_set

let new_property ?default () =
  let id = Oo.id (object end) in
  let v = ref default in
  let set t x =
    Hashtbl.replace t id (fun () -> v := Some x)
  in
  let get t =
    try
      (Hashtbl.find t id) ();
      match !v with
        | Some x ->
            v := None;
            x
        | None ->
            raise Not_set
    with Not_found ->
      raise Not_set
  in
    (set, get)
