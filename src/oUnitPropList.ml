
(** Property list.

   @see http://bit.ly/17mVpKv Eigenclass Article on property list.
  *)

type t = (int, unit -> unit) Hashtbl.t

let create () = Hashtbl.create 13

let new_property default =
  let id = Oo.id (object end) in
  let v = ref default in
  let set t x =
    Hashtbl.replace t id (fun () -> v := x)
  in
  let get t =
    try
      let x =
        (Hashtbl.find t id) ();
        !v
      in
        v := default;
        x
    with Not_found ->
      default
  in
    (set, get)
