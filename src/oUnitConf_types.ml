
type data = 
  | Unit 
  | Bool of string * bool
  | Int of string * int
  | Float of string * float
  | String of string
  | Var of string
  | Tuple of data list

let data_merge d1 d2 =
  match d1, d2 with 
    | Unit, d 
    | d, Unit -> d

    | Tuple lst1, Tuple lst2 -> Tuple (lst1 @ lst2)
    | Tuple lst, d -> Tuple (lst @ [d])
    | d, Tuple lst -> Tuple (d :: lst)

    | d1, d2 -> Tuple [d1; d2]

let rec string_of_data =
  function
    | Unit -> ""
    | Bool (str, _) -> str
    | Int (str, _) -> str
    | Float (str, _) -> str
    | String str -> str
    | Var str -> str
    | Tuple lst -> String.concat " " (List.map string_of_data lst)
