type t =
  | Integer of int
  | Float of float
  | Symbol of string
  | List of t list

let rec print = function
  | Integer num -> string_of_int num
  | Float num -> string_of_float num
  | Symbol str -> str
  | List exprs ->
    let str =
      exprs
      |> List.map print
      |> String.concat " "
    in
    String.concat "" ["("; str; ")"]

type number =
  | Float of float
  | Int of int

module StringMap = Map.Make(String)
let funcs = StringMap.of_seq @@ List.to_seq [
    ("+", fun (x, y) -> Integer (x + y))
]

(*
let rec eval = function
  | Integer num -> Integer num
  | Float num -> Float num
  | Symbol str -> Symbol str
  | List exprs ->
    match List.map eval exprs with
    | Some func :: args -> 
      let func = StringMap.find_opt func funcs in
      func args
    | _ -> (Symbol "error")
*)

