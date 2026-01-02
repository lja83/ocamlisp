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
