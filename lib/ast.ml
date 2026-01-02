type t =
  | Symbol of string
  | Float of float
  | Integer of int
  | List of t list

let rec print = function
  | Symbol str -> str
  | Float num -> string_of_float num
  | Integer num -> string_of_int num
  | List exprs ->
    let str =
      exprs
      |> List.map print
      |> String.concat " "
    in
    String.concat "" ["("; str; ")"]
