type t =
  | Integer of int
  | Float of float
  | Symbol of string
  | List of t list
  | Function of (t list -> t)

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
  | Function _ -> "function"

let add = function
  | (Integer a) :: (Integer b) :: _ -> Integer (a + b)
  | (Integer a) :: (Float b) :: _ -> Float ((float_of_int a) +. b)
  | (Float a) :: (Integer b) :: _ -> Float (a +. (float_of_int b))
  | _ -> Symbol "Error"

module StringMap = Map.Make(String)
let funcs = StringMap.of_seq @@ List.to_seq [
    ("+", add)
]

let rec eval = function
  | Integer num -> Integer num
  | Float num -> Float num
  | Symbol str ->
    (match StringMap.find_opt str funcs with
     | Some func -> Function func
     | None -> Symbol str)
  | List exprs ->
    (match List.map eval exprs with
     | (Function func) :: args -> func args
     | _ -> Symbol "Failed to run func")
  | Function func -> Function func
