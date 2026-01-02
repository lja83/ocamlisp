type t =
  | Symbol of string
  | Float of float
  | Integer of int
  | List of t list
