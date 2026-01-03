open Parser
open Parser.O

let is_digit ch =
  '0' <= ch && ch <= '9'

let is_alpha ch =
  let lower = Char.lowercase_ascii ch in
  'a' <= lower && lower <= 'z'

let is_symbol_char =
  String.contains "+-*/"

let is_whitespace =
  String.contains " \n"

let whitespace = make_parser is_whitespace
let alpha = make_parser is_alpha
let digit = make_parser is_digit
let open_paren = make_parser (fun x -> x = '(')
let close_paren = make_parser (fun x -> x = ')')
let delimiter = eof <|> whitespace <|> close_paren
let dot = make_parser (fun x -> x = '.')
let syms = make_parser is_symbol_char

let integer =
  let int_of = map_opt int_of_string_opt in
  let* num = int_of (many1 (digit)) in
  let* _ = peek delimiter in
  return (Ast.Integer num)

let float =
  let float_of = map_opt float_of_string_opt in
  let* num = float_of (many1 (digit <|> dot)) in
  let* _ = peek delimiter in
  return (Ast.Float num)

let symbol =
  let* _ = peek (alpha <|> syms) in
  let* sym = string_of (many1 (alpha <|> digit <|> syms)) in
  let* _ = peek delimiter in
  return (Ast.Symbol sym)

let rec list s =
  (let* _ = open_paren in
   let* atoms = many sexp in
   let* _ = close_paren in
   let* _ = peek delimiter in
   return (Ast.List atoms))
  s
and sexp s =
  (let* _ = many whitespace in
   let* exp = integer <|> float <|> symbol <|> list in
   let* _ = many whitespace in
   return exp)
  s

let read = parse_string sexp

let rec repl () =
  let s = read_line () in
  let response =
    match read s with
    | Some (ast, _) -> Ast.print (Ast.eval ast)
    | None -> "Bad input"
  in
  Printf.printf "%s\n" response;
  repl ()
