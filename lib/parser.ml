type 'a parse_result = 'a * char list
type 'a t = char list -> 'a parse_result option

let return (x : 'a) : 'a t =
  fun s -> Some (x, s)

let fail : 'a t =
  fun _ -> None

let bind (p : 'a t) (f : ('a -> 'b t)) : 'b t =
  fun input ->
    match p input with
    | Some (x, rest) -> f x rest
    | None -> None

let choice (p1 : 'a t) (p2 : 'a t) : 'a t =
  fun input ->
    match p1 input with
    | Some x -> Some x
    | None -> p2 input

module O = struct
  let ( >>= ) = bind
  let ( <|> ) = choice
  let ( let* ) = ( >>= )
end 

let rec many (p : 'a t) : 'a list t =
  let open O in
  (many1 p) <|> return []

and many1 (p : 'a t) : 'a list t =
  let open O in
  let* x = p in
  let* xs = many p in
  return (x :: xs)

let both (p1 : 'a t) (p2 : 'b t) : ('a * 'b) t =
  let open O in
  let* x = p1 in
  let* y = p2 in
  return (x, y)

let peek (p : 'a t) : 'a t =
  fun input ->
    match p input with
    | Some (ch, _) -> Some (ch, input)
    | _ -> None

let eof : 'a t = function
  | [] -> Some ('\000', [])
  | _ -> None

let make_parser (f : char -> bool) : 'a t = function
  | ch :: rest when f ch -> Some (ch, rest)
  | _ -> None

let chars_to_string chars =
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let string_to_chars string =
  List.init (String.length string) (String.get string)

let string_of (p : 'a t) : string t =
  let open O in
  let* chars = p in
  return (chars_to_string chars)

let map_opt (f : string -> 'b option) (p : 'a t) : 'b t =
  let open O in
  let* str = string_of p in
  match f str with
  | Some v -> return v
  | _ -> fail

let parse_string (p : 'a t) string : 'a parse_result option =
  p (string_to_chars string)
