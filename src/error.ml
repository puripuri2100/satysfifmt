open Range

exception Parser_error of string

exception LexError of Range.t * string

exception NoInputMainFileName

type t =
  | Parser
  | Lexer
  | Option


let print_error (t:t) str =
  let err_title =
    match t with
    | Parser -> "Parser"
    | Lexer  -> "Lexer"
    | Option -> "Option"
  in
  Printf.printf "![%sError]\n%s\n" err_title str

let error_msg t =
  try
    t ()
  with
    | Parser_error(msg) -> print_error Parser msg
    | LexError(rng, msg) -> print_error Lexer ("at" ^ (Range.to_string rng) ^ ":" ^ msg)
    | NoInputMainFileName -> print_error Option "no input main file name"
