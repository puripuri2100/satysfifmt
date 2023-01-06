open Sedlexing
open Parser
open Range
open Lexing
open Error
open Logging
open Types
open Hashtbl


(*
  * The SATySFi lexer is stateful; the transitions are:
  * | to \ from |program|block |inline|active |  math  |
  * |-----------|-------|------|------|-------|--------|
  * |  program  | (   ) |      |      | (   ) | !(   ) |
  * |           | (| |) |      |      | (| |) | !(| |) |
  * |           | [   ] |      |      | [   ] | ![   ] |
  * |  block    | '<  > | <  > | <  > | <     | !<   > |
  * |  inline   | {   } | {  } | {  } | {     | !{   } |
  * |  active   |       | +x ; | \x ; |       |        |
  * |           |       | #x ; | #x ; |       |        |
  * |  math     | ${  } |      | ${ } |       | {    } |
  *
  * Note that the active-block and active-inline transitions are one-way.
  *)



let get_pos lexbuf =
  let (posS, posE) = Sedlexing.lexing_positions lexbuf in
  let fname = posS.Lexing.pos_fname in
  let lnum = posS.Lexing.pos_lnum in
  let cnumS = posS.Lexing.pos_cnum - posS.Lexing.pos_bol in
  let cnumE = posE.Lexing.pos_cnum - posE.Lexing.pos_bol in
    Range.make fname lnum cnumS cnumE


let get_pos_last_num i lexbuf =
  let (_, posE) = Sedlexing.lexing_positions lexbuf in
  let fname = posE.Lexing.pos_fname in
  let lnum = posE.Lexing.pos_lnum in
  let cnumE = posE.Lexing.pos_cnum - posE.Lexing.pos_bol in
    Range.make fname lnum (cnumE - i) cnumE


let get_pos_last lexbuf = get_pos_last_num 1 lexbuf


let lexeme lexbuf = Sedlexing.Utf8.lexeme lexbuf


let report_error lexbuf errmsg =
  let rng = get_pos lexbuf in
    raise (LexError(rng, errmsg))


let remove_space_break str =
  let str_len = String.length str in
  let rec sub_f pos =
    match String.sub str pos 1 with
    | " " -> sub_f (pos + 1)
    | "\n" -> sub_f (pos + 1)
    | "\r" -> sub_f (pos + 1)
    | _ -> pos
  in
  let pos = sub_f 0 in
  String.sub str pos (str_len - pos)



type lexer_mode =
  | ProgramState
  | VerticalState
  | HorizontalState
  | ActiveState
  | MathState


let mode_stack_ref = ref [ProgramState]

let mode_push mode =
  mode_stack_ref := mode::!mode_stack_ref

let mode_pop lexbuf msg =
  match !mode_stack_ref with
  | [] -> report_error lexbuf msg
  | m::ms -> mode_stack_ref := ms

let mode_pop_opt () =
  match !mode_stack_ref with
  | [] -> None
  | m::ms -> mode_stack_ref := ms; Some(())

let top_mode () =
  match !mode_stack_ref with
  | [] -> failwith "empty stack"
  | m::_ -> m

let top_mode_opt () =
  match !mode_stack_ref with
  | [] -> None
  | m::_ -> Some(m)



let split_module_list tokstr =
  let lst = String.split_on_char '.' tokstr in
    match List.rev lst with
    | ident :: mdllstrev -> (List.rev mdllstrev, ident)
    | []                 -> assert false


let split_length_unitnm tokstr =
  let re = Str.regexp "[-0-9\\.]+" in
  if Str.string_match re tokstr 0 then
    let matched = Str.matched_string tokstr in
    let start = String.length matched in
    let rest = String.sub tokstr start (String.length tokstr - start) in
    (matched, rest)
  else
    assert false


let space = [%sedlex.regexp? ' ' | '\t']
let break = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let nzdigit = [%sedlex.regexp? '1'..'9']
let digit = [%sedlex.regexp? nzdigit | '0']
let hex = [%sedlex.regexp? digit | 'A'..'F']
let capital = [%sedlex.regexp? 'A'..'Z']
let small = [%sedlex.regexp? 'a'..'z']
let latin = [%sedlex.regexp? small | capital]
let item = [%sedlex.regexp? Plus '*']
let lower = [%sedlex.regexp? small, Star (digit | latin | '-')]
let upper = [%sedlex.regexp? capital, Star (digit | latin | '-')]
let symbol = [%sedlex.regexp? ' '..'@' | '['..'`' | '{'..'~']
let opsymbol = [%sedlex.regexp? '+' | '-' | '*' | '/' | '^' | '&' | '|' | '!' | ':' | '=' | '<' | '>' | '~' | '\'' | '.' | '?']
let notstr = [%sedlex.regexp? ' ' | '\t' | '\n' | '\r' | '@' | '`' | '\\' | '{' | '}' | '<' | '>' | '%' | '|' | '*' | '$' | '#' | ';']
let mathsymboltop = [%sedlex.regexp? '+' | '-' | '*' | '/' | ':' | '=' | '<' | '>' | '~' | '.' | ',' | '`']
let mathsymbol = [%sedlex.regexp? mathsymboltop | '?']
let mathascii = [%sedlex.regexp? small | capital | digit]
let notmathstr = [%sedlex.regexp? '+' | '-' | '*' | '/' | ':' | '=' | '<' | '>' | '~' | '.' | ',' | '`' | '?' | ' ' | '\t' | '\n' | '\r' | '\\' | '{' | '}' | '%' | '|' | '$' | '#' | ';' | '\'' | '^' | '_' | '!' | mathascii]




let rec progexpr line_break_counter comment_stack lexbuf =
  match%sedlex lexbuf with
  | '%' -> (
    let buffer = Buffer.create 256 in
    let (comment_str, lexbuf) = comment buffer lexbuf in
    progexpr 0 ((line_break_counter, comment_str)::comment_stack) lexbuf
  )
  | '@', Plus '`' -> (
    let pos_start = get_pos lexbuf in
    let quote_length = (String.length (lexeme lexbuf)) - 1 in
    let buffer = Buffer.create 256 in
    let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
    let pos = Range.unite pos_start pos_last in
    if not omit_post then Logging.warn_number_sign_end pos_last;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) None
    in
    match Range.get_last pos_start with
    | None ->
        assert false

    | Some(last) ->
        let (fname, ln, col) = last in
        let ipos =
          {
            input_file_name = fname;
            input_line      = ln;
            input_column    = col;
          }
        in
        POSITIONED_STRING(pos, token_data, ipos, s)
  )
  | '@' -> (
    let (headertype, content) = lex_header lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) None
    in
    match headertype with
    | "require" -> HEADER_REQUIRE(pos, token_data, content)
    | "import"  -> HEADER_IMPORT(pos, token_data, content)
    | _ ->
        raise (LexError(pos, "undefined header type '" ^ headertype ^ "'"))
  )
  | space -> progexpr line_break_counter comment_stack lexbuf
  | break -> progexpr (line_break_counter + 1) comment_stack lexbuf
  | "(|" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ProgramState;
    L_RECORD(pos, token_data)
  )
  | "|)" -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "too many closing";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    R_RECORD(pos, token_data)
  )
  | '(' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ProgramState;
    L_PAREN(pos, token_data)
  )
  | ')' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "too many closing";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    R_PAREN(pos, token_data)
  )
  | '[' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ProgramState;
    L_SQUARE(pos, token_data)
  )
  | ']' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "too many closing";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    R_SQUARE(pos, token_data)
  )
  | "{", Star (break | space),"|" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    mode_push HorizontalState;
    L_INLINE_TEXT_LIST(pos, token_data)
  )
  | '{' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    mode_push HorizontalState;
    L_INLINE_TEXT(pos, token_data)
  )
  | "\'<" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push VerticalState;
    L_BLOCK_TEXT(pos, token_data)
  )
  | "${", Star (break | space),"|" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push MathState;
    L_MATH_TEXT_LIST(pos, token_data)
  )
  | "${" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push MathState;
    L_MATH_TEXT(pos, token_data)
  )
  | Plus '`' -> (
    let pos_start = get_pos lexbuf in
    let quote_length = String.length (lexeme lexbuf) in
    let buffer = Buffer.create 256 in
    let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
    let pos = Range.unite pos_start pos_last in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    STRING(pos, token_data, s, true, omit_post)
  )
  | '#', Plus '`' -> (
    let pos_start = get_pos lexbuf in
    let quote_length = (String.length (lexeme lexbuf)) - 1 in
    let buffer = Buffer.create 256 in
    let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
    let pos = Range.unite pos_start pos_last in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    STRING(pos, token_data, s, false, omit_post)
  )
  | '\\', (lower| upper), '@' -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BACKSLASH_MACRO(pos, token_data, tokstr)
  )
  | '\\', (lower | upper) -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BACKSLASH_CMD(pos, token_data, tokstr)
  )
  | '+', (lower | upper), '@' -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    PLUS_MACRO(pos, token_data, tokstr)
  )
  | '+', (lower | upper) -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    PLUS_CMD(pos, token_data, tokstr)
  )
  | '#' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    ACCESS(pos, token_data)
  )
  | "?'", lower -> (
    let tok = lexeme lexbuf in
    let tok_len = String.length tok in
    let xpltyvarnm = String.sub tok 2 (tok_len - 2) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    ROWVAR(pos, token_data, xpltyvarnm)
  )
  | "?" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    QUESTION(pos, token_data)
  )
  | ":>" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    COERCE(pos, token_data)
  )
  | "->" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    ARROW(pos, token_data)
  )
  | "<-" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    REVERSED_ARROW(pos, token_data)
  )
  | '_' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    WILDCARD(pos, token_data)
  )
  | ',' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    COMMA(pos, token_data)
  )
  | "::" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    CONS(pos, token_data)
  )
  | ':' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    COLON(pos, token_data)
  )

  (* -- start: binary operators -- *)
  | '+', Star opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BINOP_PLUS(pos, token_data, tok)
  )
  | '-', Plus opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BINOP_MINUS(pos, token_data, tok)
  )
  | '*', Plus opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BINOP_TIMES(pos, token_data, tok)
  )
  | '/', Star opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BINOP_DIVIDES(pos, token_data, tok)
  )
  | '=', Plus opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BINOP_EQ(pos, token_data, tok)
  )
  | '<', Star opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BINOP_LT(pos, token_data, tok)
  )
  | '>', Star opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BINOP_GT(pos, token_data, tok)
  )
  | '&', Plus opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BINOP_AMP(pos, token_data, tok)
  )
  | '|', Plus opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BINOP_BAR(pos, token_data, tok)
  )
  | '^', Star opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BINOP_HAT(pos, token_data, tok)
  )
  | '!', Star opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    UNOP_EXCLAM(pos, token_data, tok)
  )
  (* -- end: binary operators -- *)


  | '\'', lower -> (
    let tok = lexeme lexbuf in
    let tok_len = String.length tok in
    let xpltyvarnm = String.sub tok 1 (tok_len - 1) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    TYPEVAR(pos, token_data, xpltyvarnm)
  )

  | Plus (upper, '.'), lower -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let (mdlnmlst, varnm) = split_module_list tokstr in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
      LONG_LOWER(pos, token_data, mdlnmlst, varnm)
  )

  | lower -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    match tokstr with
    | "and"       -> AND(pos, token_data)
    | "as"        -> AS(pos, token_data)
    | "block"     -> BLOCK(pos, token_data)
    | "command"   -> COMMAND(pos, token_data)
    | "else"      -> ELSE(pos, token_data)
    | "end"       -> END(pos, token_data)
    | "false"     -> FALSE(pos, token_data)
    | "fun"       -> FUN(pos, token_data)
    | "if"        -> IF(pos, token_data)
    | "include"   -> INCLUDE(pos, token_data)
    | "inline"    -> INLINE(pos, token_data)
    | "in"        -> IN(pos, token_data)
    | "let"       -> LET(pos, token_data)
    | "mod"       -> MOD(pos, token_data)
    | "match"     -> MATCH(pos, token_data)
    | "math"      -> MATH(pos, token_data)
    | "module"    -> MODULE(pos, token_data)
    | "mutable"   -> MUTABLE(pos, token_data)
    | "of"        -> OF(pos, token_data)
    | "open"      -> OPEN(pos, token_data)
    | "persistent"-> PERSISTENT(pos, token_data)
    | "rec"       -> REC(pos, token_data)
    | "signature" -> SIGNATURE(pos, token_data)
    | "sig"       -> SIG(pos, token_data)
    | "struct"    -> STRUCT(pos, token_data)
    | "then"      -> THEN(pos, token_data)
    | "true"      -> TRUE(pos, token_data)
    | "type"      -> TYPE(pos, token_data)
    | "val"       -> VAL(pos, token_data)
    | "with"      -> WITH(pos, token_data)
    | _           -> LOWER(pos, token_data, tokstr)
  )

  | Plus (upper, '.'), upper -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let (mdlnmlst, varnm) = split_module_list tokstr in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
      LONG_UPPER(pos, token_data, mdlnmlst, varnm)
  )

  | upper -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    UPPER(pos, token_data, tokstr)
  )

  | ((Opt '-', digit) | (Opt '-', nzdigit, Plus digit)), lower -> (
    let tokstr = lexeme lexbuf in
    let (size_str, unitnm) = split_length_unitnm tokstr in
    let size = size_str |> int_of_string |> float_of_int in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    LENGTH(pos, token_data, size, unitnm)
  )
  | Opt '-', Plus digit, '.', Star digit, lower -> (
    let tokstr = lexeme lexbuf in
    let (size_str, unitnm) = split_length_unitnm tokstr in
    let size = size_str |> float_of_string in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    LENGTH(pos, token_data, size, unitnm)
  )
  | Opt '-', '.', Plus digit, lower -> (
    let tokstr = lexeme lexbuf in
    let (size_str, unitnm) = split_length_unitnm tokstr in
    let size = size_str |> float_of_string in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    LENGTH(pos, token_data, size, unitnm)
  )
  | (Opt '-', digit) | (Opt '-', nzdigit, Plus digit) -> (
    let i = int_of_string (lexeme lexbuf) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    INT(pos, token_data, i)
  )
  | ("0x" | "0X"), Plus hex -> (
    let i = int_of_string (lexeme lexbuf) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    INT(pos, token_data, i)
  )
  | (Opt '-', Plus digit, '.', Star digit) | (Opt '-', '.', Plus digit) -> (
    let f = float_of_string (lexeme lexbuf) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    FLOAT(pos, token_data, f)
  )

  (* -- start: exact binary operators -- *)
  | '|' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BAR(pos, token_data)
  )
  | '-' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    EXACT_MINUS(pos, token_data)
  )
  | '=' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    EXACT_EQ(pos, token_data)
  )
  | '*' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    EXACT_TIMES(pos, token_data)
  )
  | '&' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    EXACT_AMP(pos, token_data)
  )
  | '~' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    EXACT_TILDE(pos, token_data)
  )
  (* -- end: exact binary operators -- *)

  | eof -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) None
    in
    if (List.length !mode_stack_ref) = 1 then
      EOI(pos, token_data)
    else
      report_error lexbuf "text input ended while reading a program area"
  )

  | any -> (
    let s = lexeme lexbuf in
    report_error lexbuf ("illegal token '" ^ s ^ "' in a program area")
  )
  | _ -> report_error lexbuf "illegal token in a program area"


  and lex_block line_break_counter comment_stack lexbuf =
  match%sedlex lexbuf with
  | '%' -> (
    let buffer = Buffer.create 256 in
    let (comment_str, lexbuf) = comment buffer lexbuf in
    lex_block 0 ((line_break_counter, comment_str)::comment_stack) lexbuf
  )
  | space -> lex_block line_break_counter comment_stack lexbuf
  | break -> lex_block (line_break_counter + 1) comment_stack lexbuf
  | '#', lower -> (
    let tokstr = lexeme lexbuf in
    let varnm = String.sub tokstr 1 ((String.length tokstr) - 1) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    VAR_IN_TEXT(pos, token_data, [], varnm)
  )
  | '#', Star (upper, '.'), (lower | upper) -> (
    let csnmpure = lexeme lexbuf in
    let csstr = String.sub csnmpure 1 ((String.length csnmpure) - 1) in
    let (mdlnmlst, csnm) = split_module_list csstr in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    VAR_IN_TEXT(pos, token_data, mdlnmlst, csnm)
  )
  | '+', (lower | upper), '@' -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    PLUS_MACRO(pos, token_data, tokstr)
  )
  | '+', (lower | upper) -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    PLUS_CMD(pos, token_data, tokstr)
  )
  | '+', Star (upper, '.'), (lower | upper) -> (
    let tokstrpure = lexeme lexbuf in
    let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
    let (mdlnmlst, csnm) = split_module_list tokstr in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    LONG_PLUS_CMD(pos, token_data, mdlnmlst, csnm)
  )
  | '<' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
      mode_push VerticalState;
      L_BLOCK_TEXT(pos, token_data)
  )
  | '>' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "too many closing";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    R_BLOCK_TEXT(pos, token_data)
  )
  | "{", Star (break | space),"|" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    mode_push HorizontalState;
    L_INLINE_TEXT_LIST(pos, token_data)
  )
  | '{' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    mode_push HorizontalState;
    L_INLINE_TEXT(pos, token_data)
  )

  | eof -> (
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) None
    in
    if (List.length !mode_stack_ref) = 1 then
      let pos = get_pos lexbuf in
      EOI(pos, token_data)
    else
      report_error lexbuf "unexpected end of input while reading a vertical area"
  )

  | any -> (
    let s = lexeme lexbuf in
    report_error lexbuf ("unexpected character '" ^ s ^ "' in a vertical area")
  )
  | _ -> report_error lexbuf "unexpected character in a vertical area"


and lex_inline line_break_counter comment_stack lexbuf =
  match%sedlex lexbuf with
  | '%' -> (
    let buffer = Buffer.create 256 in
    let (comment_str, lexbuf) = comment buffer lexbuf in
    lex_inline 0 ((line_break_counter, comment_str)::comment_stack) lexbuf
  )
  | Star (break | space), "{", Star (break | space),"|" -> (
    mode_push HorizontalState;
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    L_INLINE_TEXT_LIST(pos, token_data)
  )
  | Star (break | space), '{' -> (
    mode_push HorizontalState;
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    L_INLINE_TEXT(pos, token_data)
  )
  | Star (break | space), "|", Star (break | space),"}" -> (
    mode_pop lexbuf "too many closing";
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    R_INLINE_TEXT_LIST(pos, token_data)
  )
  | Star (break | space), '}' -> (
    mode_pop lexbuf "too many closing";
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    R_INLINE_TEXT(pos, token_data)
  )
  | Star (break | space), '<' -> (
    mode_push VerticalState;
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    L_BLOCK_TEXT(pos, token_data)
  )
  | Star (break | space), '|' -> (
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    BAR(pos, token_data)
  )
  | Star (break | space), item -> (
    let item_str = remove_space_break (lexeme lexbuf) in
    let item_str_len = String.length item_str in
    let pos = get_pos_last_num item_str_len lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    ITEM(pos, token_data, item_str_len)
  )
  | space -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    SPACE(pos, token_data)
  )
  | break -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    BREAK(pos, token_data)
  )
  | '#', lower -> (
    let tokstr = lexeme lexbuf in
    let varnm = String.sub tokstr 1 ((String.length tokstr) - 1) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    VAR_IN_TEXT(pos, token_data, [], varnm)
  )
  | '#', Star (upper, '.'), (lower | upper) -> (
    let csnmpure = lexeme lexbuf in
    let csstr = String.sub csnmpure 1 ((String.length csnmpure) - 1) in
    let (mdlnmlst, csnm) = split_module_list csstr in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    VAR_IN_TEXT(pos, token_data, mdlnmlst, csnm)
  )
  | '\\', (lower | upper), '@' -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    BACKSLASH_MACRO(pos, token_data, tokstr)
  )
  | '\\', (lower | upper) -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
   BACKSLASH_CMD(pos, token_data, tokstr)
  )
  | '\\', Star (upper, '.'), (lower | upper) -> (
    let tokstrpure = lexeme lexbuf in
    let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
    let (mdlnmlst, csnm) = split_module_list tokstr in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    LONG_BACKSLASH_CMD(pos, token_data, mdlnmlst, csnm)
  )
  | '\\', symbol -> (
    let tokstr = String.sub (lexeme lexbuf) 1 1 in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    CHAR(pos, token_data, tokstr)
  )
  | "${", Star (break | space),"|" -> (
    mode_push MathState;
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_MATH_TEXT_LIST(pos, token_data)
  )
  | "${" -> (
    mode_push MathState;
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_MATH_TEXT(pos, token_data)
  )
  | Plus '`' -> (
    let pos_start = get_pos lexbuf in
    let quote_length = String.length (lexeme lexbuf) in
    let buffer = Buffer.create 256 in
    let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
    let pos = Range.unite pos_start pos_last in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    STRING(pos, token_data, s, true, omit_post)
  )
  | '#', Plus '`' -> (
    let pos_start = get_pos lexbuf in
    let quote_length = (String.length (lexeme lexbuf)) - 1 in
    let buffer = Buffer.create 256 in
    let (pos_last, s, omit_post) = literal quote_length buffer lexbuf in
    let pos = Range.unite pos_start pos_last in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment_horz lexbuf)
    in
    STRING(pos, token_data, s, false, omit_post)
  )
  | eof -> (
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) None
    in
    if (List.length !mode_stack_ref) = 1 then
      let pos = get_pos lexbuf in
      EOI(pos, token_data)
    else
      report_error lexbuf "unexpected end of input while reading an inline text area"
  )

  | any -> (
    let pos_start = get_pos lexbuf in
    Sedlexing.rollback lexbuf;
    let buffer = Buffer.create 256 in
    let (s, lexbuf) = lex_char buffer lexbuf in
    let pos_end = get_pos lexbuf in
    let pos = Range.unite pos_start pos_end in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    CHAR(pos, token_data, s)
  )
  | _ -> report_error lexbuf "unexpected character in a text area"


and mathexpr line_break_counter comment_stack lexbuf =
  match%sedlex lexbuf with
  | space -> mathexpr line_break_counter comment_stack lexbuf
  | break -> mathexpr (line_break_counter + 1) comment_stack lexbuf
  | '%' -> (
    let buffer = Buffer.create 256 in
    let (comment_str, lexbuf) = comment buffer lexbuf in
    mathexpr 0 ((line_break_counter, comment_str)::comment_stack) lexbuf
  )
  | "?" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    QUESTION(pos, token_data)
  )
  | "!{", Star (break | space),"|" -> (
    let pos = get_pos lexbuf in
    mode_push HorizontalState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_INLINE_TEXT_LIST(pos, token_data)
  )
  | "!{" -> (
    let pos = get_pos lexbuf in
    mode_push HorizontalState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_INLINE_TEXT(pos, token_data)
  )
  | "!<" -> (
    let pos = get_pos lexbuf in
    mode_push VerticalState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_BLOCK_TEXT(pos, token_data)
  )
  | "!(|" -> (
    let pos = get_pos lexbuf in
    mode_push ProgramState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_RECORD(pos, token_data)
  )
  | "!(" -> (
    let pos = get_pos lexbuf in
    mode_push ProgramState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_PAREN(pos, token_data)
  )
  | "![" -> (
    let pos = get_pos lexbuf in
    mode_push ProgramState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_SQUARE(pos, token_data)
  )
  | "{", Star (break | space),"|" -> (
    let pos = get_pos lexbuf in
    mode_push MathState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_MATH_TEXT_LIST(pos, token_data)
  )
  | '{' -> (
    let pos = get_pos lexbuf in
    mode_push MathState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_MATH_TEXT(pos, token_data)
  )
  | "|", Star (break | space),"}" -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "too many closing";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    R_MATH_TEXT_LIST(pos, token_data)
  )
  | '}' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "too many closing";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    R_MATH_TEXT(pos, token_data)
  )
  | '|' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BAR(pos, token_data)
  )
  | '^' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    SUPERSCRIPT(pos, token_data)
  )
  | '_' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    SUBSCRIPT(pos, token_data)
  )
  | Plus "\'" -> (
    let n = String.length (lexeme lexbuf) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    PRIMES(pos, token_data, n)
  )
  | mathsymboltop, Star mathsymbol -> (
    let str = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    MATHCHARS(pos, token_data, str)
  )
  | mathascii -> (
    let str = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    MATHCHARS(pos, token_data, str)
  )
  | '#', lower -> (
    let tokstr = lexeme lexbuf in
    let varnm = String.sub tokstr 1 ((String.length tokstr) - 1) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    VAR_IN_TEXT(pos, token_data, [], varnm)
  )
  | '#', Star (upper, '.'), (lower | upper) -> (
    let csnmpure = lexeme lexbuf in
    let csstr = String.sub csnmpure 1 ((String.length csnmpure) - 1) in
    let (mdlnmlst, csnm) = split_module_list csstr in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    VAR_IN_TEXT(pos, token_data, mdlnmlst, csnm)
  )
  | '\\', (lower | upper) -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    BACKSLASH_CMD(pos, token_data, tokstr)
  )
  | '\\', Star (upper, '.'), (lower | upper) -> (
    let tokstrpure = lexeme lexbuf in
    let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
    let (mdlnmlst, csnm) = split_module_list tokstr in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    LONG_BACKSLASH_CMD(pos, token_data, mdlnmlst, csnm)
  )
  | '\\', symbol -> (
    let tok = String.sub (lexeme lexbuf) 1 1 in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    MATHCHARS(pos, token_data, tok)
  )

  | any -> (
    let pos_start = get_pos lexbuf in
    Sedlexing.rollback lexbuf;
    let buffer = Buffer.create 256 in
    let (s, lexbuf) = lex_mathchars buffer lexbuf in
    let pos_end = get_pos lexbuf in
    let pos = Range.unite pos_start pos_end in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    MATHCHARS(pos, token_data, s)
  )
  | _ -> report_error lexbuf "unexpected character in a math area"


and active line_break_counter comment_stack lexbuf  =
  match%sedlex lexbuf with
  | '%' -> (
    let buffer = Buffer.create 256 in
    let (comment_str, lexbuf) = comment buffer lexbuf in
    active 0 ((line_break_counter, comment_str)::comment_stack) lexbuf
  )
  | space -> active line_break_counter comment_stack lexbuf
  | break -> active (line_break_counter + 1) comment_stack lexbuf
  | '~' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    EXACT_TILDE(pos, token_data)
  )
  | '?' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    QUESTION(pos, token_data)
  )
  | "(|" -> (
    let pos = get_pos lexbuf in
    mode_push ProgramState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_RECORD(pos, token_data)
  )
  | '(' -> (
    let pos = get_pos lexbuf in
    mode_push ProgramState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_PAREN(pos, token_data)
  )
  | '[' -> (
    let pos = get_pos lexbuf in
    mode_push ProgramState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_SQUARE(pos, token_data)
  )
  | "{", Star (break | space),"|" -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "BUG; this cannot happen";
    mode_push HorizontalState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_INLINE_TEXT_LIST(pos, token_data)
  )
  | '{' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "BUG; this cannot happen";
    mode_push HorizontalState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_INLINE_TEXT(pos, token_data)
  )
  | '<' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "BUG; this cannot happen";
    mode_push VerticalState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    L_BLOCK_TEXT(pos, token_data)
  )
  | ';' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "BUG; this cannot happen";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    SEMICOLON(pos, token_data)
  )
  | eof -> (
      report_error lexbuf "unexpected end of input while reading an active area"
  )

  | any -> (
    let s = lexeme lexbuf in
    report_error lexbuf ("unexpected character '" ^ s ^ "' in a text area")
  )
  | _ -> report_error lexbuf "unexpected character in a text area"



  and lex_header lexbuf =
  match%sedlex lexbuf with
  | lower -> (
    let headertype = lexeme lexbuf in
    let _ = lex_header_sub lexbuf in
    let buffer = Buffer.create 256 in
    let content = lex_header_content buffer lexbuf in
    (headertype, content)
  )
  | any -> (
    let s = lexeme lexbuf in
    report_error lexbuf ("illegal token '" ^ s ^ "' in a program area")
  )
  | _ -> report_error lexbuf "illegal token in a program area"

and lex_header_sub lexbuf =
  match%sedlex lexbuf with
  | ':', Star ' ' -> ()
  | any -> (
    let s = lexeme lexbuf in
    report_error lexbuf ("illegal token '" ^ s ^ "' in a program area")
  )
  | _ -> report_error lexbuf "illegal token in a program area"

and lex_header_content buffer lexbuf =
  match%sedlex lexbuf with
  | break -> Buffer.contents buffer
  | eof -> Buffer.contents buffer
  | any -> (
    let s = lexeme lexbuf in
    Buffer.add_string buffer s;
    lex_header_content buffer lexbuf
  )
  | _ -> report_error lexbuf "illegal token in a program area"


and literal quote_length buffer lexbuf =
  match%sedlex lexbuf with
  | Plus '`' -> (
    let tok = lexeme lexbuf in
    let len = String.length tok in
      if len < quote_length then begin
        Buffer.add_string buffer tok;
        literal quote_length buffer lexbuf
      end else if len > quote_length then
        report_error lexbuf "literal area was closed with too many '`'s"
      else
        let pos_last = get_pos lexbuf in
        (pos_last, Buffer.contents buffer, true)
  )
  | Plus '`', '#' -> (
    let tok = lexeme lexbuf in
    let len = (String.length tok) - 1 in
      if len < quote_length then begin
        Buffer.add_string buffer tok;
        literal quote_length buffer lexbuf
      end else if len > quote_length then
        report_error lexbuf "literal area was closed with too many '`'s"
      else
        let pos_last = get_pos lexbuf in
        (pos_last, Buffer.contents buffer, false)
  )
  | break -> (
    let s = lexeme lexbuf in
    Buffer.add_string buffer s;
    literal quote_length buffer lexbuf
  )
  | eof -> (
      report_error lexbuf "unexpected end of input while reading literal area"
    )
  | any -> (
    let s = lexeme lexbuf in
    Buffer.add_string buffer s;
    literal quote_length buffer lexbuf
  )
  | _ -> report_error lexbuf "illegal token in a literal area"


and lex_char buffer lexbuf =
  match%sedlex lexbuf with
  | notstr -> Sedlexing.rollback lexbuf; (Buffer.contents buffer, lexbuf)
  | any -> (
    let s = lexeme lexbuf in
    Buffer.add_string buffer s;
    lex_char buffer lexbuf
  )
  | _ -> report_error lexbuf "unexpected character in a text area"


and lex_mathchars buffer lexbuf =
  match%sedlex lexbuf with
  | space -> Sedlexing.rollback lexbuf; (Buffer.contents buffer, lexbuf)
  | break -> Sedlexing.rollback lexbuf; (Buffer.contents buffer, lexbuf)
  | notmathstr -> Sedlexing.rollback lexbuf; (Buffer.contents buffer, lexbuf)
  | any -> (
    let s = lexeme lexbuf in
    Buffer.add_string buffer s;
    lex_mathchars buffer lexbuf
  )
  | _ -> report_error lexbuf "unexpected character in a math area"



and comment buffer lexbuf =
  match%sedlex lexbuf with
  | break -> (Buffer.contents buffer, lexbuf)
  | any -> (
    let s = lexeme lexbuf in
    Buffer.add_string buffer s;
    comment buffer lexbuf
  )
  | _ -> failwith "unexpected character in a comment area"


and after_comment lexbuf =
  match%sedlex lexbuf with
  | space -> after_comment lexbuf
  | '%' -> (
    let buffer = Buffer.create 256 in
    let (comment_str, lexbuf) = comment buffer lexbuf in
    Some(comment_str)
  )
  | _ -> None


and after_comment_horz lexbuf =
  match%sedlex lexbuf with
  | space -> Sedlexing.rollback lexbuf; None
  | '%' -> (
    let buffer = Buffer.create 256 in
    let (comment_str, lexbuf) = comment buffer lexbuf in
    Some(comment_str)
  )
  | _ -> None




let lex lexbuf =
  match top_mode () with
  | ProgramState -> progexpr 0 [] lexbuf
  | VerticalState -> lex_block 0 [] lexbuf
  | HorizontalState -> lex_inline 0 [] lexbuf
  | ActiveState -> active 0 [] lexbuf
  | MathState -> mathexpr 0 [] lexbuf
