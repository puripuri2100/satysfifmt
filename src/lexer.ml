open Sedlexing
open Parser
open Range
open Lexing
open Error
open Logging
open Types



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
let identifier = [%sedlex.regexp? small, Star (digit | latin | '-')]
let constructor = [%sedlex.regexp? capital, Star (digit | latin | '-')]
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
        Parser.POSITIONED_LITERAL(pos, token_data, ipos, s)
  )
  | '@' -> (
    let (headertype, content) = lex_header lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) None
    in
    match headertype with
    | "require" -> Parser.HEADER_REQUIRE(pos, token_data, content)
    | "import"  -> Parser.HEADER_IMPORT(pos, token_data, content)

    | "stage" ->
        begin
          match content with
          | "persistent" -> Parser.HEADER_PERSISTENT0(pos, token_data)
          | "0"          -> Parser.HEADER_STAGE0(pos, token_data)
          | "1"          -> Parser.HEADER_STAGE1(pos, token_data)
          | _            -> raise (LexError(pos, "undefined stage type '" ^ content ^ "'; should be 'persistent', '0', or '1'."))
        end

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
    Parser.BRECORD(pos, token_data)
  )
  | "|)" -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "too many closing";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.ERECORD(pos, token_data)
  )
  | '(' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ProgramState;
    Parser.LPAREN(pos, token_data)
  )
  | ')' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "too many closing";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.RPAREN(pos, token_data)
  )
  | '[' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ProgramState;
    Parser.BLIST(pos, token_data)
  )
  | ']' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "too many closing";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.ELIST(pos, token_data)
  )
  | ';' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.LISTPUNCT(pos, token_data)
  )
  | '{' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push HorizontalState;
    Parser.BHORZGRP(pos, token_data)
  )
  | "\'<" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push VerticalState;
    Parser.BVERTGRP(pos, token_data)
  )
  | "${" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push MathState;
    Parser.BMATHGRP(pos, token_data)
  )
  | "<[" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BPATH(pos, token_data)
  )
  | "]>" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.EPATH(pos, token_data)
  )
  | ".." -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.PATHCURVE(pos, token_data)
  )
  | "--" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.PATHLINE(pos, token_data)
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
    Parser.LITERAL(pos, token_data, s, true, omit_post)
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
    Parser.LITERAL(pos, token_data, s, false, omit_post)
  )
  | '\\', (identifier | constructor), '@' -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.HORZMACRO(pos, token_data, tokstr)
  )
  | '\\', (identifier | constructor) -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.HORZCMD(pos, token_data, tokstr)
  )
  | '+', (identifier | constructor), '@' -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.VERTMACRO(pos, token_data, tokstr)
  )
  | '+', (identifier | constructor) -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.VERTCMD(pos, token_data, tokstr)
  )
  | '#' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.ACCESS(pos, token_data)
  )
  | "->" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.ARROW(pos, token_data)
  )
  | "<-" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.OVERWRITEEQ(pos, token_data)
  )
  | '|' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BAR(pos, token_data)
  )
  | '_' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.WILDCARD(pos, token_data)
  )
  | "::" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.CONS(pos, token_data)
  )
  | ':' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.COLON(pos, token_data)
  )
  | ',' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.COMMA(pos, token_data)
  )

  (* -- start: binary operators -- *)
  | '+', Star opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BINOP_PLUS(pos, token_data, tok)
  )
  | '-', Plus opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BINOP_MINUS(pos, token_data, tok)
  )
  | '*', Plus opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BINOP_TIMES(pos, token_data, tok)
  )
  | '/', Star opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BINOP_DIVIDES(pos, token_data, tok)
  )
  | '=', Plus opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BINOP_EQ(pos, token_data, tok)
  )
  | '<', Star opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BINOP_LT(pos, token_data, tok)
  )
  | '>', Star opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BINOP_GT(pos, token_data, tok)
  )
  | '&', Plus opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BINOP_AMP(pos, token_data, tok)
  )
  | '|', Plus opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BINOP_BAR(pos, token_data, tok)
  )
  | '^', Star opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BINOP_HAT(pos, token_data, tok)
  )
  | '!', Star opsymbol -> (
    let tok = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.UNOP_EXCLAM(pos, token_data, tok)
  )
  | "?->" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.OPTIONALARROW(pos, token_data)
  )
  | "?:" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.OPTIONAL(pos, token_data)
  )
  | "?*" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.OMISSION(pos, token_data)
  )
  | '?' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.OPTIONALTYPE(pos, token_data)
  )
  (* -- end: binary operators -- *)

  | '\'', identifier -> (
    let tok = lexeme lexbuf in
    let tok_len = String.length tok in
    let xpltyvarnm = String.sub tok 1 (tok_len - 1) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.TYPEVAR(pos, token_data, xpltyvarnm)
  )

  | '=' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.DEFEQ(pos, token_data)
  )
  | '*' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.EXACT_TIMES(pos, token_data)
  )
  | '&' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.EXACT_AMP(pos, token_data)
  )
  | '~' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.EXACT_TILDE(pos, token_data)
  )

  | Plus (constructor, '.'), identifier -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let (mdlnmlst, varnm) = split_module_list tokstr in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
      Parser.VARWITHMOD(pos, token_data, mdlnmlst, varnm)
  )

  | identifier -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    match tokstr with
    | "not"               -> LNOT(pos, token_data)
    | "mod"               -> MOD(pos, token_data)
    | "if"                -> IF(pos, token_data)
    | "then"              -> THEN(pos, token_data)
    | "else"              -> ELSE(pos, token_data)
    | "let"               -> LETNONREC(pos, token_data)
    | "let-rec"           -> LETREC(pos, token_data)
    | "and"               -> LETAND(pos, token_data)
    | "in"                -> IN(pos, token_data)
    | "fun"               -> LAMBDA(pos, token_data)
    | "true"              -> TRUE(pos, token_data)
    | "false"             -> FALSE(pos, token_data)
    | "before"            -> BEFORE(pos, token_data)
    | "while"             -> WHILE(pos, token_data)
    | "do"                -> DO(pos, token_data)
    | "let-mutable"       -> LETMUTABLE(pos, token_data)
    | "match"             -> MATCH(pos, token_data)
    | "with"              -> WITH(pos, token_data)
    | "when"              -> WHEN(pos, token_data)
    | "as"                -> AS(pos, token_data)
    | "type"              -> TYPE(pos, token_data)
    | "of"                -> OF(pos, token_data)
    | "module"            -> MODULE(pos, token_data)
    | "struct"            -> STRUCT(pos, token_data)
    | "sig"               -> SIG(pos, token_data)
    | "val"               -> VAL(pos, token_data)
    | "end"               -> END(pos, token_data)
    | "direct"            -> DIRECT(pos, token_data)
    | "constraint"        -> CONSTRAINT(pos, token_data)
    | "let-inline"        -> LETHORZ(pos, token_data)
    | "let-block"         -> LETVERT(pos, token_data)
    | "let-math"          -> LETMATH(pos, token_data)
    | "controls"          -> CONTROLS(pos, token_data)
    | "cycle"             -> CYCLE(pos, token_data)
    | "inline-cmd"        -> HORZCMDTYPE(pos, token_data)
    | "block-cmd"         -> VERTCMDTYPE(pos, token_data)
    | "math-cmd"          -> MATHCMDTYPE(pos, token_data)
    | "command"           -> COMMAND(pos, token_data)
    | "open"              -> OPEN(pos, token_data)
    | _                   -> VAR(pos, token_data, tokstr)
  )

  | constructor -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.CONSTRUCTOR(pos, token_data, tokstr)
  )

  | ((Opt '-', digit) | (Opt '-', nzdigit, Plus digit)), identifier -> (
    let tokstr = lexeme lexbuf in
    let (size_str, unitnm) = split_length_unitnm tokstr in
    let size = size_str |> int_of_string |> float_of_int in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.LENGTHCONST(pos, token_data, size, unitnm)
  )
  | Opt '-', Plus digit, '.', Star digit, identifier -> (
    let tokstr = lexeme lexbuf in
    let (size_str, unitnm) = split_length_unitnm tokstr in
    let size = size_str |> float_of_string in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.LENGTHCONST(pos, token_data, size, unitnm)
  )
  | Opt '-', '.', Plus digit, identifier -> (
    let tokstr = lexeme lexbuf in
    let (size_str, unitnm) = split_length_unitnm tokstr in
    let size = size_str |> float_of_string in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.LENGTHCONST(pos, token_data, size, unitnm)
  )
  | digit | nzdigit, Plus digit -> (
    let i = int_of_string (lexeme lexbuf) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.INTCONST(pos, token_data, i)
  )
  | ("0x" | "0X"), Plus hex -> (
    let i = int_of_string (lexeme lexbuf) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.INTCONST(pos, token_data, i)
  )
  | (Plus digit, '.', Star digit) | ('.', Plus digit) -> (
    let f = float_of_string (lexeme lexbuf) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.FLOATCONST(pos, token_data, f)
  )

  | '-' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.EXACT_MINUS(pos, token_data)
  )

  | eof -> (
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) None
    in
    if (List.length !mode_stack_ref) = 1 then
      Parser.EOI(token_data)
    else
      report_error lexbuf "text input ended while reading a program area"
  )

  | any -> (
    let s = lexeme lexbuf in
    report_error lexbuf ("illegal token '" ^ s ^ "' in a program area")
  )
  | _ -> report_error lexbuf "illegal token in a program area"


  and vertexpr line_break_counter comment_stack lexbuf =
  match%sedlex lexbuf with
  | '%' -> (
    let buffer = Buffer.create 256 in
    let (comment_str, lexbuf) = comment buffer lexbuf in
    vertexpr 0 ((line_break_counter, comment_str)::comment_stack) lexbuf
  )
  | space -> vertexpr line_break_counter comment_stack lexbuf
  | break -> vertexpr (line_break_counter + 1) comment_stack lexbuf
  | '#', identifier -> (
    let tokstr = lexeme lexbuf in
    let varnm = String.sub tokstr 1 ((String.length tokstr) - 1) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    Parser.VARINVERT(pos, token_data, [], varnm)
  )
  | '#', Star (constructor, '.'), (identifier | constructor) -> (
    let csnmpure = lexeme lexbuf in
    let csstr = String.sub csnmpure 1 ((String.length csnmpure) - 1) in
    let (mdlnmlst, csnm) = split_module_list csstr in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    Parser.VARINVERT(pos, token_data, mdlnmlst, csnm)
  )
  | '+', (identifier | constructor), '@' -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    Parser.VERTMACRO(pos, token_data, tokstr)
  )
  | '+', (identifier | constructor) -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    Parser.VERTCMD(pos, token_data, tokstr)
  )
  | '+', Star (constructor, '.'), (identifier | constructor) -> (
    let tokstrpure = lexeme lexbuf in
    let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
    let (mdlnmlst, csnm) = split_module_list tokstr in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    Parser.VERTCMDWITHMOD(pos, token_data, mdlnmlst, "+" ^ csnm)
  )
  | '<' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
      mode_push VerticalState;
      Parser.BVERTGRP(pos, token_data)
  )
  | '>' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "too many closing";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.EVERTGRP(pos, token_data)
  )
  | '{' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push HorizontalState;
    Parser.BHORZGRP(pos, token_data)
  )

  | eof -> (
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) None
    in
    if (List.length !mode_stack_ref) = 1 then
      Parser.EOI(token_data)
    else
      report_error lexbuf "unexpected end of input while reading a vertical area"
  )

  | any -> (
    let s = lexeme lexbuf in
    report_error lexbuf ("unexpected character '" ^ s ^ "' in a vertical area")
  )
  | _ -> report_error lexbuf "unexpected character in a vertical area"


and horzexpr line_break_counter comment_stack lexbuf =
  match%sedlex lexbuf with
  | '%' -> (
    let buffer = Buffer.create 256 in
    let (comment_str, lexbuf) = comment buffer lexbuf in
    horzexpr 0 ((line_break_counter, comment_str)::comment_stack) lexbuf
  )
  | Star (break | space), '{' -> (
    mode_push HorizontalState;
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BHORZGRP(pos, token_data)
  )
  | Star (break | space), '}' -> (
    mode_pop lexbuf "too many closing";
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.EHORZGRP(pos, token_data)
  )
  | Star (break | space), '<' -> (
    mode_push VerticalState;
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BVERTGRP(pos, token_data)
  )
  | Star (break | space), '|' -> (
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.SEP(pos, token_data)
  )
  | Star (break | space), item -> (
    let item_str = remove_space_break (lexeme lexbuf) in
    let item_str_len = String.length item_str in
    let pos = get_pos_last_num item_str_len lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.ITEM(pos, token_data, item_str_len)
  )
  | space -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.SPACE(pos, token_data)
  )
  | break -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BREAK(pos, token_data)
  )
  | '#', identifier -> (
    let tokstr = lexeme lexbuf in
    let varnm = String.sub tokstr 1 ((String.length tokstr) - 1) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    Parser.VARINHORZ(pos, token_data, [], varnm)
  )
  | '#', Star (constructor, '.'), (identifier | constructor) -> (
    let csnmpure = lexeme lexbuf in
    let csstr = String.sub csnmpure 1 ((String.length csnmpure) - 1) in
    let (mdlnmlst, csnm) = split_module_list csstr in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    Parser.VARINHORZ(pos, token_data, mdlnmlst, csnm)
  )
  | '\\', (identifier | constructor), '@' -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    Parser.HORZMACRO(pos, token_data, tokstr)
  )
  | '\\', (identifier | constructor) -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    Parser.HORZCMD(pos, token_data, tokstr)
  )
  | '\\', Star (constructor, '.'), (identifier | constructor) -> (
    let tokstrpure = lexeme lexbuf in
    let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
    let (mdlnmlst, csnm) = split_module_list tokstr in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    mode_push ActiveState;
    Parser.HORZCMDWITHMOD(pos, token_data, mdlnmlst, "\\" ^ csnm)
  )
  | '\\', symbol -> (
    let tokstr = String.sub (lexeme lexbuf) 1 1 in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.CHAR(pos, token_data, tokstr)
  )
  | "${" -> (
    mode_push MathState;
    let pos = get_pos_last lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BMATHGRP(pos, token_data)
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
    Parser.LITERAL(pos, token_data, s, true, omit_post)
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
    Parser.LITERAL(pos, token_data, s, false, omit_post)
  )
  | eof -> (
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) None
    in
    if (List.length !mode_stack_ref) = 1 then
      Parser.EOI(token_data)
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
    Parser.CHAR(pos, token_data, s)
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
  | "?:" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.OPTIONAL(pos, token_data)
  )
  | "?*" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.OMISSION(pos, token_data)
  )
  | "!{" -> (
    let pos = get_pos lexbuf in
    mode_push HorizontalState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BHORZGRP(pos, token_data)
  )
  | "!<" -> (
    let pos = get_pos lexbuf in
    mode_push VerticalState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BVERTGRP(pos, token_data)
  )
  | "!(|" -> (
    let pos = get_pos lexbuf in
    mode_push ProgramState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BRECORD(pos, token_data)
  )
  | "!(" -> (
    let pos = get_pos lexbuf in
    mode_push ProgramState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.LPAREN(pos, token_data)
  )
  | "![" -> (
    let pos = get_pos lexbuf in
    mode_push ProgramState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BLIST(pos, token_data)
  )
  | '{' -> (
    let pos = get_pos lexbuf in
    mode_push MathState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BMATHGRP(pos, token_data)
  )
  | '}' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "too many closing";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.EMATHGRP(pos, token_data)
  )
  | '|' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.SEP(pos, token_data)
  )
  | '^' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.SUPERSCRIPT(pos, token_data)
  )
  | '_' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.SUBSCRIPT(pos, token_data)
  )
  | Plus "\'" -> (
    let n = String.length (lexeme lexbuf) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.PRIMES(pos, token_data, n)
  )
  | mathsymboltop, Star mathsymbol -> (
    let str = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.MATHCHARS(pos, token_data, str)
  )
  | mathascii -> (
    let str = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.MATHCHARS(pos, token_data, str)
  )
  | '#', identifier -> (
    let tokstr = lexeme lexbuf in
    let varnm = String.sub tokstr 1 ((String.length tokstr) - 1) in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.VARINMATH(pos, token_data, [], varnm)
  )
  | '#', Star (constructor, '.'), (identifier | constructor) -> (
    let csnmpure = lexeme lexbuf in
    let csstr = String.sub csnmpure 1 ((String.length csnmpure) - 1) in
    let (mdlnmlst, csnm) = split_module_list csstr in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.VARINMATH(pos, token_data, mdlnmlst, csnm)
  )
  | '\\', (identifier | constructor) -> (
    let tokstr = lexeme lexbuf in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.MATHCMD(pos, token_data, tokstr)
  )
  | '\\', Star (constructor, '.'), (identifier | constructor) -> (
    let tokstrpure = lexeme lexbuf in
    let tokstr = String.sub tokstrpure 1 ((String.length tokstrpure) - 1) in
    let (mdlnmlst, csnm) = split_module_list tokstr in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.MATHCMDWITHMOD(pos, token_data, mdlnmlst, "\\" ^ csnm)
  )
  | '\\', symbol -> (
    let tok = String.sub (lexeme lexbuf) 1 1 in
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.MATHCHARS(pos, token_data, tok)
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
    Parser.MATHCHARS(pos, token_data, s)
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
  | "?:" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.OPTIONAL(pos, token_data)
  )
  | "?*" -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.OMISSION(pos, token_data)
  )
  | '~' -> (
    let pos = get_pos lexbuf in
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.EXACT_TILDE(pos, token_data)
  )
  | "(|" -> (
    let pos = get_pos lexbuf in
    mode_push ProgramState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BRECORD(pos, token_data)
  )
  | '(' -> (
    let pos = get_pos lexbuf in
    mode_push ProgramState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.LPAREN(pos, token_data)
  )
  | '[' -> (
    let pos = get_pos lexbuf in
    mode_push ProgramState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BLIST(pos, token_data)
  )
  | '{' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "BUG; this cannot happen";
    mode_push HorizontalState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BHORZGRP(pos, token_data)
  )
  | '<' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "BUG; this cannot happen";
    mode_push VerticalState;
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.BVERTGRP(pos, token_data)
  )
  | ';' -> (
    let pos = get_pos lexbuf in
    mode_pop lexbuf "BUG; this cannot happen";
    let token_data =
      Types.make_token_data line_break_counter (List.rev comment_stack) (after_comment lexbuf)
    in
    Parser.ENDACTIVE(pos, token_data)
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
  | identifier -> (
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




let lex lexbuf =
  match top_mode () with
  | ProgramState -> progexpr 0 [] lexbuf
  | VerticalState -> vertexpr 0 [] lexbuf
  | HorizontalState -> horzexpr 0 [] lexbuf
  | ActiveState -> active 0 [] lexbuf
  | MathState -> mathexpr 0 [] lexbuf
