open Arg
open Filename

open OptionState
open Parser
open Lexer
open Error
open Types



let parse lexbuf =
  let lexer () =
    let (ante_position, post_position) =
      Sedlexing.lexing_positions lexbuf
    in
    let token = Lexer.lex lexbuf in
    (token, ante_position, post_position)
  in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Parser.main
  in
  parser lexer


  (* テスト用　lexerが返したトークンをプリントする *)
(*
let test_parse lexbuf =
  let rec sub () =
    let token = Lexer.lex lexbuf in
    match token with
    | EOI(_) -> print_endline "EOI\n"
    | _ -> Printf.printf "%s\n" (show_token token); sub ()
  in
  sub ()
*)



let arg_version () =
  let () = print_string "satysfifmt version 0.0.1\n" in
  exit 0


  let arg_input curdir s =
    let path =
      if Filename.is_relative s then
        Filename.concat curdir s
      else
        s
    in
    OptionState.set_input_file path


let arg_output curdir s =
  let path =
    if Filename.is_relative s then
      Filename.concat curdir s
    else
      s
  in
  OptionState.set_output_file path


let arg_spec curdir =
  [
    ("-v",        Arg.Unit(arg_version)  , "Prints version");
    ("--version", Arg.Unit(arg_version)  , "Prints version");
    ("-o",        Arg.String (arg_output curdir), "Specify output file name");
    ("--output",  Arg.String (arg_output curdir), "Specify output file name");
  ]


let main =
  Error.error_msg (fun () ->
    let curdir = Sys.getcwd () in
    let () = Arg.parse (arg_spec curdir) (arg_input curdir) "" in
    let input_file_name =
      match OptionState.input_file () with
      | Some(s) -> s
      | None -> raise NoInputMainFileName
    in
    let output_file_name_opt = OptionState.output_file () in
    let output_file_name =
      match output_file_name_opt with
      | Some(s) -> s
      | None -> input_file_name
    in
    let file_channel = open_in input_file_name in
    let lexbuf =
      Sedlexing.Utf8.from_channel file_channel
    in
    let () = Sedlexing.set_filename lexbuf input_file_name in
    let ctx = {
        depth = 2;
        tab_spaces = 2;
        line_width = 120;
        break_str = "\n";
        list_join_str = None;
        oneline_comment_format = (fun s -> "% " ^ s);
        block_comment_format = (fun _ lst -> List.map (fun s -> "% " ^ s) lst);
      }
    in
    let (head_rwc_lst, body_rwc) = parse lexbuf in
    let _ = close_in file_channel in
    let head_str_lst = List.map (Code_format.code_format ctx) head_rwc_lst in
    let body_str = Code_format.code_format ctx body_rwc in
    let head_code = Types.lst_join ctx.break_str head_str_lst in
    let code = head_code ^ ctx.break_str ^ ctx.break_str ^ body_str in
    let output_channel = open_out output_file_name in
    let () = output_string output_channel code in
    let _ = close_out output_channel in
    ()
  )

