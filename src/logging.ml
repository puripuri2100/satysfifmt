type abs_path = AbsPath of string

type lib_path = LibPath of string

let open_in_abs (AbsPath(pathstr)) =
  open_in pathstr


let open_in_bin_abs (AbsPath(pathstr)) =
  open_in_bin pathstr


let open_out_abs (AbsPath(pathstr)) =
  open_out pathstr


let dirname_abs (AbsPath(pathstr)) =
  Filename.dirname pathstr


let basename_abs (AbsPath(pathstr)) =
  Filename.basename pathstr


let string_of_file (abspath : abs_path) : (string, string) result =
  try
    let ic = open_in_bin_abs abspath in
    let bufsize = 65536 in
    let stepsize = 65536 in
    let buf = Buffer.create bufsize in
    let bytes = Bytes.create stepsize in
    let flag = ref true in
    try
      while !flag do
        let c = input ic bytes 0 bufsize in
        if c = 0 then
          flag := false
        else
          Buffer.add_subbytes buf bytes 0 c
      done;
      close_in ic;
      let s = Buffer.contents buf in
      Ok(s)
    with
    | Failure(_) -> close_in ic; assert false
  with
  | Sys_error(msg) -> Error(msg)


let make_abs_path pathstr = AbsPath(pathstr)

let make_lib_path pathstr = LibPath(pathstr)

let get_abs_path_string (AbsPath(pathstr)) = pathstr

let get_lib_path_string (LibPath(pathstr)) = pathstr

let get_abs_path_extension (AbsPath(pathstr)) = Filename.extension pathstr


let show_path abspath =
  let pathstr = get_abs_path_string abspath in
  Filename.basename pathstr


let begin_to_typecheck_file abspath_in =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  type checking '" ^ (show_path abspath_in) ^ "' ...")


let begin_to_preprocess_file abspath_in =
  print_endline ("  preprocessing '" ^ (show_path abspath_in) ^ "' ...")


let begin_to_eval_file abspath_in =
  print_endline ("  evaluating '" ^ (show_path abspath_in) ^ "' ...")


let begin_to_parse_file abspath_in =
  print_endline ("  parsing '" ^ (show_path abspath_in) ^ "' ...")


let pass_type_check opt =
  match opt with
  | None ->
      print_endline ("  type check passed.")

  | Some(str) ->
      print_endline ("  type check passed. (" ^ str ^ ")")


let ordinal i =
  let suffix =
    match i mod 10 with
    | 1 -> "st"
    | 2 -> "nd"
    | 3 -> "rd"
    | _ -> "th"
  in
  (string_of_int i) ^ suffix


let start_evaluation i =
  print_endline (" ---- ---- ---- ----");
  begin
    if i <= 1 then
      print_endline ("  evaluating texts ...")
    else
      print_endline ("  evaluating texts (" ^ (ordinal i) ^ " trial) ...")
  end


let end_evaluation () =
  print_endline ("  evaluation done.")


let start_page_break () =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  breaking contents into pages ...")


let needs_another_trial () =
  print_endline ("  needs another trial for solving cross references...")


let achieve_count_max () =
  print_endline ("  could not reach to fixpoint when resolving cross references.")


let achieve_fixpoint unresolved_crossrefs =
  if unresolved_crossrefs = [] then
    print_endline ("  all cross references were solved.")
  else
    print_endline ("  some cross references were not solved: " ^ String.concat " " unresolved_crossrefs ^ ".")


let end_output file_name_out =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  output written on '" ^ (show_path file_name_out) ^ "'.")


let no_output () =
  print_endline " ---- ---- ---- ----";
  print_endline "  no output."


let target_file file_name_out =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  target file: '" ^ (show_path file_name_out) ^ "'")


let  dump_file dump_file_exists dump_file =
  if dump_file_exists then
    print_endline ("  dump file: '" ^ (show_path dump_file) ^ "' (already exists)")
  else
    print_endline ("  dump file: '" ^ (show_path dump_file) ^ "' (will be created)")


let begin_to_embed_fonts () =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  embedding fonts ...")


let begin_to_write_page () =
  print_endline (" ---- ---- ---- ----");
  print_endline ("  writing pages ...")


let show_single_font abbrev relpath =
  print_endline ("    * `" ^ abbrev ^ "`: '" ^ (get_lib_path_string relpath) ^ "'")


let show_collection_font abbrev relpath i =
  print_endline ("    * `" ^ abbrev ^ "`: '" ^ (get_lib_path_string relpath) ^ "' [" ^ (string_of_int i) ^ "]")


let warn_deprecated msg =
  print_endline ("  [Warning] " ^ msg)


let warn_cmyk_image file_name =
  print_endline ("  [Warning] (" ^ (show_path file_name) ^ ") Jpeg images with CMYK color mode are not fully supported.");
  print_endline ("  Please convert the image to a jpeg image with YCbCr (RGB) color model.")


let warn_math_script_without_brace rng =
  Format.printf "  [Warning] at %s: math script without brace.\n" (Range.to_string rng)


let warn_noninjective_cmap uchpre uch gidorg =
  Format.printf "  [Warning] Multiple Unicode code points (U+%04X and U+%04X) are mapped to the same GID %d.\n" (Uchar.to_int uchpre) (Uchar.to_int uch) gidorg


let warn_noninjective_ligature gidorglig =
  Format.printf "  [Warning] GID %d is used as more than one kind of ligatures.\n" gidorglig


let warn_nonattachable_mark gomark gobase =
  Format.printf "  [Warning] The combining diacritical mark of GID %d cannot be attached to the base glyph of GID %d.\n" gomark gobase


let warn_no_glyph abbrev uch =
  Format.printf "  [Warning] No glyph is provided for U+%04X by font `%s`.\n" (Uchar.to_int uch) abbrev


let warn_no_math_glyph mfabbrev uch =
  Format.printf "  [Warning] No glyph is provided for U+%04X by math font `%s`.\n" (Uchar.to_int uch) mfabbrev


let warn_duplicate_font_hash abbrev relpath =
  Format.printf "  [Warning] more than one font is named `%s`; '%s' will be associated with the font name.\n" abbrev (get_lib_path_string relpath)


let warn_duplicate_math_font_hash mfabbrev relpath =
  Format.printf "  [Warning] more than one font is named `%s`; '%s' will be associated with the font name.\n" mfabbrev (get_lib_path_string relpath)


let warn_number_sign_end rng =
  Format.printf "  [Warning] at %a: '#' has no effect here\n"
    Range.pp rng


let warn_overfull_line (pageno : int) =
  Format.printf "  [Warning] an overfull line occurs on page %d\n"
    pageno


let warn_underfull_line (pageno : int) =
  Format.printf "  [Warning] an underfull line occurs on page %d\n"
    pageno


let warn_unreachable (pageno : int) =
  Format.printf "  [Warning] a line unable to be broken into a paragraph occurs on page %d\n"
    pageno
