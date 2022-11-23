
exception ParseErrorDetail of Range.t * string


type token_data = {
  td_line_break_counter : int;
  td_before_comments : (int * string) list;
  td_after_comment : string option;
}
[@@deriving show]

let make_token_data td_line_break_counter td_before_comments td_after_comment =
  { td_line_break_counter; td_before_comments; td_after_comment }

let empty_token_data =
  { td_line_break_counter = 0; td_before_comments = []; td_after_comment = None; }

let is_none opt =
  match opt with
  | None -> true
  | _ -> false

let token_data_is_none data =
  data.td_line_break_counter = 0 &&
  (List.length data.td_before_comments) = 0 &&
  is_none data.td_after_comment

let token_comment_is_none data =
  (List.length data.td_before_comments) = 0 &&
  is_none data.td_after_comment



type command_name       = string  [@@deriving show]
type var_name           = string  [@@deriving show]
type id_name            = string  [@@deriving show]
type class_name         = string  [@@deriving show]
type type_name          = string  [@@deriving show]
type kind_name          = string  [@@deriving show]
type constructor_name   = string  [@@deriving show]
type macro_name         = string  [@@deriving show]
type module_name        = string  [@@deriving show]
type signature_name     = string  [@@deriving show]
type length_unit_name   = string  [@@deriving show]
type type_variable_name = string  [@@deriving show]
type row_variable_name  = string  [@@deriving show]
type label              = string  [@@deriving show]


type input_position = {
  input_file_name : string;
  input_line      : int;
  input_column    : int;
}
[@@deriving show { with_path = false }]


type column_config = {
  is_break : bool option;
  space_size : int option;
}
[@@deriving show]

let column_config_default = {
  is_break = None;
  space_size = None
}
let set_is_break is_break self = {self with is_break = Some(is_break)}
let set_space_size size self = {self with space_size = Some(size)}

type rule =
  | AST of rule_with_comment
  | Raw of string
  | List of string * rule_with_comment list
  | Paren of string * rule_with_comment * string
  | Column of (rule_with_comment * column_config) list
  | Null
  [@@deriving show]

and rule_with_comment = {
  before_comments : string list;
  rule : rule;
  after_comment : string option;
}
[@@deriving show]

let with_comment (rule:rule) (token_data:token_data) : rule_with_comment ={
  before_comments = List.map (fun (_,s) -> s) token_data.td_before_comments;
  rule = rule;
  after_comment = token_data.td_after_comment;
}



let with_comment_paren (opn_data:token_data) (rule:rule) (cls_data:token_data) : rule_with_comment ={
  before_comments = List.map (fun (_,s) -> s) opn_data.td_before_comments;
  rule = rule;
  after_comment = cls_data.td_after_comment;
}

let with_comment_none (rule:rule) : rule_with_comment = {
  before_comments = [];
  rule = rule;
  after_comment = None;
}


type context = {
  depth : int;
  tab_spaces: int;
  line_width: int;
  break_str: string;
  list_join_str: string option;
  oneline_comment_format: string -> string;
  block_comment_format: context -> string list -> string list;
}


let str_repeat str count =
  let rec sub count buf =
    if count == 0 then
      buf
    else
      sub (count - 1) (buf ^ str)
  in
  sub count ""


let increment_depth self = { self with depth = self.depth + 1 }
let indent self = str_repeat " " self.tab_spaces
let len_max self =
  let indent_len = self.tab_spaces * self.depth in
  self.line_width - indent_len
let set_list_join_str j_opt self = { self with list_join_str = j_opt }


let is_string_empty str = String.equal str ""

let is_lst_empty lst =
  match lst with
  | [] -> true
  | _ -> false


let lst_join join lst =
  let rec sub lst buf =
    match lst with
    | [] -> buf
    | x::[] -> if is_string_empty buf then x else buf ^ join ^ x
    | x::xs -> if is_string_empty buf then sub xs x else sub xs (buf ^ join ^ x)
  in
  sub lst ""


let option_unwrap_or value opt =
  match opt with
  | Some(v) -> v
  | None -> value


let second (_,d) = d


let is_in_range i leq geq =
  leq <= i && i <= geq

(*
  その文字の後で改行してよいかの判定
  East Asianとスペース以外は改行不可

  https://unicode.org/reports/tr14/#BreakOpportunities
  https://www.asahi-net.or.jp/~AX2S-KMTN/ref/unicode/e_asia.html
*)
let is_break_ok_char uchar =
  let code = BatUChar.code uchar in
  List.exists (fun (leq, geq) -> is_in_range code leq geq) [
    (0x4E00, 0x9FFC);
    (0x3400, 0x4DB5);
    (0x20000, 0x2A6DD);
    (0x2A700, 0x2B734);
    (0x2B740, 0x2B81D);
    (0x2B820, 0x2CEA1);
    (0x2CEB0, 0x2EBE0);
    (0x30000, 0x3134A);
    (0xF900, 0xFAFF);
    (0x2F800, 0x2FA1F);
    (0x2F00, 0x2FDF);
    (0x2E80, 0x2EFF);
    (0x31C0, 0x31EF);
    (0x2FF0, 0x2FFF);
    (0x1100, 0x11FF);
    (0xA960, 0xA97F);
    (0xD7B0, 0xD7FF);
    (0x3130, 0x318F);
    (0xAC00, 0xD7A3);
    (0x3040, 0x309F);
    (0x30A0, 0x30FF);
    (0x31F0, 0x31FF);
    (0x1B000, 0x1B0FF);
    (0x1B100, 0x1B12F);
    (0x1AFF0, 0x1AFFF);
    (0x1B130, 0x1B16F);
    (0xFF00, 0xFFEF);
    (0x3190, 0x319F);
  ]


(*
  「隣り合っていて場合に行分割しない場合にそのあとに空白を入れない文字」の場合にtrueを返す
  - ID
  - CJ
  - IN
  - SA
  - JLOP
  - JLCP
  - JLFS
  - JLCM
  - JLPL
  がtrueになる
  一応アルファベットと英数字記号を除外すれば良さそう
  The SATySFi book v1のp.102と https://unicode.org/reports/tr14/#Table1 を参照

  - https://www.asahi-net.or.jp/~AX2S-KMTN/ref/unicode/european.html
  - https://www.asahi-net.or.jp/~AX2S-KMTN/ref/unicode/alphanumeric.html
*)
let is_space_zero_char uchar =
  let code = BatUChar.code uchar in
  not (List.exists (fun (leq, geq) -> is_in_range code leq geq) [
    (0x0530, 0x058F);
    (0x102A0, 0x102DF);
    (0x10530, 0x1056F);
    (0x10800, 0x1083F);
    (0x12F90, 0x12FFF);
    (0x0400, 0x04FF);
    (0x0500, 0x052F);
    (0x2DE0, 0x2DFF);
    (0xA640, 0xA69F);
    (0x1C80, 0x1C8F);
    (0x10500, 0x1052F);
    (0x10A0, 0x10FF);
    (0x1C90, 0x1CBF);
    (0x2D00, 0x2D2F);
    (0x2C00, 0x2C5F);
    (0x1E000, 0x1E02F);
    (0x10330, 0x1034F);
    (0x0370, 0x03FF);
    (0x1F00, 0x1FFF);
    (0x10140, 0x1018F);
    (0x0080, 0x00FF);
    (0x0100, 0x017F);
    (0x0180, 0x024F);
    (0x2C60, 0x2C7F);
    (0xA720, 0xA7FF);
    (0xAB30, 0xAB6F);
    (0x10780, 0x107BF);
    (0x1DF00, 0x1DFFF);
    (0x1E00, 0x1EFF);
    (0x0250, 0x02AF);
    (0x1D00, 0x1D7F);
    (0x1D80, 0x1DBF);
    (0x10600, 0x1077F);
    (0x10000, 0x1007F);
    (0x10100, 0x1013F);
    (0x10280, 0x1029F);
    (0x10920, 0x1093F);
    (0x1680, 0x169F);
    (0x10C80, 0x10CFF);
    (0x10300, 0x1032F);
    (0x10350, 0x1037F);
    (0x101D0, 0x101FF);
    (0x16A0, 0x16FF);
    (0x10450, 0x1047F);
    (0x10570, 0x105BF);
  ])



(*
  空白の除去と分割可能ポイントでの分割・スペースの有無を判定する
  まずその直後で分割可能かどうかを確認する
  分割可能であれば出力する
  分割不可能であればtmpに格納する
*)
let break_str (str_lst:string list) : (string * bool) list =
  (* 「スペースを入れない」にするかどうかを判定する *)
  let rec sub tmp lst =
    match lst with
    | [] -> []
    | " "::xs ->
      if String.length tmp == 0 then
        sub "" xs
      else
        (tmp,false) :: (sub "" xs)
    | x :: xs ->
      let char = BatUTF8.get x 0 in
      if is_break_ok_char char then
        if String.length tmp == 0 then
          (x,is_space_zero_char char) :: (sub "" xs)
        else
          (tmp^x,is_space_zero_char char) :: (sub "" xs)
      else
        sub (tmp^x) xs
  in
  sub "" str_lst


let make_str_rwc_lst
  (lst:(string * bool) list)
  (before_comments:(int * string) list)
  (after_comment:string option)
  :(rule_with_comment * column_config) list
=
  let rec sub lst =
    match lst with
    | [] -> []
    | (s,_)::[] ->
      let rwc = {
        before_comments = [];
        rule = Raw s;
        after_comment = after_comment;
      } in
      [(rwc, column_config_default)]
    | (s,b)::xs ->
      let head = {
        before_comments = [];
        rule = Raw s;
        after_comment = None;
      } in
      let config =
        if b then
          column_config_default |> set_space_size 0
        else
          column_config_default |> set_space_size 1
      in
      (head,config) :: (sub xs)
  in
  match lst with
  | [] -> []
  | (s,_)::[] ->
    let rwc = {
      before_comments = List.map second before_comments;
      rule = Raw s;
      after_comment = after_comment;
    } in
    [(rwc, column_config_default)]
  | (s, b)::xs ->
    let head = {
      before_comments = List.map second before_comments;
      rule = Raw s;
      after_comment = None;
    } in
    let config =
      if b then
        column_config_default |> set_space_size 0
      else
        column_config_default |> set_space_size 1
    in
    (head,config) :: (sub xs)


let rec break_with_comment
  (before_comments_tmp:(int * string) list)
  (after_comment_tmp:string option)
  (str_tmp:string list)
  (ichars:(token_data * string) list)
  : (rule_with_comment * column_config) list
  =
  match ichars with
  | [] ->
    if
      List.length str_tmp == 0 &&
      List.length before_comments_tmp == 0 &&
      is_none after_comment_tmp
    then
      []
    else
      let breaked_lst = break_str str_tmp in
      let rwc_lst = make_str_rwc_lst breaked_lst before_comments_tmp after_comment_tmp in
      rwc_lst
  | (tok, str)::xs ->
    if List.length tok.td_before_comments == 0 then
      if is_none tok.td_after_comment then
        break_with_comment before_comments_tmp after_comment_tmp (str::str_tmp) xs
      else
        (* after commentがある *)
        let breaked_lst = break_str (str::str_tmp) in
        let rwc_lst = make_str_rwc_lst breaked_lst before_comments_tmp tok.td_after_comment in
        List.append rwc_lst (break_with_comment [] None [] xs)
    else
      (* before commentsがある *)
      if is_none tok.td_after_comment then
        (* 今までの文字列を出力して、次の行を今の文字からはじめる *)
        let breaked_lst = break_str str_tmp in
        let rwc_lst = make_str_rwc_lst breaked_lst before_comments_tmp after_comment_tmp in
        List.append rwc_lst (break_with_comment tok.td_before_comments tok.td_after_comment [str] xs)
      else
        (* 今までの文字列を出力して、今の文字も出力するからはじめる *)
        let breaked_lst1 = break_str str_tmp in
        let rwc_lst1 = make_str_rwc_lst breaked_lst1 before_comments_tmp after_comment_tmp in
        let breaked_lst2 = break_str [str] in
        let rwc_lst2 = make_str_rwc_lst breaked_lst2 tok.td_before_comments tok.td_after_comment in
        List.append rwc_lst2 (List.append rwc_lst1 (break_with_comment [] None [] xs))

(*
  空白除去関係はまだ精度が悪いので改良の余地あり
  unidata/LineBreak.txtを見に行く必要もあるかも
  The SATySFi book p.102 参照
*)
let break_inline_elem_text (ichars:(token_data * string) list) : (rule_with_comment * column_config) list =
  break_with_comment [] None [] ichars

