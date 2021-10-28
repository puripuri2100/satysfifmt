(*
  !!! テスト用
  !!! lexerが正常に動作することを確認したら削除
*)


type token_data = {
  line_break_counter : int;
  before_comments : (int * string) list;
  after_comment : string option;
}
[@@deriving show]

let make_token_data line_break_counter before_comments after_comment =
  { line_break_counter; before_comments; after_comment }


type ctrlseq_name       = string  [@@deriving show]
type var_name           = string  [@@deriving show]
type id_name            = string  [@@deriving show]
type class_name         = string  [@@deriving show]
type type_name          = string  [@@deriving show]
type constructor_name   = string  [@@deriving show]
type module_name        = string  [@@deriving show]
type sig_var_name       = string  [@@deriving show]
type field_name         = string  [@@deriving show]
type type_argument_name = string  [@@deriving show]
type length_unit_name   = string  [@@deriving show]

type input_position = {
  input_file_name : string;
  input_line      : int;
  input_column    : int;
}
[@@deriving show { with_path = false }]

