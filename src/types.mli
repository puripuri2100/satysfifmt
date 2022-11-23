exception ParseErrorDetail of Range.t * string

type token_data = {
  td_line_break_counter : int;
  td_before_comments : (int * string) list;
  td_after_comment : string option;
}

val show_token_data : token_data -> string

val make_token_data : int -> (int * string) list -> string option -> token_data

val empty_token_data : token_data

val is_none : 'a option -> bool

val token_data_is_none : token_data -> bool

val token_comment_is_none : token_data -> bool

type command_name       = string
type var_name           = string
type id_name            = string
type class_name         = string
type type_name          = string
type kind_name          = string
type constructor_name   = string
type macro_name         = string
type module_name        = string
type signature_name     = string
type length_unit_name   = string
type type_variable_name = string
type row_variable_name  = string
type label              = string

val show_command_name       : command_name       -> string
val show_var_name           : var_name           -> string
val show_id_name            : id_name            -> string
val show_class_name         : class_name         -> string
val show_type_name          : type_name          -> string
val show_kind_name          : kind_name          -> string
val show_constructor_name   : constructor_name   -> string
val show_macro_name         : macro_name         -> string
val show_module_name        : module_name        -> string
val show_signature_name     : signature_name     -> string
val show_length_unit_name   : length_unit_name   -> string
val show_type_variable_name : type_variable_name -> string
val show_row_variable_name  : row_variable_name  -> string
val show_label              : label              -> string

type input_position = {
  input_file_name : string;
  input_line      : int;
  input_column    : int;
}


type column_config = {
  is_break : bool option;
  space_size : int option;
}

val show_column_config : column_config -> string

val column_config_default : column_config

val set_is_break : bool -> column_config -> column_config
val set_space_size : int -> column_config -> column_config

type rule =
  | AST of rule_with_comment
  | Raw of string
  | List of string * rule_with_comment list
  | Paren of string * rule_with_comment * string
  | Column of (rule_with_comment * column_config) list
  | Null

and rule_with_comment = {
  before_comments : string list;
  rule : rule;
  after_comment : string option;
}


val show_rule : rule -> string

val show_rule_with_comment : rule_with_comment -> string

val with_comment : rule -> token_data -> rule_with_comment

val with_comment_paren : token_data -> rule -> token_data -> rule_with_comment

val with_comment_none : rule -> rule_with_comment

type context = {
  depth : int;
  tab_spaces: int;
  line_width: int;
  break_str: string;
  list_join_str: string option;
  oneline_comment_format: string -> string;
  block_comment_format: context -> string list -> string list;
}

val increment_depth : context -> context
val indent : context -> string
val len_max : context -> int
val set_list_join_str : string option -> context -> context


val str_repeat : string -> int -> string
val is_lst_empty : 'a list -> bool
val lst_join : string -> string list -> string
val option_unwrap_or : 'a -> 'a option -> 'a
val second : ('a * 'b) -> 'b

val is_in_range : int -> int -> int -> bool
val is_break_ok_char : BatUChar.t -> bool
val is_space_zero_char : BatUChar.t -> bool
val break_str : string list -> (string * bool) list
val make_str_rwc_lst : (string * bool) list -> (int * string) list -> string option -> (rule_with_comment * column_config) list
val break_with_comment : (int * string) list -> string option -> string list -> (token_data * string) list -> (rule_with_comment * column_config) list
val break_inline_elem_text : (token_data * string) list -> (rule_with_comment * column_config) list
