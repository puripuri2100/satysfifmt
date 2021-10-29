
exception ParseErrorDetail of Range.t * string


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


type header_element =
  | HeaderRequire of string
  | HeaderImport  of string


type manual_type = Range.t * manual_type_main
and manual_type_main =
  | MTypeName        of (manual_type list) * module_name list * type_name
  | MTypeParam       of var_name
  | MFuncType        of manual_type list * manual_type * manual_type
  | MProductType     of manual_type list
  | MRecordType      of manual_type Assoc.t
      [@printer (fun fmt _ -> Format.fprintf fmt "MRecordType(...)")]
  | MHorzCommandType of manual_command_argument_type list
  | MVertCommandType of manual_command_argument_type list
  | MMathCommandType of manual_command_argument_type list
[@@deriving show]

and manual_command_argument_type =
  | MMandatoryArgumentType of manual_type
  | MOptionalArgumentType  of manual_type

type manual_kind =
  | MUniversalKind
  | MRecordKind    of manual_type Assoc.t
      [@printer (fun fmt _ -> Format.fprintf fmt "MRecordKind(...)")]
[@@deriving show]


(* ---- untyped ---- *)

let pp_sep fmt () =
  Format.fprintf fmt ";@ "


type stage =
  | Persistent0
  | Stage0
  | Stage1


let string_of_stage = function
  | Persistent0 -> "persistent stage"
  | Stage0      -> "stage 0"
  | Stage1      -> "stage 1"


type untyped_macro_parameter =
  | UTLateMacroParam  of (Range.t * var_name)
  | UTEarlyMacroParam of (Range.t * var_name)
[@@deriving show { with_path = false; } ]


type untyped_letrec_binding =
  UTLetRecBinding of manual_type option * Range.t * var_name * untyped_abstract_tree

and untyped_input_horz_element = Range.t * untyped_input_horz_element_main
  [@printer (fun fmt (_, utihmain) -> Format.fprintf fmt "%a" pp_untyped_input_horz_element_main utihmain)]

and untyped_input_horz_element_main =
  | UTInputHorzText         of string
      [@printer (fun fmt s -> Format.fprintf fmt "IT:%s" s)]
  | UTInputHorzEmbedded     of untyped_abstract_tree * untyped_command_argument list
      [@printer (fun fmt (utast, lst) -> Format.fprintf fmt "IC:%a %a" pp_untyped_abstract_tree utast (Format.pp_print_list ~pp_sep pp_untyped_command_argument) lst)]
  | UTInputHorzContent      of untyped_abstract_tree
  | UTInputHorzEmbeddedMath of untyped_abstract_tree
  | UTInputHorzEmbeddedCodeText of string
  | UTInputHorzMacro        of (Range.t * ctrlseq_name) * untyped_macro_argument list

and untyped_macro_argument =
  | UTLateMacroArg  of untyped_abstract_tree
  | UTEarlyMacroArg of untyped_abstract_tree

and untyped_input_vert_element = Range.t * untyped_input_vert_element_main
  [@printer (fun fmt (_, utivmain) -> Format.fprintf fmt "%a" pp_untyped_input_vert_element_main utivmain)]

and untyped_input_vert_element_main =
  | UTInputVertEmbedded of untyped_abstract_tree * untyped_command_argument list
      [@printer (fun fmt (utast, lst) -> Format.fprintf fmt "BC:%a %a" pp_untyped_abstract_tree utast (Format.pp_print_list ~pp_sep pp_untyped_command_argument) lst)]
  | UTInputVertContent  of untyped_abstract_tree
  | UTInputVertMacro    of (Range.t * ctrlseq_name) * untyped_macro_argument list

and 'a untyped_path_component =
  | UTPathLineTo        of 'a
  | UTPathCubicBezierTo of untyped_abstract_tree * untyped_abstract_tree * 'a

and untyped_abstract_tree =
  Range.t * untyped_abstract_tree_main
    [@printer (fun fmt (_, utastmain) -> Format.fprintf fmt "%a" pp_untyped_abstract_tree_main utastmain)]

and untyped_abstract_tree_main =
(* -- basic value -- *)
  | UTUnitConstant
      [@printer (fun fmt () -> Format.fprintf fmt "U:()")]
  | UTBooleanConstant      of bool
  | UTIntegerConstant      of int
  | UTFloatConstant        of float
  | UTLengthDescription    of float * length_unit_name
      [@printer (fun fmt (fl, lun) -> Format.fprintf fmt "L:%f%s" fl lun)]
  | UTStringEmpty
  | UTStringConstant       of string
      [@printer (fun fmt s -> Format.fprintf fmt "S:\"%s\"" s)]
  | UTPositionedString     of input_position * string
(* -- inputs -- *)
  | UTInputHorz            of untyped_input_horz_element list
  | UTInputVert            of untyped_input_vert_element list
  | UTConcat               of untyped_abstract_tree * untyped_abstract_tree
  | UTLambdaHorz           of Range.t * var_name * untyped_abstract_tree
  | UTLambdaVert           of Range.t * var_name * untyped_abstract_tree
  | UTLambdaMath           of untyped_abstract_tree
(* -- graphics -- *)
  | UTPath                 of untyped_abstract_tree * (untyped_abstract_tree untyped_path_component) list * (unit untyped_path_component) option
(* ----------------------------------
(* -- horizontal box list -- *)
  | UTHorz                 of HorzBox.horz_box list
  | UTHorzConcat           of untyped_abstract_tree * untyped_abstract_tree
(* -- vertical box list -- *)
  | UTVert                 of HorzBox.vert_box list
  | UTVertConcat           of untyped_abstract_tree * untyped_abstract_tree
---------------------------------- *)
(* -- list value -- *)
  | UTListCons             of untyped_abstract_tree * untyped_abstract_tree
  | UTEndOfList
(* -- tuple value -- *)
  | UTTuple               of untyped_abstract_tree list
(* -- record value -- *)
  | UTRecord               of (field_name * untyped_abstract_tree) list
  | UTAccessField          of untyped_abstract_tree * field_name
  | UTUpdateField          of untyped_abstract_tree * field_name * untyped_abstract_tree
(* -- fundamental -- *)
  | UTContentOf            of (module_name list) * var_name
      [@printer (fun fmt (_, vn) -> Format.fprintf fmt "%s" vn)]
  | UTApply                of untyped_abstract_tree * untyped_abstract_tree
      [@printer (fun fmt (u1, u2) -> Format.fprintf fmt "(%a %a)" pp_untyped_abstract_tree u1 pp_untyped_abstract_tree u2)]
  | UTApplyOmission        of untyped_abstract_tree
  | UTApplyOptional        of untyped_abstract_tree * untyped_abstract_tree
  | UTLetRecIn             of untyped_letrec_binding list * untyped_abstract_tree
  | UTLetNonRecIn          of manual_type option * untyped_pattern_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTIfThenElse           of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTFunction             of (Range.t * var_name) list * untyped_pattern_tree * untyped_abstract_tree
  | UTOpenIn               of Range.t * module_name * untyped_abstract_tree
  | UTFinishHeaderFile
  | UTFinishStruct
(* -- pattern match -- *)
  | UTPatternMatch         of untyped_abstract_tree * untyped_pattern_branch list
  | UTConstructor          of constructor_name * untyped_abstract_tree
      [@printer (fun fmt (cn, u) -> Format.fprintf fmt "%s(%a)" cn pp_untyped_abstract_tree u)]
(* -- declaration of type and module -- *)
  | UTDeclareVariantIn     of untyped_mutual_variant_cons * untyped_abstract_tree
  | UTModule               of Range.t * module_name * manual_signature option * untyped_abstract_tree * untyped_abstract_tree
(* -- imperative -- *)
  | UTLetMutableIn         of Range.t * var_name * untyped_abstract_tree * untyped_abstract_tree
  | UTSequential           of untyped_abstract_tree * untyped_abstract_tree
  | UTWhileDo              of untyped_abstract_tree * untyped_abstract_tree
  | UTOverwrite            of Range.t * var_name * untyped_abstract_tree
(* -- lightweight itemize -- *)
  | UTItemize              of untyped_itemize
(* -- math -- *)
  | UTMath                 of untyped_math
(* -- for lightweight command definition -- *)
  | UTLexHorz              of untyped_abstract_tree * untyped_abstract_tree
  | UTLexVert              of untyped_abstract_tree * untyped_abstract_tree
(* -- multi-stage constructs -- *)
  | UTNext                 of untyped_abstract_tree
  | UTPrev                 of untyped_abstract_tree
(* -- macros -- *)
  | UTLetHorzMacroIn       of Range.t * ctrlseq_name * untyped_macro_parameter list * untyped_abstract_tree * untyped_abstract_tree
  | UTLetVertMacroIn       of Range.t * ctrlseq_name * untyped_macro_parameter list * untyped_abstract_tree * untyped_abstract_tree

and constraints = (var_name * manual_kind) list

and manual_signature_content =
  | SigType   of untyped_type_argument list * type_name
  | SigValue  of var_name * manual_type * constraints
  | SigDirect of var_name * manual_type * constraints
(*
  | SigModule of module_name * manual_signature
*)

and manual_signature = manual_signature_content list

and untyped_itemize =
  | UTItem of untyped_abstract_tree * (untyped_itemize list)

and untyped_constructor_dec = Range.t * constructor_name * manual_type

and untyped_mutual_variant_cons =
  | UTMutualVariantCons    of untyped_type_argument list * Range.t * type_name * untyped_constructor_dec list * untyped_mutual_variant_cons
  | UTMutualSynonymCons    of untyped_type_argument list * Range.t * type_name * manual_type * untyped_mutual_variant_cons
  | UTEndOfMutualVariant

and untyped_pattern_tree = Range.t * untyped_pattern_tree_main
and untyped_pattern_tree_main =
  | UTPIntegerConstant     of int
  | UTPBooleanConstant     of bool
  | UTPStringConstant      of string
  | UTPUnitConstant
  | UTPListCons            of untyped_pattern_tree * untyped_pattern_tree
  | UTPEndOfList
  | UTPTuple               of untyped_pattern_tree list
  | UTPWildCard
  | UTPVariable            of var_name
  | UTPAsVariable          of var_name * untyped_pattern_tree
  | UTPConstructor         of constructor_name * untyped_pattern_tree

and untyped_pattern_branch =
  | UTPatternBranch     of untyped_pattern_tree * untyped_abstract_tree
  | UTPatternBranchWhen of untyped_pattern_tree * untyped_abstract_tree * untyped_abstract_tree

and untyped_unkinded_type_argument = Range.t * var_name

and untyped_type_argument = Range.t * var_name * manual_kind

and untyped_math = Range.t * untyped_math_main

and untyped_math_main =
  | UTMChars       of Uchar.t list
      [@printer (fun ppf uchs ->
        let buf = Buffer.create (4 * List.length uchs) in
        uchs |> List.iter (Buffer.add_utf_8_uchar buf);
        let s = Buffer.contents buf in
        Format.fprintf ppf "(UTMChars \"%s\")" s
      )]
  | UTMSuperScript of untyped_math * bool * untyped_math
  | UTMSubScript   of untyped_math * bool * untyped_math
  | UTMCommand     of untyped_abstract_tree * untyped_command_argument list
  | UTMList        of untyped_math list
  | UTMEmbed       of untyped_abstract_tree

and untyped_command_argument =
  | UTMandatoryArgument of untyped_abstract_tree
  | UTOptionalArgument  of untyped_abstract_tree
  | UTOmission          of Range.t
[@@deriving show { with_path = false; }]

type untyped_letrec_pattern_branch =
  | UTLetRecPatternBranch of untyped_pattern_tree list * untyped_abstract_tree

type untyped_argument =
  | UTPatternArgument  of untyped_pattern_tree
  | UTOptionalArgument of Range.t * var_name

type untyped_let_binding = manual_type option * untyped_pattern_tree * untyped_argument list * untyped_abstract_tree


let get_range (rng, _) = rng

