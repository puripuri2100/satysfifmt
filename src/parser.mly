%{
  open Types

  let make_itemize_elem depth token_data rwc =
    let rec sub rwc dep =
      if dep <= 0 then
        rwc
      else
        Types.with_comment_none (AST rwc)
    in
    Types.with_comment (AST (sub rwc (depth - 1))) token_data

  let dummy = Types.with_comment_none Null

  let dummy_cc = column_config_default
%}

%token<Range.t * Types.token_data>
  AND AS BLOCK COMMAND ELSE END FALSE FUN
  IF IN INCLUDE INLINE LET MOD MATCH MATH MODULE MUTABLE OF OPEN
  REC SIG SIGNATURE STRUCT THEN TRUE TYPE VAL WITH PERSISTENT

%token<Range.t * Types.token_data> BAR WILDCARD COLON ARROW REVERSED_ARROW SEMICOLON COMMA CONS ACCESS QUESTION COERCE

%token<Range.t * Types.token_data>
  L_PAREN R_PAREN L_SQUARE R_SQUARE L_RECORD R_RECORD
  L_BLOCK_TEXT R_BLOCK_TEXT L_INLINE_TEXT R_INLINE_TEXT L_MATH_TEXT R_MATH_TEXT
  L_INLINE_TEXT_LIST R_INLINE_TEXT_LIST L_MATH_TEXT_LIST R_MATH_TEXT_LIST

%token<Range.t * Types.token_data> EXACT_MINUS EXACT_TIMES EXACT_AMP EXACT_TILDE EXACT_EQ

%token<Range.t * Types.token_data * Types.var_name>
  BINOP_TIMES BINOP_DIVIDES BINOP_PLUS BINOP_MINUS BINOP_HAT BINOP_AMP BINOP_BAR BINOP_GT BINOP_LT BINOP_EQ

%token<Range.t * Types.token_data * Types.var_name> UNOP_EXCLAM

%token<Range.t * Types.token_data * Types.var_name> LOWER
%token<Range.t * Types.token_data * Types.constructor_name> UPPER
%token<Range.t * Types.token_data * Types.module_name list * Types.var_name> LONG_LOWER
%token<Range.t * Types.token_data * Types.module_name list * Types.constructor_name> LONG_UPPER

%token<Range.t * Types.token_data * Types.command_name> BACKSLASH_CMD PLUS_CMD

%token<Range.t * Types.token_data * Types.module_name list * Types.command_name>
  LONG_BACKSLASH_CMD LONG_PLUS_CMD

%token<Range.t * Types.token_data * Types.module_name list * Types.var_name>
  VAR_IN_TEXT

%token<Range.t * Types.token_data * Types.type_variable_name> TYPEVAR
%token<Range.t * Types.token_data * Types.row_variable_name> ROWVAR

%token<Range.t * Types.token_data * int> INT
%token<Range.t * Types.token_data * float> FLOAT
%token<Range.t * Types.token_data * float * Types.length_unit_name> LENGTH
%token<Range.t * Types.token_data * string> CHAR
%token<Range.t * Types.token_data * string * bool * bool> STRING
%token<Range.t * Types.token_data * Types.input_position * string> POSITIONED_STRING

%token<Range.t * Types.token_data> SPACE BREAK
%token<Range.t * Types.token_data * string> MATHCHARS
%token<Range.t * Types.token_data * int> PRIMES
%token<Range.t * Types.token_data> SUBSCRIPT SUPERSCRIPT
%token<Range.t * Types.token_data * int> ITEM

%token <Range.t * Types.token_data * string> HEADER_REQUIRE HEADER_IMPORT

%token <Range.t * Types.token_data * Types.macro_name> BACKSLASH_MACRO PLUS_MACRO
%token <Range.t * Types.token_data * Types.module_name list * Types.macro_name>
  LONG_BACKSLASH_MACRO LONG_PLUS_MACRO

%token <Range.t * Types.token_data> EOI

%left  BINOP_BAR
%left  BINOP_AMP
%right BINOP_EQ BINOP_GT BINOP_LT
%right BINOP_HAT CONS
%right BINOP_PLUS
%left  BINOP_MINUS EXACT_MINUS
%right BINOP_TIMES EXACT_TIMES BINOP_DIVIDES MOD

%start main
%type<Types.rule_with_comment list * Types.rule_with_comment> main
%type<Types.rule_with_comment> main_lib headerelem
%type<Types.rule_with_comment> modexpr modexpr_app modexpr_bot mod_chain
%type<Types.rule_with_comment list> bind
%type<(Types.rule_with_comment * Types.column_config) list> bind_value
%type<Types.rule_with_comment> bind_val_rec_and bind_type_and
%type<(Types.rule_with_comment * Types.column_config) list> param_val
%type<(Types.rule_with_comment * Types.column_config) list> param_inline
%type<(Types.rule_with_comment * Types.column_config) list> param_block
%type<(Types.rule_with_comment * Types.column_config) list> scripts_param
%type<(Types.rule_with_comment * Types.column_config) list> param_type
%type<(Types.rule_with_comment * Types.column_config) list> variants
%type<(Types.rule_with_comment * Types.column_config) list> sig_annot
%type<Types.rule_with_comment> sigexpr sigexpr_bot
%type<Types.rule_with_comment> decl
%type<(Types.rule_with_comment * Types.column_config) list> quant
%type<Types.rule_with_comment> tyquant rowquant
%type<(Types.rule_with_comment * Types.column_config) list> param_unit
%type<Types.rule_with_comment> opt_params opt_param
%type<Types.rule_with_comment> macro_param
%type<Types.rule_with_comment> kind kind_row kind_base
%type<Types.rule_with_comment> typ
%type<(Types.rule_with_comment * Types.column_config) list> typ_prod
%type<Types.rule_with_comment> typ_app
%type<Types.rule_with_comment> typ_bot
%type<Types.rule_with_comment> typ_opt_dom typ_opt_dom_entry
%type<Types.rule_with_comment> typ_cmd_arg typ_record_elem
%type<Types.rule_with_comment> inline_macro_type block_macro_type typ_macro_arg
%type<(Types.rule_with_comment * Types.column_config) list> match_branches
%type<Types.rule_with_comment> expr
%type<(Types.rule_with_comment * Types.column_config) list> let_expr
%type<(Types.rule_with_comment * Types.column_config)> let_rec_and_expr
%type<Types.rule_with_comment> expr_overwrite expr_op expr_app expr_un expr_bot expr_bot_list expr_bot_record
%type<Types.rule_with_comment> record_field
%type<Types.rule_with_comment> branch
%type<Types.rule_with_comment> binop
%type<Types.rule_with_comment> pattern pattern_non_var pattern_cons pattern_non_var_cons
%type<Types.rule_with_comment> pattern_bot pattern_non_var_bot
%type<Types.rule_with_comment> inline
%type<Types.rule_with_comment list> inline_lst
%type<Types.rule_with_comment> inline_itemize_elem
%type<Types.rule_with_comment> inline_single
%type<(Types.rule_with_comment * Types.column_config) list> inline_elems
%type<Types.rule_with_comment> inline_elem_cmd
%type<(Types.rule_with_comment * Types.column_config) list> inline_elem_text
%type<(Types.token_data * string)> inline_char
%type<Types.rule_with_comment> block
%type<Types.rule_with_comment> block_elem
%type<Types.rule_with_comment> math math_single math_elem math_group math_bot
%type<Types.rule_with_comment list> math_lst
%type<Types.rule_with_comment list> math_cmd_arg
%type<(Types.rule_with_comment * Types.column_config) list> macroargs
%type<Types.rule_with_comment> macronarg
%type<Types.rule_with_comment list> cmd_arg_expr
%type<Types.rule_with_comment option> expr_opts
%type<Types.rule_with_comment> expr_opt_entry
%type<Types.rule_with_comment list> cmd_args_text
%type<Types.rule_with_comment> cmd_arg_text
%type<Types.rule_with_comment> backslash_cmd plus_cmd


%%

optterm_list(sep, X):
  |   { [] }
  | x=X; sep?
      { [x] }
  | x=X; sep; xs=optterm_nonempty_list(sep, X)
      { x :: xs }
;
optterm_nonempty_list(sep, X):
  | x=X; sep?
      { [x] }
  | x=X; sep; xs=optterm_nonempty_list(sep, X)
      { x :: xs }
;
%inline bound_identifier:
  | ident=LOWER
      {
        let (_, d, s) = ident in
        Types.with_comment (Raw s) d
      }
  | opn_tok=L_PAREN; ident=binop; cls_tok=R_PAREN
      {
        let (_, opn_d) = opn_tok in
        let (_, cls_d) = cls_tok in
        Types.with_comment_paren opn_d (Paren ("(", ident, ")")) cls_d
      }
;
main:
  | header=list(headerelem); lib_rwc=main_lib; EOI
      { (header, lib_rwc) }
  | header=list(headerelem); expr_rwc=expr; EOI
      { (header, expr_rwc) }
  | tok=EOI
      { let (rng,_) = tok in raise (ParseErrorDetail(rng, "empty input")) }
;
main_lib:
  | module_tok=MODULE; modident=UPPER; sig_opt=option(sig_annot); exact_eq_tok=EXACT_EQ; struct_tok=STRUCT; binds=list(bind); end_tok=END
      {
        let (_, d, s) = modident in
        let lst = [
            (Types.with_comment (Raw "module") (second module_tok), column_config_default |> set_is_break false);
            (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
          ]
        in
        let lst =
          match sig_opt with
          | Some(lst_tmp) -> List.append lst lst_tmp
          | None -> lst
        in
        let lst = List.append lst [
            Types.with_comment (Raw "=") (second exact_eq_tok), column_config_default |> set_is_break false;
            Types.with_comment (Raw "struct") (second struct_tok), column_config_default |> set_is_break true;
            Types.with_comment_none (AST (Types.with_comment_none (Column (List.map (fun rwc -> (rwc, column_config_default |> set_is_break true)) (List.concat binds))))), column_config_default |> set_is_break true;
            Types.with_comment (Raw "end") (second end_tok), column_config_default;
          ]
        in
        Types.with_comment_none (Column lst)
      }
;
headerelem:
  | content=HEADER_REQUIRE { let (_, d, s) = content in Types.with_comment (Raw ("@require: " ^ s)) d }
  | content=HEADER_IMPORT  { let (_, d, s) = content in Types.with_comment (Raw ("@import: " ^ s)) d }
;
modexpr:
  | fun_tok=FUN; opn_tok=L_PAREN; modident=UPPER; colon_tok=COLON; rwc_sig=sigexpr; cls_tok=R_PAREN; arrow_tok=ARROW; rwc_mod=modexpr {
      let (_,d,s) = modident in
      let paren =
        Types.with_comment_paren (second opn_tok) (Paren ("(", Types.with_comment_none (Column [
          (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
          (Types.with_comment (Raw ":") (second colon_tok), column_config_default);
          (rwc_sig, column_config_default);
        ]), ")")) (second cls_tok)
      in
      Types.with_comment_none (Column [
        (Types.with_comment (Raw "fun") (second fun_tok), column_config_default |> set_is_break false);
        (paren, column_config_default |> set_is_break false);
        (Types.with_comment (Raw "->") (second arrow_tok), column_config_default |> set_is_break false);
        (rwc_mod, column_config_default);
      ])
  }
  | modident=UPPER; coerce_tok=COERCE; rwc_sig=sigexpr {
      let (_,d,s) = modident in
      Types.with_comment_none (Column [
        (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
        (Types.with_comment (Raw ":>") (second coerce_tok), column_config_default);
        (rwc_sig, column_config_default);
      ])
    }
  | rwc_mod=modexpr_app { rwc_mod }
;
modexpr_app:
  | rmodchain1=mod_chain; rmodchain2=mod_chain
      {
        Types.with_comment_none (Column [
          (rmodchain1, column_config_default);
          (rmodchain2, column_config_default);
        ])
      }
  | rwc_mod=modexpr_bot { rwc_mod }
;
modexpr_bot:
  | rmodchain=mod_chain { rmodchain }
  | str_tok=STRUCT; utbinds=list(bind); end_tok=END
      {
        let rwc = Types.with_comment_none (Column (List.map (fun r -> (r, column_config_default)) (List.concat utbinds))) in
        Types.with_comment_none (Column [
          (Types.with_comment (Raw "struct") (second str_tok), column_config_default |> set_is_break false);
          (rwc, column_config_default |> set_is_break false);
          (Types.with_comment (Raw "end") (second end_tok), column_config_default);
        ])
      }
;
mod_chain:
  | modident=UPPER
      { let (_,d,s) = modident in Types.with_comment (Raw s) d }
  | modpath=LONG_UPPER
      {
        let (_,d,ms,s) = modpath in
        let s = (Types.lst_join "." ms) ^ "." ^ s in
        Types.with_comment (Raw s) d
      }
;

bind:
  | val_tok=VAL; bind_value=bind_value {
    [
      Types.with_comment_none (Column (
        (Types.with_comment (Raw "val") (second val_tok), column_config_default |> set_is_break false) :: bind_value
      ))
    ]
  }
  (* and の衝突があるため、 let rec と type は少し違う *)
  | val_tok=VAL; rec_tok=REC; param_val=param_val; eq_tok=EXACT_EQ; rwc_expr=expr ands=list(bind_val_rec_and) {
    let lst = [
      (Types.with_comment (Raw "val") (second val_tok), column_config_default |> set_is_break false);
      (Types.with_comment (Raw "rec") (second rec_tok), column_config_default |> set_is_break false);
    ] in
    let lst = List.append lst param_val in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default);
      (rwc_expr, column_config_default);
    ] in
    (Types.with_comment_none (Column lst)) :: ands
  }
  | type_tok=TYPE; param_type=param_type; eq_tok=EXACT_EQ; rwc_typ=typ ands=list(bind_type_and) {
    let lst = [
      (Types.with_comment (Raw "type") (second type_tok), column_config_default |> set_is_break false);
    ] in
    let lst = List.append lst param_type in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default);
      (rwc_typ, column_config_default);
    ] in
    (Types.with_comment_none (Column lst)) :: ands
  }
  | type_tok=TYPE; param_type=param_type; eq_tok=EXACT_EQ; bar_tok_opt=BAR?; variants=variants ands=list(bind_type_and) {
    let lst = [
      (Types.with_comment (Raw "type") (second type_tok), column_config_default |> set_is_break false);
    ] in
    let lst = List.append lst param_type in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default);
    ] in
    let lst =
      match bar_tok_opt with
      | Some((_, bar_data)) -> List.append lst [
          (Types.with_comment (Raw " ") bar_data, column_config_default |> set_is_break false);
        ]
      | None -> List.append lst [(Types.with_comment_none (Raw " "), column_config_default |> set_is_break false)]
    in
    let lst = List.append lst variants in
    (Types.with_comment_none (Column lst)) :: ands
  }
  | module_tok=MODULE; modident=UPPER; rwc_sig_opt=option(sig_annot); exact_eq_tok=EXACT_EQ; rwc_mod=modexpr {
    let (_, d, s) = modident in
    let lst = [
        (Types.with_comment (Raw "module") (second module_tok), column_config_default |> set_is_break false);
        (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
      ]
    in
    let lst =
      match rwc_sig_opt with
      | Some(lst_tmp) -> List.append lst lst_tmp
      | None -> lst
    in
    let lst = List.append lst [(Types.with_comment (Raw "=") (second exact_eq_tok), column_config_default |> set_is_break false)] in
    let lst = List.append lst [(rwc_mod, column_config_default)] in
    [Types.with_comment_none (Column lst)]
  }
  | signature_tok=SIGNATURE; sigident=UPPER; eq_tok=EXACT_EQ; rwc_sig=sigexpr {
    let (_, sig_d, sig_s) = sigident in
    [
      Types.with_comment_none (Column ([
        (Types.with_comment (Raw "sigunature") (second signature_tok), column_config_default |> set_is_break false);
        (Types.with_comment (Raw sig_s) sig_d, column_config_default |> set_is_break false);
        (Types.with_comment (Raw "=") (second eq_tok), column_config_default);
        (Types.with_comment_none (AST (rwc_sig)), column_config_default);
      ]))
    ]
  }
  | include_tok=INCLUDE; rwc_mod=modexpr {
    [
      Types.with_comment_none (Column ([
        (Types.with_comment (Raw "include") (second include_tok), column_config_default |> set_is_break false);
        (rwc_mod, column_config_default);
      ]))
    ]
  }
;
bind_value: /* (rule_with_comment * column_config) list */
  | param_val=param_val; eq_tok=EXACT_EQ; rwc_expr=expr {
    List.append param_val [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default |> set_is_break false);
      (rwc_expr, column_config_default);
    ]
  }
  | tilde_tok=EXACT_TILDE; param_val=param_val; eq_tok=EXACT_EQ; rwc_expr=expr {
    let lst = [
      (Types.with_comment (Raw "~") (second tilde_tok), column_config_default |> set_is_break false |> set_space_size 0);
    ] in
    let lst = List.append lst param_val in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default |> set_is_break false);
      (rwc_expr, column_config_default);
    ] in
    lst
  }
  | persistent_tok=PERSISTENT; param_val=param_val; eq_tok=EXACT_EQ; rwc_expr=expr {
    let lst = [
      (Types.with_comment (Raw "persistent") (second persistent_tok), column_config_default |> set_is_break false);
    ] in
    let lst = List.append lst param_val in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default |> set_is_break false);
      (rwc_expr, column_config_default);
    ] in
    lst
  }
  | mutable_tok=MUTABLE; ident_tok=LOWER; arrow_tok=REVERSED_ARROW; rwc_expr=expr {
    let (_, ident_d, ident_s) = ident_tok in
    [
      (Types.with_comment (Raw "mutable") (second mutable_tok), column_config_default |> set_is_break false);
      (Types.with_comment (Raw ident_s) ident_d, column_config_default |> set_is_break false);
      (Types.with_comment (Raw "<-") (second arrow_tok), column_config_default);
      (rwc_expr, column_config_default);
    ]
  }
  |inline_tok=INLINE; param_inline=param_inline; eq_tok=EXACT_EQ; rwc_expr=expr {
    let lst = [
      (Types.with_comment (Raw "inline") (second inline_tok), column_config_default |> set_is_break false);
    ] in
    let lst = List.append lst param_inline in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default |> set_is_break false);
      (rwc_expr, column_config_default);
    ] in
    lst
  }
  | block_tok=BLOCK; param_block=param_block; eq_tok=EXACT_EQ; rwc_expr=expr {
    let lst = [
      (Types.with_comment (Raw "block") (second block_tok), column_config_default |> set_is_break false);
    ] in
    let lst = List.append lst param_block in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default |> set_is_break false);
      (rwc_expr, column_config_default);
    ] in
    lst
  }
  | math_tok=MATH; param_math=param_math; eq_tok=EXACT_EQ; rwc_expr=expr {
    let lst = [
      (Types.with_comment (Raw "math") (second math_tok), column_config_default |> set_is_break false);
    ] in
    let lst = List.append lst param_math in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default |> set_is_break false);
      (rwc_expr, column_config_default);
    ] in
    lst
  }
;
bind_val_rec_and:
  | and_tok=AND; param_val=param_val; eq_tok=EXACT_EQ; rwc_expr=expr {
    let lst = [
      (Types.with_comment (Raw "and") (second and_tok), column_config_default |> set_is_break false);
    ] in
    let lst = List.append lst param_val in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default);
      (rwc_expr, column_config_default);
    ] in
    Types.with_comment_none (Column lst)
  }
;
bind_type_and:
  | and_tok=AND; param_type=param_type; eq_tok=EXACT_EQ; rwc_typ=typ {
    let lst = [
      (Types.with_comment (Raw "and") (second and_tok), column_config_default |> set_is_break false);
    ] in
    let lst = List.append lst param_type in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default);
      (rwc_typ, column_config_default);
    ] in
    Types.with_comment_none (Column lst)
  }
  | and_tok=AND; param_type=param_type; eq_tok=EXACT_EQ; bar_tok_opt=BAR?; variants=variants {
    let lst = [
      (Types.with_comment (Raw "and") (second and_tok), column_config_default |> set_is_break false);
    ] in
    let lst = List.append lst param_type in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default);
    ] in
    let lst =
      match bar_tok_opt with
      | Some((_, bar_data)) -> List.append lst [
          (Types.with_comment (Raw " ") bar_data, column_config_default |> set_is_break false);
        ]
      | None -> List.append lst [(Types.with_comment_none (Raw " "), column_config_default |> set_is_break false)]
    in
    let lst = List.append lst variants in
    Types.with_comment_none (Column lst)
  }
;
param_val: /* (rule_with_comment * column_config) list */
  | ident=bound_identifier; param_units=list(param_unit) {
    (ident, column_config_default |> set_is_break false) :: (List.concat param_units)
  }
;
param_inline:
  | ident_ctx=LOWER; cs=BACKSLASH_CMD; param_units=list(param_unit) {
    let (_, ctx_d, ctx_s) = ident_ctx in
    let (_, cs_d, cs_s) = ident_ctx in
    (Types.with_comment (Raw ctx_s) ctx_d, column_config_default |> set_is_break false) ::
      (Types.with_comment (Raw ("\\" ^ cs_s)) cs_d, column_config_default |> set_is_break false) ::
        (List.concat param_units)
  }
  | cs=BACKSLASH_CMD; param_units=list(param_unit) {
    let (_, cs_d, cs_s) = cs in
    (Types.with_comment (Raw ("\\" ^ cs_s)) cs_d, column_config_default |> set_is_break false)
      :: (List.concat param_units)
  }
  | cs=BACKSLASH_MACRO; macro_params=list(macro_param) {
    let (_, cs_d, cs_s) = cs in
    let f r = (r, column_config_default) in
    (Types.with_comment (Raw ("\\" ^ cs_s ^ "@")) cs_d, column_config_default |> set_is_break false) :: (List.map f macro_params)
  }
;
param_block:
  | ident_ctx=LOWER; cs=PLUS_CMD; param_units=list(param_unit) {
    let (_, ctx_d, ctx_s) = ident_ctx in
    let (_, cs_d, cs_s) = ident_ctx in
    let lst =
      (Types.with_comment (Raw ctx_s) ctx_d, column_config_default |> set_is_break false) ::
        (Types.with_comment (Raw ("+" ^ cs_s)) cs_d, column_config_default |> set_is_break false) ::
          (List.concat param_units)
    in
    lst
  }
  | cs=PLUS_CMD; param_units=list(param_unit) {
    let (_, cs_d, cs_s) = cs in
    let lst =
      (Types.with_comment (Raw ("+" ^ cs_s)) cs_d, column_config_default |> set_is_break false)
        :: (List.concat param_units)
    in
    lst
  }
  | cs=PLUS_MACRO; macro_params=list(macro_param) {
    let (_, cs_d, cs_s) = cs in
    let f r = (r, column_config_default) in
    (Types.with_comment (Raw ("+" ^ cs_s ^ "@")) cs_d, column_config_default |> set_is_break false) :: (List.map f macro_params)
  }
;
param_math:
  | ident_ctx=LOWER; cs=BACKSLASH_CMD; param_units=list(param_unit); scripts_param_opt=option(scripts_param) {
    let (_, ctx_d, ctx_s) = ident_ctx in
    let (_, cs_d, cs_s) = cs in
    let lst =
      (Types.with_comment (Raw ctx_s) ctx_d, column_config_default |> set_is_break false) ::
        (Types.with_comment (Raw ("\\" ^ cs_s)) cs_d, column_config_default |> set_is_break false) ::
        (List.concat param_units)
    in
    match scripts_param_opt with
    | Some(scripts_param_lst) -> List.append lst scripts_param_lst
    | None -> lst
  }
;
scripts_param:
  | with_tok=WITH; sub=LOWER; sup=LOWER {
    let (_, with_d) = with_tok in
    let (_, sub_d, sub_s) = sub in
    let (_, sup_d, sup_s) = sup in
    [
      (Types.with_comment (Raw "with") with_d, column_config_default |> set_is_break false);
      (Types.with_comment (Raw sub_s) sub_d, column_config_default |> set_is_break false);
      (Types.with_comment (Raw sup_s) sup_d, column_config_default |> set_is_break false);
    ]
  }
;
param_type: /* (rule_with_comment * column_config) list */
  | tyident=LOWER; tyvars=list(TYPEVAR) {
    let (_, ty_d, ty_s) = tyident in
    let lst = List.map (fun (_, tyvar_d, tyvar_s) ->
      (Types.with_comment (Raw ("'" ^ tyvar_s)) tyvar_d, column_config_default |> set_is_break false)
    ) tyvars in
    (Types.with_comment (Raw ty_s) ty_d, column_config_default |> set_is_break false) :: lst
  }
;
variants: /* (rule_with_comment * column_config) list */
  | bar_tok=BAR; ctor=UPPER; of_tok=OF; mnty=typ; variants=variants {
    let (_, ctor_d, ctor_s) = ctor in
      (Types.with_comment (Raw "|") (second bar_tok), column_config_default |> set_is_break false) ::
        ((Types.with_comment (Raw ctor_s) ctor_d, column_config_default |> set_is_break false) ::
          (Types.with_comment (Raw "of") (second of_tok), column_config_default |> set_is_break false) ::
            (mnty, column_config_default) :: variants)
  }
  | bar_tok=BAR; ctor=UPPER; of_tok=OF; mnty=typ {
    let (_, ctor_d, ctor_s) = ctor in
    [
      (Types.with_comment (Raw "|") (second bar_tok), column_config_default |> set_is_break false);
      (Types.with_comment (Raw ctor_s) ctor_d, column_config_default |> set_is_break false);
      (Types.with_comment (Raw "of") (second of_tok), column_config_default |> set_is_break false);
      (mnty, column_config_default);
    ]
  }
  | bar_tok=BAR; ctor=UPPER; variants=variants {
    let (_, ctor_d, ctor_s) = ctor in
      (Types.with_comment (Raw "|") (second bar_tok), column_config_default |> set_is_break false) ::
        ((Types.with_comment (Raw ctor_s) ctor_d, column_config_default) :: variants)
  }
  | bar_tok=BAR; ctor=UPPER {
    let (_, ctor_d, ctor_s) = ctor in
    [
      (Types.with_comment (Raw "|") (second bar_tok), column_config_default |> set_is_break false);
      (Types.with_comment (Raw ctor_s) ctor_d, column_config_default);
    ]
  }
;
sig_annot: /* likes: `:> sig`, (rule_with_comment, column_config) list */
  | coerce_tok=COERCE; rwc_sig=sigexpr {
    [
      (Types.with_comment (Raw ":>") (second coerce_tok), column_config_default |> set_is_break false);
      (rwc_sig, column_config_default);
    ]
  }
;
sigexpr:
  | rwc_sig=sigexpr_bot; with_tok=WITH; type_tok=TYPE; tybinds=param_type
    {
      let lst = [
        (rwc_sig, column_config_default);
        (Types.with_comment (Raw "with") (second with_tok), column_config_default);
        (Types.with_comment (Raw "type") (second type_tok), column_config_default);
      ] in
      Types.with_comment_none (Column (List.append lst tybinds))
    }
  | rwc_sig=sigexpr_bot; with_tok=WITH; rmodchain=mod_chain; type_tok=TYPE; tybinds=param_type
    {
      let lst = [
        (rwc_sig, column_config_default);
        (Types.with_comment (Raw "with") (second with_tok), column_config_default);
        (rmodchain, column_config_default);
        (Types.with_comment (Raw "type") (second type_tok), column_config_default);
      ] in
      Types.with_comment_none (Column (List.append lst tybinds))
    }
  | opn_tok=L_PAREN; modident=UPPER; colon_tok=COLON; rwc_sig1=sigexpr; cls_tok=R_PAREN; arrow_tok=ARROW; rwc_sig2=sigexpr
    {
      let (_,d,s) = modident in
      Types.with_comment_none (Column [
        (Types.with_comment_paren (second opn_tok) (Paren ("(", (Types.with_comment_none (Column [
          (Types.with_comment (Raw s) d, column_config_default);
          (Types.with_comment (Raw ":") (second colon_tok), column_config_default);
          (rwc_sig1, column_config_default);
        ])), ")")) (second cls_tok), column_config_default);
        (Types.with_comment (Raw "->") (second arrow_tok), column_config_default);
        (rwc_sig2, column_config_default);
      ])
    }
  | rwc_sig=sigexpr_bot { rwc_sig }
;
sigexpr_bot:
  | sigident=UPPER
    {
      let (_,d,s) = sigident in
      Types.with_comment (Raw s) d
    }
  | sigpath=LONG_UPPER
    {
      let (_,d,ms,s) = sigpath in
      let s = (Types.lst_join "." ms) ^ "." ^ s in
      Types.with_comment (Raw s) d
    }
  | sig_tok=SIG; decls=list(decl); end_tok=END
    {
      let decls =
        Types.with_comment_none (Column (List.map (fun r -> (r, column_config_default |> set_is_break true)) decls))
      in
      let lst = [
          Types.with_comment (Raw "sig") (second sig_tok), column_config_default;
          Types.with_comment_none (AST decls), column_config_default |> set_is_break true;
          Types.with_comment (Raw "end") (second end_tok), column_config_default
        ]
      in
      Types.with_comment_none (Column lst)
    }
;
decl:
  | val_tok=VAL; ident=bound_identifier; mnquant=quant; colon_tok=COLON; mnty=typ
    {
      let lst = [
        (Types.with_comment (Raw "val") (second val_tok), column_config_default);
        (ident, column_config_default);
      ] in
      let lst = List.append lst mnquant in
      let lst = List.append lst [
        (Types.with_comment (Raw ":") (second colon_tok), column_config_default);
        (mnty, column_config_default)
      ] in
      Types.with_comment_none (Column lst)
    }
  | val_tok=VAL; tilde_tok=EXACT_TILDE; ident=bound_identifier; mnquant=quant; colon_tok=COLON; mnty=typ
    {
      let lst = [
        (Types.with_comment (Raw "val") (second val_tok), column_config_default);
        (Types.with_comment (Raw "~") (second tilde_tok), column_config_default |> set_is_break false |> set_space_size 0);
        (ident, column_config_default);
      ] in
      let lst = List.append lst mnquant in
      let lst = List.append lst [
        (Types.with_comment (Raw ":") (second colon_tok), column_config_default);
        (mnty, column_config_default)
      ] in
      Types.with_comment_none (Column lst)
    }
  | val_tok=VAL; persistent_tok=PERSISTENT; tilde_tok=EXACT_TILDE; ident=bound_identifier; mnquant=quant; colon_tok=COLON; mnty=typ
    {
      let lst = [
        (Types.with_comment (Raw "val") (second val_tok), column_config_default);
        (Types.with_comment (Raw "persistent") (second persistent_tok), column_config_default);
        (Types.with_comment (Raw "~") (second tilde_tok), column_config_default |> set_is_break false |> set_space_size 0);
        (ident, column_config_default);
      ] in
      let lst = List.append lst mnquant in
      let lst = List.append lst [
        (Types.with_comment (Raw ":") (second colon_tok), column_config_default);
        (mnty, column_config_default)
      ] in
      Types.with_comment_none (Column lst)
    }
  | val_tok=VAL; cs=BACKSLASH_CMD; mnquant=quant; colon_tok=COLON; mnty=typ
    {
      let (_,d,s) = cs in
      let lst = [
        (Types.with_comment (Raw "val") (second val_tok), column_config_default);
        (Types.with_comment (Raw ("\\" ^ s)) d, column_config_default);
      ] in
      let lst = List.append lst mnquant in
      let lst = List.append lst [
        (Types.with_comment (Raw ":") (second colon_tok), column_config_default);
        (mnty, column_config_default)
      ] in
      Types.with_comment_none (Column lst)
    }
  | val_tok=VAL; cs=PLUS_CMD; mnquant=quant; colon_tok=COLON; mnty=typ
    {
      let (_,d,s) = cs in
      let lst = [
        (Types.with_comment (Raw "val") (second val_tok), column_config_default);
        (Types.with_comment (Raw ("+" ^ s)) d, column_config_default);
      ] in
      let lst = List.append lst mnquant in
      let lst = List.append lst [
        (Types.with_comment (Raw ":") (second colon_tok), column_config_default);
        (mnty, column_config_default)
      ] in
      Types.with_comment_none (Column lst)
    }
  | val_tok=VAL; cs=BACKSLASH_MACRO; colon_tok=COLON; mnmacty=inline_macro_type
    {
      let (_,d,s) = cs in
      Types.with_comment_none (Column [
        (Types.with_comment (Raw "val") (second val_tok), column_config_default);
        (Types.with_comment (Raw ("\\" ^ s ^ "@")) d, column_config_default);
        (Types.with_comment (Raw ":") (second colon_tok), column_config_default);
        (mnmacty, column_config_default)
      ])
    }
  | val_tok=VAL; cs=PLUS_MACRO; colon_tok=COLON; mnmacty=block_macro_type
    {
      let (_,d,s) = cs in
      Types.with_comment_none (Column [
        (Types.with_comment (Raw "val") (second val_tok), column_config_default);
        (Types.with_comment (Raw ("+" ^ s ^ "@")) d, column_config_default);
        (Types.with_comment (Raw ":") (second colon_tok), column_config_default);
        (mnmacty, column_config_default)
      ])
    }
  | type_tok=TYPE; tyident=LOWER; cons_tok=CONS; mnkd=kind
    {
      let (_,d,s) = tyident in
      Types.with_comment_none (Column [
        (Types.with_comment (Raw "type") (second type_tok), column_config_default);
        (Types.with_comment (Raw s) d, column_config_default);
        (Types.with_comment (Raw "::") (second cons_tok), column_config_default);
        (mnkd, column_config_default)
      ])
    }
  | type_tok=TYPE; typebind=param_type
    {
      Types.with_comment_none (Column (
        (Types.with_comment (Raw "type") (second type_tok), column_config_default) :: typebind
      ))
    }
  | module_tok=MODULE; modident=UPPER; colon_tok=COLON; rwc_sig=sigexpr
    {
      let (_,d,s) = modident in
      Types.with_comment_none (Column [
        (Types.with_comment (Raw "module") (second module_tok), column_config_default);
        (Types.with_comment (Raw s) d, column_config_default);
        (Types.with_comment (Raw ":") (second colon_tok), column_config_default);
        (rwc_sig, column_config_default)
      ])
    }
  | sig_tok=SIGNATURE; sigident=UPPER; eq_tok=EXACT_EQ; rwc_sig=sigexpr
    {
      let (_,d,s) = sigident in
      Types.with_comment_none (Column [
        (Types.with_comment (Raw "signature") (second sig_tok), column_config_default);
        (Types.with_comment (Raw s) d, column_config_default);
        (Types.with_comment (Raw "=") (second eq_tok), column_config_default);
        (rwc_sig, column_config_default)
      ])
    }
  | include_tok=INCLUDE; rwc_sig=sigexpr
    {
      Types.with_comment_none (Column [
        (Types.with_comment (Raw "include") (second include_tok), column_config_default);
        (rwc_sig, column_config_default)
      ])
    }
;
quant:
  | tyquants=list(tyquant); rowquants=list(rowquant)
      { List.map (fun rwc -> (rwc, column_config_default)) (List.append tyquants rowquants) }
;
tyquant:
  | tyvar=TYPEVAR { let (_,d,s) = tyvar in Types.with_comment (Raw ("'" ^ s)) d }
;
rowquant:
  | opn_tok=L_PAREN; rowvar=ROWVAR; cons_tok=CONS; mnrbkd=kind_row; cls_tok=R_PAREN  {
    let (_,d,s) = rowvar in
    Types.with_comment_paren (second opn_tok) (Paren ("(", Types.with_comment_none (Column [
      (Types.with_comment (Raw ("?'" ^ s)) d, column_config_default);
      (Types.with_comment (Raw "::") (second cons_tok), column_config_default);
      (mnrbkd, column_config_default)
    ]), ")")) (second cls_tok)
  }
;

param_unit: /* 引数 */
  | opts_opt=option(opt_params); rwc_pat=pattern_bot
    {
      match opts_opt with
      | Some(opts) ->
        [
          (opts, column_config_default |> set_is_break false);
          (rwc_pat, column_config_default)
        ]
      | None -> [(rwc_pat, column_config_default)]
    }
  | opts_opt=option(opt_params); opn_tok=L_PAREN; rwc_pat=pattern; colon_tok=COLON; mnty=typ; cls_tok=R_PAREN /* 型注釈 */
    {
      let rwc =
        Types.with_comment_paren (second opn_tok) (Paren ("(", (Types.with_comment_none (Column [
          (rwc_pat, column_config_default |> set_is_break false);
          (Types.with_comment (Raw ":") (second colon_tok), column_config_default |> set_is_break false);
          (mnty, column_config_default);
        ])), ")")) (second cls_tok)
      in
      match opts_opt with
      | Some(opts) ->
        [
          (opts, column_config_default |> set_is_break false);
          (rwc, column_config_default)
        ]
      | None -> [(rwc_pat, column_config_default)]
    }
;
opt_params:
  | q_tok=QUESTION; L_PAREN; opts=optterm_nonempty_list(COMMA, opt_param); cls_tok=R_PAREN {
    Types.with_comment_paren (second q_tok) (Paren("?(", (Types.with_comment_none (List (",", opts))), ")")) (second cls_tok)
  }
;
opt_param:
  | rlabel=LOWER; eq_tok=EXACT_EQ; ident=LOWER {
    let (_,d1,s1) = rlabel in
    let (_,d2,s2) = ident in
    Types.with_comment_none (Column [
      (Types.with_comment (Raw s1) d1, column_config_default |> set_is_break false);
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default |> set_is_break false);
      (Types.with_comment (Raw s2) d2, column_config_default |> set_is_break false);
    ])
  }
/* TODO: add more patterns here */
;

macro_param:
  | var=LOWER { let (_,d,s) = var in Types.with_comment (Raw s) d }
  | tok=EXACT_TILDE; var=LOWER {
    let (_,d) = tok in
    let (_,d,s) = var in
    Types.with_comment_none (Column [
      (Types.with_comment (Raw "~") d, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw s) d, column_config_default);
    ])
  }
;
kind:
  | bkd=kind_base; arrow_tok=ARROW; kd=kind {
    Types.with_comment_none (Column [
      (bkd, column_config_default);
      (Types.with_comment (Raw "->") (second arrow_tok), column_config_default);
      (kd, column_config_default)
    ])
  }
  | bkd=kind_base { bkd }
;
kind_base:
  | kdident=LOWER { let (_,d,s) = kdident in Types.with_comment (Raw s) d }
;
kind_row:
  | opn_tok=L_RECORD; rlabels=optterm_nonempty_list(COMMA, LOWER); cls_tok=R_RECORD {
    Types.with_comment_paren (second opn_tok)
      (Paren ("(|", (Types.with_comment_none (List (",", List.map (fun (_,d,s) -> Types.with_comment (Raw s) d) rlabels))), "|)"))
      (second cls_tok)
  }
;
typ: /* rule_with_comment */
  | mnty1=typ_prod; arrow_tok=ARROW; mnty2=typ {
    let lst = List.append mnty1 [(Types.with_comment (Raw "->") (second arrow_tok), column_config_default)] in
    let lst = List.append lst [(mnty2, column_config_default)] in
    Types.with_comment_none (Column lst)
  }
  | rmnopts=typ_opt_dom; mnty1=typ_prod; arrow_tok=ARROW; mnty2=typ {
    let lst = (rmnopts, column_config_default) :: mnty1 in
    let lst = List.append lst [((Types.with_comment (Raw "->") (second arrow_tok)), column_config_default)] in
    let lst = List.append lst [(mnty2, column_config_default)] in
    Types.with_comment_none (Column lst)
  }
  | rwcty=typ_prod { Types.with_comment_none (Column rwcty) }
;
typ_prod: /* tuple type as likes : `a * b * c * d`, (rule_with_comment * column_config) list */
  | typ_app=typ_app {
    [(typ_app, column_config_default)]
  }
  | typ_app=typ_app; times_tok=EXACT_TIMES; prod=typ_prod {
    let (_, d) = times_tok in
    (typ_app, column_config_default |> set_is_break false) ::
      (Types.with_comment (Raw "*") d, column_config_default) :: prod
  }
;
typ_app:
  | tyident=LOWER; mntys=nonempty_list(typ_bot) /* likes: `list a 'b c` */
      {
        let c = column_config_default |> set_is_break false in
        let (_, ty_d, ty_s) = tyident in
        let tyident = Types.with_comment (Raw ty_s) ty_d in
        let clmn_mntys = List.map (fun rwc -> (rwc, c)) mntys in
        Types.with_comment_none (Column ((tyident, c)  :: clmn_mntys))
      }
  | long_tyident=LONG_LOWER; mntys=nonempty_list(typ_bot) /* likes: `X.t a b c` */
      {
        let c = column_config_default |> set_is_break false in
        let (_, ty_d, module_name_lst, ty_s) = long_tyident in
        let s = (lst_join "." module_name_lst) ^ "." ^ ty_s in
        let tyident = Types.with_comment (Raw s) ty_d in
        let clmn_mntys = List.map (fun rwc -> (rwc, c)) mntys in
        Types.with_comment_none (Column ((tyident,c) :: clmn_mntys))
      }
  | tok=INLINE; opn_tok=L_SQUARE; typ_cmd_arg_lst=optterm_list(COMMA, typ_cmd_arg); cls_tok=R_SQUARE /* likes: `inline [string, inline-text]` */
      {
        let c = column_config_default |> set_is_break false in
        let lst = Types.with_comment_none (List (",", typ_cmd_arg_lst)) in
        let paren = Types.with_comment_paren (second opn_tok) (Paren ("[", lst, "]")) (second cls_tok) in
        Types.with_comment_none (Column ([
          (Types.with_comment (Raw "inline") (second tok), c);
          (paren, column_config_default);
        ]))
      }
  | tok=BLOCK; opn_tok=L_SQUARE; typ_cmd_lst=optterm_list(COMMA, typ_cmd_arg); cls_tok=R_SQUARE
      {
        let c = column_config_default |> set_is_break false in
        let lst = Types.with_comment_none (List (",", typ_cmd_lst)) in
        let paren = Types.with_comment_paren (second opn_tok) (Paren ("[", lst, "]")) (second cls_tok) in
        Types.with_comment_none (Column ([
          (Types.with_comment (Raw "block") (second tok), c);
          (paren, column_config_default);
        ]))
      }
  | tok=MATH; opn_tok=L_SQUARE; typ_cmd_lst=optterm_list(COMMA, typ_cmd_arg); cls_tok=R_SQUARE
      {
        let c = column_config_default |> set_is_break false in
        let lst = Types.with_comment_none (List (",", typ_cmd_lst)) in
        let paren = Types.with_comment_paren (second opn_tok) (Paren ("[", lst, "]")) (second cls_tok) in
        Types.with_comment_none (Column ([
          (Types.with_comment (Raw "math") (second tok), c);
          (paren, column_config_default);
        ]))
      }
  | mnty=typ_bot
      { mnty }
;
typ_bot:
  | tyident=LOWER
    {
      let (_, d, s) = tyident in
      Types.with_comment (Raw s) d
    }
  | long_tyident=LONG_LOWER
    {
      let (_, d, module_name_lst, ty_s) = long_tyident in
        let s = (lst_join "." module_name_lst) ^ "." ^ ty_s in
      Types.with_comment (Raw s) d
    }
  | tyvar=TYPEVAR /* likes: `'a` */
    {
      let (_, d, s) = tyvar in
      Types.with_comment (Raw ("'" ^ s)) d
    }
  | opn_tok=L_RECORD; fields=optterm_nonempty_list(COMMA, typ_record_elem); cls_tok=R_RECORD
    {
      let rwc_lst = Types.with_comment_none (List (",", fields)) in
      Types.with_comment_paren (second opn_tok) (Paren("(|", rwc_lst, "|)")) (second cls_tok)
    }
  | opn_tok=L_RECORD; fields=optterm_nonempty_list(COMMA, typ_record_elem); bar_tok=BAR; rv=ROWVAR; cls_tok=R_RECORD /* record type as likes: `(| x = int, y = string | ?'t |)` */
    {
      let (_, rv_d, rv_s) = rv in
      let rwc_lst = Types.with_comment_none (List (",", fields)) in
      let clmn_lst = [
        (rwc_lst, column_config_default);
        (Types.with_comment (Raw "|") (second bar_tok), column_config_default);
        (Types.with_comment (Raw ("?'" ^ rv_s)) rv_d, column_config_default);
      ] in
      let paren = Paren ("(|", Types.with_comment_none (Column clmn_lst), "|)") in
      Types.with_comment_paren (second opn_tok) paren (second cls_tok)
    }
  | opn_tok=L_PAREN; mnty=typ; cls_tok=R_PAREN
      { Types.with_comment_paren (second opn_tok) (Paren ("(", mnty, ")")) (second cls_tok) }
;
typ_opt_dom:
  | q_tok=QUESTION; L_PAREN; mnopts=optterm_nonempty_list(COMMA, typ_opt_dom_entry); cls_tok=R_PAREN
    {
      let rwc_lst = Types.with_comment_none (List (",", mnopts)) in
      let paren = Paren ("?(", rwc_lst, ")") in
      Types.with_comment_paren (second q_tok) paren (second cls_tok)
    }
  | q_tok=QUESTION; L_PAREN; mnopts=optterm_nonempty_list(COMMA, typ_opt_dom_entry); bar_tok=BAR; rv=ROWVAR cls_tok=R_PAREN
  /* optional argument as likes: `( x : int, y : string | ?'t )` */
    {
      let (_, rv_d, rv_s) = rv in
      let rwc_lst = Types.with_comment_none (List (",", mnopts)) in
      let clmn_lst = [
        (rwc_lst, column_config_default);
        (Types.with_comment (Raw "|") (second bar_tok), column_config_default);
        (Types.with_comment (Raw ("?'" ^ rv_s)) rv_d, column_config_default);
      ] in
      let paren = Paren ("?(", Types.with_comment_none (Column clmn_lst), ")") in
      Types.with_comment_paren (second q_tok) paren (second cls_tok)
    }
;
typ_opt_dom_entry:
  | rlabel=LOWER; colon_tok=COLON; mnty=typ
    {
      let (_, d, s) = rlabel in
      Types.with_comment_none (Column ([
        (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
        (Types.with_comment (Raw ":") (second colon_tok), column_config_default |> set_is_break false);
        (mnty, column_config_default);
      ]))
    }
;
typ_cmd_arg:
  | rmnopts_opt=option(typ_opt_dom); mnty=typ_prod
      {
        let lst =
          match rmnopts_opt with
          | Some(rwc) -> (rwc, column_config_default) :: mnty
          | None -> mnty
        in
        Types.with_comment_none (Column lst)
      }
;
typ_record_elem:
  | rlabel=LOWER; colon_tok=COLON; mnty=typ {
    let (_, d, s) = rlabel in
    let lst = [
      (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
      (Types.with_comment (Raw ":") (second colon_tok), column_config_default |> set_is_break false);
      (mnty, column_config_default);
    ] in
    Types.with_comment_none (Column lst)
  }
;
inline_macro_type:
  | inline_tok=INLINE; opn_tok=L_SQUARE; mnmacroargtys=optterm_list(COMMA, typ_macro_arg); cls_tok=R_SQUARE {
    let paren = Paren ("(", Types.with_comment_none (List (",", mnmacroargtys)), ")") in
    let lst = [
      (Types.with_comment (Raw "inline") (second inline_tok), column_config_default |> set_is_break false);
      (Types.with_comment_paren (second opn_tok) paren (second cls_tok), column_config_default);
    ] in
    Types.with_comment_none (Column lst)
  }
;
block_macro_type:
  | block_tok=BLOCK; opn_tok=L_SQUARE; mnmacroargtys=optterm_list(COMMA, typ_macro_arg); cls_tok=R_SQUARE {
    let paren = Paren ("(", Types.with_comment_none (List (",", mnmacroargtys)), ")") in
    let lst = [
      (Types.with_comment (Raw "block") (second block_tok), column_config_default |> set_is_break false);
      (Types.with_comment_paren (second opn_tok) paren (second cls_tok), column_config_default);
    ] in
    Types.with_comment_none (Column lst)
  }
;
typ_macro_arg:
  | mnty=typ_prod
      { Types.with_comment_none (Column mnty) }
  | tok=EXACT_TILDE; mnty=typ_bot
    {
      Types.with_comment_none (Column [
        (Types.with_comment (Raw "~") (second tok), column_config_default |> set_is_break false |> set_space_size 0);
        (mnty, column_config_default)
      ])
    }
;
match_branches:
  | branch=branch; {
    [
      (branch, column_config_default |> set_is_break true)
    ]
  }
  | bar_tok=BAR; branch=branch; {
    [
      (Types.with_comment (Raw "|") (second bar_tok), column_config_default |> set_is_break false);
      (branch, column_config_default |> set_is_break true);
    ]
  }
  | bar_tok=BAR; branch=branch; lst=match_branches; {
    (Types.with_comment (Raw "|") (second bar_tok), column_config_default |> set_is_break false) ::
      ((branch, column_config_default |> set_is_break true) :: lst)
  }
;
expr:
  | let_exprs=nonempty_list(let_expr) rwc_expr=expr {
    let lst = List.concat let_exprs in
    Types.with_comment_none (Column (List.append lst [(rwc_expr, column_config_default)]))
  }
  | match_tok=MATCH; rwc=expr; with_tok=WITH; branches=match_branches; end_tok=END {
    let lst =
      [
        (Types.with_comment (Raw "match") (second match_tok), column_config_default |> set_is_break false);
        (rwc, column_config_default |> set_is_break false |> set_space_size 1);
        (Types.with_comment (Raw "with") (second with_tok), column_config_default |> set_is_break true);
      ]
    in
    let lst = List.append lst branches in
    let lst = List.append lst [(Types.with_comment (Raw "end") (second end_tok), column_config_default)] in
    Types.with_comment_none (Column lst)
  }
  | if_tok=IF; rwc0=expr; then_tok=THEN; rwc1=expr; else_tok=ELSE; rwc2=expr {
    Types.with_comment_none (Column [
      (Types.with_comment (Raw "if") (second if_tok), column_config_default);
      (rwc0, column_config_default);
      (Types.with_comment (Raw "then") (second then_tok), column_config_default);
      (rwc1, column_config_default);
      (Types.with_comment (Raw "else") (second else_tok), column_config_default);
      (rwc2, column_config_default);
    ])
  }
  | fun_tok=FUN; param_units=list(param_unit); arrow_tok=ARROW; rwc=expr {
    Types.with_comment_none (Column [
      (Types.with_comment (Raw "fun") (second fun_tok), column_config_default);
      (Types.with_comment_none (Column (List.concat param_units)), column_config_default);
      (Types.with_comment (Raw "->") (second arrow_tok), column_config_default);
      (rwc, column_config_default);
    ])
  }
  | rwc=expr_overwrite { rwc }
;
let_expr: /* let ~ in, (rule_with_comment * column_config) list */
  | val_tok=LET; rec_tok=REC; param_val=param_val; eq_tok=EXACT_EQ; rwc_expr=expr; and_expr_lst=list(let_rec_and_expr); in_tok=IN {
    let lst = [
      (Types.with_comment (Raw "let") (second val_tok), column_config_default |> set_is_break false);
      (Types.with_comment (Raw "rec") (second rec_tok), column_config_default |> set_is_break false);
    ] in
    let lst = List.append lst param_val in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default);
      (rwc_expr, column_config_default);
    ] in
    let clmn = Types.with_comment_none (Column lst) in
    let lst = (clmn, column_config_default |> set_is_break true) :: and_expr_lst in
    List.append lst [(Types.with_comment (Raw "in") (second in_tok), column_config_default |> set_is_break true)]
  }
  | tok=LET; valbind=bind_value; in_tok=IN {
    let lst = (Types.with_comment (Raw "let") (second tok), column_config_default |> set_is_break false) :: valbind in
    let lst = List.append lst valbind in
    let lst = List.append lst [(Types.with_comment (Raw "in") (second in_tok), column_config_default |> set_is_break true)]
    in
    lst
  }
  | tok=LET; rwc_pat=pattern_non_var; eq_tok=EXACT_EQ; rwc=expr; in_tok=IN {
    [
      (Types.with_comment (Raw "let") (second tok), column_config_default |> set_is_break false);
      (rwc_pat, column_config_default |> set_is_break false);
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default);
      (rwc, column_config_default);
      (Types.with_comment (Raw "in") (second in_tok), column_config_default);
    ]
  }
  | tok=LET; opn_tok=OPEN; modident=UPPER; in_tok=IN {
    let (_,d,s) = modident in
    [
      (Types.with_comment (Raw "let") (second tok), column_config_default |> set_is_break false);
      (Types.with_comment (Raw "open") (second tok), column_config_default |> set_is_break false);
      (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
      (Types.with_comment (Raw "in") (second in_tok), column_config_default);
    ]
  }
;
let_rec_and_expr:
  | and_tok=AND; param_val=param_val; eq_tok=EXACT_EQ; rwc_expr=expr {
    let lst = [
      (Types.with_comment (Raw "and") (second and_tok), column_config_default |> set_is_break false);
    ] in
    let lst = List.append lst param_val in
    let lst = List.append lst [
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default);
      (rwc_expr, column_config_default);
    ] in
    (Types.with_comment_none (Column lst), column_config_default |> set_is_break true)
  }
;
expr_overwrite:
  | ident=LOWER; arrow_tok=REVERSED_ARROW; rwc=expr_op {
    let (_,d,s) = ident in
    Types.with_comment_none (Column [
      (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
      (Types.with_comment (Raw "<-") (second arrow_tok), column_config_default |> set_is_break false);
      (rwc, column_config_default);
    ])
  }
  | rwc=expr_op { rwc }
;
expr_op:
  | rwcL=expr_op; op=BINOP_BAR;     rwcR=expr_op
  | rwcL=expr_op; op=BINOP_AMP;     rwcR=expr_op
  | rwcL=expr_op; op=BINOP_EQ;      rwcR=expr_op
  | rwcL=expr_op; op=BINOP_GT;      rwcR=expr_op
  | rwcL=expr_op; op=BINOP_LT;      rwcR=expr_op
  | rwcL=expr_op; op=BINOP_HAT;     rwcR=expr_op
  | rwcL=expr_op; op=BINOP_PLUS;    rwcR=expr_op
  | rwcL=expr_op; op=BINOP_MINUS;   rwcR=expr_op
  | rwcL=expr_op; op=BINOP_TIMES;   rwcR=expr_op
  | rwcL=expr_op; op=BINOP_DIVIDES; rwcR=expr_op
    {
      let (_, d, s) = op in
      Types.with_comment_none (Column [
        (rwcL, column_config_default);
        (Types.with_comment (Raw s) d, column_config_default);
        (rwcR, column_config_default);
      ])
    }
  | rwcL=expr_op; tok=CONS;         rwcR=expr_op
    {
      Types.with_comment_none (Column [
        (rwcL, column_config_default);
        (Types.with_comment (Raw "::") (second tok), column_config_default);
        (rwcR, column_config_default);
      ])
    }
  | rwcL=expr_op; tok=EXACT_MINUS;  rwcR=expr_op
    {
      Types.with_comment_none (Column [
        (rwcL, column_config_default);
        (Types.with_comment (Raw "-") (second tok), column_config_default);
        (rwcR, column_config_default);
      ])
    }
  | rwcL=expr_op; tok=EXACT_TIMES;  rwcR=expr_op
    {
      Types.with_comment_none (Column [
        (rwcL, column_config_default);
        (Types.with_comment (Raw "*") (second tok), column_config_default);
        (rwcR, column_config_default);
      ])
    }
  | rwcL=expr_op; tok=MOD;          rwcR=expr_op
    {
      Types.with_comment_none (Column [
        (rwcL, column_config_default);
        (Types.with_comment (Raw "mod") (second tok), column_config_default);
        (rwcR, column_config_default);
      ])
    }
  | tok=EXACT_MINUS; rwc2=expr_app
    {
      Types.with_comment_none (Column [
        (Types.with_comment (Raw "-") (second tok), column_config_default |> set_is_break false |> set_space_size 0);
        (rwc2, column_config_default);
      ])
    }
  | ctor=UPPER; rwc2=expr_un
    {
      let (_,d,s) = ctor in
      Types.with_comment_none (Column [
        (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
        (rwc2, column_config_default);
      ])
    }
  | ctor=UPPER
    {
      let (_,d,s) = ctor in
      Types.with_comment (Raw s) d
    }
  | rwc=expr_app { rwc }
;
expr_app:
  | rwc1=expr_app; mnopts=expr_opts; rwc2=expr_un
    {
      let lst =
        match mnopts with
        | Some(mnopts) -> [
          (rwc1, column_config_default);
          (mnopts, column_config_default);
          (rwc2, column_config_default);
        ]
        | None -> [
          (rwc1, column_config_default);
          (rwc2, column_config_default);
        ]
      in
      Types.with_comment_none (Column lst)
    }
  | rwc1=expr_app; mnopts=expr_opts; ctor=UPPER
    {
      let (_,d,s) = ctor in
      let lst =
        match mnopts with
        | Some(mnopts) -> [
          (rwc1, column_config_default);
          (mnopts, column_config_default);
          (Types.with_comment (Raw s) d, column_config_default);
        ]
        | None -> [
          (rwc1, column_config_default);
          (Types.with_comment (Raw s) d, column_config_default);
        ]
      in
      Types.with_comment_none (Column lst)
    }
  | rwc=expr_un { rwc }
;
expr_un:
  | unop=UNOP_EXCLAM; rwc2=expr_bot
    {
      let (_,d,s) = unop in
      Types.with_comment_none (Column [
        (Types.with_comment (Raw s) d, column_config_default);
        (rwc2, column_config_default);
      ])
    }
  | tok=EXACT_AMP; rwc2=expr_bot
    {
      Types.with_comment_none (Column [
        (Types.with_comment (Raw "&") (second tok), column_config_default |> set_is_break false |> set_space_size 0);
        (rwc2, column_config_default);
      ])
    }
  | tok=EXACT_TILDE; rwc2=expr_bot
    {
      Types.with_comment_none (Column [
        (Types.with_comment (Raw "~") (second tok), column_config_default |> set_is_break false |> set_space_size 0);
        (rwc2, column_config_default);
      ])
    }
  | rwc=expr_bot { rwc }
;
expr_bot:
  | rwc=expr_bot; access_tok=ACCESS; rlabel=LOWER
      {
        let (_,d,s) = rlabel in
        match rwc.rule with
        | Paren(_,_,_) ->
          Types.with_comment_none (Column [
            (rwc, column_config_default |> set_is_break false |> set_space_size 0);
            (Types.with_comment (Raw "#") (second access_tok), column_config_default |> set_is_break false |> set_space_size 0);
            (Types.with_comment (Raw s) d, column_config_default |> set_is_break false |> set_space_size 0);
          ])
        | _ ->
          Types.with_comment_none (Column [
            (rwc, column_config_default);
            (Types.with_comment (Raw "#") (second access_tok), column_config_default);
            (Types.with_comment (Raw s) d, column_config_default);
          ])
      }
  | ident=LOWER
      {
        let (_,d,s) = ident in
        Types.with_comment (Raw s) d
      }
  | long_ident=LONG_LOWER
      {
        let (_,d,ms,s) = long_ident in
        let s = (Types.lst_join "." ms) ^ "." ^ s in
        Types.with_comment (Raw s) d
      }
  | ic=INT
      {
        let (_,d,i) = ic in
        Types.with_comment (Raw (string_of_int i)) d
      }
  | fc=FLOAT
      {
        let (_,d,f) = fc in
        Types.with_comment (Raw (string_of_float f)) d
      }
  | lc=LENGTH
      {
        let (_,d,fc,unitnm) = lc in
        Types.with_comment (Raw (string_of_float fc ^ unitnm)) d
      }
  | tok=TRUE
      {
        Types.with_comment (Raw "true") (second tok)
      }
  | tok=FALSE
      {
        Types.with_comment (Raw "false") (second tok)
      }
  | tok=STRING
      {
        let (rng, d, str, pre, post) = tok in
        (*
          TODO:中の"`"の数のカウントと左右の空白の有無を判定して自動で外側に"#"や"`"を追加する関数を実装せよ
        *)
        let to_str str = "`" ^ str ^ "`" in
        let s = to_str str in
        Types.with_comment (Raw s) d
      }
  | tok=POSITIONED_STRING
      {
        let (rng, d, ipos, s) = tok in
        (*
          TODO:中の"`"の数のカウントと左右の空白の有無を判定して自動で外側に"#"や"`"を追加する関数を実装せよ
        *)
        let to_str str = "@`" ^ str ^ "`" in
        let s = to_str s in
        Types.with_comment (Raw s) d
      }
  | opn_tok=L_PAREN; ident=binop; cls_tok=R_PAREN
      {
        Types.with_comment_paren (second opn_tok) (Paren("(", ident, ")")) (second cls_tok)
      }
  | opn_tok=L_PAREN; cmd_tok=COMMAND; long_ident=backslash_cmd; cls_tok=R_PAREN
  | opn_tok=L_PAREN; cmd_tok=COMMAND; long_ident=plus_cmd; cls_tok=R_PAREN
      {
        let clmn =
          Types.with_comment_none (Column [
            (Types.with_comment (Raw "command") (second cmd_tok), column_config_default |> set_is_break false);
            (long_ident, column_config_default);
          ])
        in
        Types.with_comment_paren (second opn_tok) (Paren("(", clmn, ")")) (second cls_tok)
      }
  | opn_tok=L_PAREN; cls_tok=R_PAREN
      {
        Types.with_comment_paren (second opn_tok) (Paren("(", (Types.with_comment_none Null), ")")) (second cls_tok)
      }
  | opn_tok=L_PAREN; rwc=expr; cls_tok=R_PAREN
      {
        match rwc.rule with
        | Paren(_,_,_) -> rwc
        | _ -> Types.with_comment_paren (second opn_tok) (Paren("(", rwc, ")")) (second cls_tok)
      }
  | opn_tok=L_PAREN; rwc1=expr; COMMA; rwcs=separated_nonempty_list(COMMA, expr); cls_tok=R_PAREN
      {
        let rwc = Types.with_comment_none (List (",", (rwc1 :: rwcs))) in
        Types.with_comment_paren (second opn_tok) (Paren("(", rwc, ")")) (second cls_tok)
      }
  | rwc=expr_bot_list { rwc }
  | rwc=expr_bot_record { rwc }
  | opn_tok=L_INLINE_TEXT; rwc=inline; cls_tok=R_INLINE_TEXT
    {
      Types.with_comment_paren (second opn_tok) (Paren("{", rwc, "}")) (second cls_tok)
    }
  | opn_tok=L_INLINE_TEXT_LIST; lst=inline_lst; cls_tok=R_INLINE_TEXT_LIST
    {
      Types.with_comment_paren (second opn_tok) (Paren("{|", Types.with_comment_none (List ("|", lst)), "|}")) (second cls_tok)
    }
  | opn_tok=L_BLOCK_TEXT; rwc=block; cls_tok=R_BLOCK_TEXT
    {
      Types.with_comment_paren (second opn_tok) (Paren("'<", rwc, ">")) (second cls_tok)
    }
  | opn_tok=L_MATH_TEXT; rwc=math; cls_tok=R_MATH_TEXT
    {
      Types.with_comment_paren (second opn_tok) (Paren("${", rwc, "}")) (second cls_tok)
    }
  | opn_tok=L_MATH_TEXT_LIST; rwc=math_lst; cls_tok=R_MATH_TEXT_LIST
    {
      Types.with_comment_paren (second opn_tok) (Paren("${|", Types.with_comment_none (List ("|", rwc)), "|}")) (second cls_tok)
    }
;
expr_bot_list:
  | opn_tok=L_SQUARE; rwcs=optterm_list(COMMA, expr); cls_tok=R_SQUARE
    {
      let rwc = Types.with_comment_none (List (",", rwcs)) in
      Types.with_comment_paren (second opn_tok) (Paren("[", rwc, "]")) (second cls_tok)
    }
;
expr_bot_record:
  | opn_tok=L_RECORD; fields=optterm_list(COMMA, record_field); cls_tok=R_RECORD
    {
      let rwc = Types.with_comment_none (List (",", fields)) in
      Types.with_comment_paren (second opn_tok) (Paren("{|", rwc, "}|")) (second cls_tok)
    }
  | opn_tok=L_RECORD; rwc1=expr_bot; with_tok=WITH; fields=optterm_nonempty_list(COMMA, record_field); cls_tok=R_RECORD
    {
      let rwc2 = Types.with_comment_none (List (",", fields)) in
      let rwc = Types.with_comment_none (Column [
        (rwc1, column_config_default);
        (Types.with_comment (Raw "with") (second with_tok), column_config_default);
        (rwc2, column_config_default);
      ])
      in
      Types.with_comment_paren (second opn_tok) (Paren("{|", rwc, "}|")) (second cls_tok)
    }
;
record_field:
  | rlabel=LOWER; eq_tok=EXACT_EQ; rwc=expr
    {
      let (_,d,s) = rlabel in
      Types.with_comment_none (Column [
        (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
        (Types.with_comment (Raw "=") (second eq_tok), column_config_default |> set_is_break false);
        (rwc, column_config_default |> set_is_break false);
      ])
    }
;
branch:
  | rwcpat=pattern; arrow_tok=ARROW; rwc=expr
    {
      Types.with_comment_none (Column [
        (rwcpat, column_config_default |> set_is_break false);
        (Types.with_comment (Raw "->") (second arrow_tok), column_config_default);
        (rwc, column_config_default |> set_is_break false);
      ])
    }
;
binop:
  | tok=UNOP_EXCLAM
  | tok=BINOP_TIMES
  | tok=BINOP_DIVIDES
  | tok=BINOP_HAT
  | tok=BINOP_EQ
  | tok=BINOP_GT
  | tok=BINOP_LT
  | tok=BINOP_AMP
  | tok=BINOP_BAR
  | tok=BINOP_PLUS
  | tok=BINOP_MINUS { let (_,d,s) = tok in Types.with_comment (Raw s) d }
  | tok=EXACT_TIMES { let (_,d) = tok in Types.with_comment (Raw "*") d  }
  | tok=EXACT_MINUS { let (_,d) = tok in Types.with_comment (Raw "-") d  }
  | tok=MOD         { let (_,d) = tok in Types.with_comment (Raw "mod") d  }
;
pattern:
  | rwcpat=pattern_cons; as_tok=AS; ident=LOWER {
    let (_,d,s) = ident in
    Types.with_comment_none (Column [
      (rwcpat, column_config_default |> set_is_break false);
      (Types.with_comment (Raw "as") (second as_tok), column_config_default |> set_is_break false);
      (Types.with_comment (Raw s) d, column_config_default);
    ])
  }
  | rwc=pattern_cons { rwc }
;
pattern_non_var:
  | rwcpat=pattern_cons; as_tok=AS; ident=LOWER {
    let (_,d,s) = ident in
    Types.with_comment_none (Column [
      (rwcpat, column_config_default |> set_is_break false);
      (Types.with_comment (Raw "as") (second as_tok), column_config_default |> set_is_break false);
      (Types.with_comment (Raw s) d, column_config_default);
    ])
  }
  | rwc=pattern_non_var_cons { rwc }
;
pattern_cons:
  | rwc1=pattern_bot; cons_tok=CONS; rwc2=pattern_cons {
    Types.with_comment_none (Column [
      (rwc1, column_config_default |> set_is_break false);
      (Types.with_comment (Raw "::") (second cons_tok), column_config_default |> set_is_break false);
      (rwc2, column_config_default);
    ])
  }
  | ctor=UPPER; rwcpat=pattern_bot {
    let (_,d,s) = ctor in
    Types.with_comment_none (Column [
      (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
      (rwcpat, column_config_default);
    ])
  }
  | ctor=UPPER { let (_,d,s) = ctor in Types.with_comment (Raw s) d }
  | rwcpat=pattern_bot { rwcpat }
;
pattern_non_var_cons:
  | rwc1=pattern_bot; cons_tok=CONS; rwc2=pattern_cons {
    Types.with_comment_none (Column [
      (rwc1, column_config_default |> set_is_break false);
      (Types.with_comment (Raw "::") (second cons_tok), column_config_default |> set_is_break false);
      (rwc2, column_config_default);
    ])
  }
  | ctor=UPPER; rwcpat=pattern_bot {
    let (_,d,s) = ctor in
    Types.with_comment_none (Column [
      (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
      (rwcpat, column_config_default);
    ])
  }
  | ctor=UPPER { let (_,d,s) = ctor in Types.with_comment (Raw s) d }
  | rwcpat=pattern_non_var_bot { rwcpat }
;
pattern_bot:
  | ident=bound_identifier { ident }
  | rwcpat=pattern_non_var_bot { rwcpat }
;
pattern_non_var_bot:
  | ic=INT
      { let (_,d,n) = ic in Types.with_comment (Raw (string_of_int n)) d }
  | tok=TRUE
      { Types.with_comment (Raw "true") (second tok) }
  | tok=FALSE
      { Types.with_comment (Raw "false") (second tok) }
  | opn_tok=L_PAREN; cls_tok=R_PAREN
      {
        Types.with_comment_paren
          (second opn_tok)
          (Paren("(", (Types.with_comment_none Null), ")"))
          (second cls_tok)
      }
  | tok=WILDCARD
      { Types.with_comment (Raw "_") (second tok) }
  | lit=STRING
      {
        let (rng, d, str, pre, post) = lit in
        (*
          TODO:中の"`"の数のカウントと左右の空白の有無を判定して自動で外側に"#"や"`"を追加する関数を実装せよ
        *)
        let to_str str pre post = "`" ^ str ^ "`" in
        let s = to_str str pre post in
        Types.with_comment (Raw s) d
      }
  | opn_tok=L_SQUARE; rwcpats=optterm_list(COMMA, pattern); cls_tok=R_SQUARE
      {
        let rwc = Types.with_comment_none (List (",", rwcpats)) in
        Types.with_comment_paren (second opn_tok) (Paren("[", rwc, "]")) (second cls_tok)
      }
  | opn_tok=L_PAREN; rwcpat=pattern; cls_tok=R_PAREN
      {
        Types.with_comment_paren (second opn_tok) (Paren("(", rwcpat, ")")) (second cls_tok)
      }
  | opn_tok=L_PAREN; rwc1=pattern; COMMA; rwcs=optterm_nonempty_list(COMMA, pattern); cls_tok=R_PAREN
      {
        let rwc = Types.with_comment_none (List (",", (rwc1 :: rwcs))) in
        Types.with_comment_paren (second opn_tok) (Paren("(", rwc, ")")) (second cls_tok)
      }
;
inline:
  | rwc=inline_single { rwc }
  | itemizes=nonempty_list(inline_itemize_elem) {
    let c = column_config_default |> set_is_break true in
    Types.with_comment_none (Column (List.map (fun r -> (r, c)) itemizes))
  }
;
inline_lst:
  | rwc=inline_single { [rwc] }
  | rwc=inline_single BAR; aux=inline_lst { rwc::aux }
;
inline_itemize_elem:
  | item=ITEM; rwc=inline_single {
    let (_, data, depth) = item in
    make_itemize_elem depth data rwc
  }
;
inline_single:
  | ielems=inline_elems {
    Types.with_comment_none (Column ielems)
  }
;
inline_elems: /* (rule_with_comment * column_config) list */
  | itext=inline_elem_text; icmd=inline_elem_cmd; ielems=inline_elems { List.append itext ((icmd, column_config_default) :: ielems) }
  | icmd=inline_elem_cmd; ielems=inline_elems { (icmd, column_config_default) :: ielems }
  | itext=inline_elem_text { itext }
  |   { [] }
;
inline_elem_cmd:
  | icmd=backslash_cmd; nargs=list(cmd_arg_expr); rsargs=cmd_args_text {
    let args = List.append (List.concat nargs) rsargs in
    let c = column_config_default in
    Types.with_comment_none (Column ((icmd, c) :: (List.map (fun r -> (r, c)) args)))
  }
  | imacro_raw=BACKSLASH_MACRO; macargsraw=macroargs {
      let (_, d, s) = imacro_raw in
      let lst =
        (Types.with_comment (Raw ("\\" ^ s ^ "@")) d, column_config_default) :: macargsraw
      in
      Types.with_comment_none (Column lst)
    }
  | imacro=LONG_BACKSLASH_MACRO; macargsraw=macroargs {
      let (_, d, ms, s) = imacro in
      let lst =
        (Types.with_comment (Raw ("\\" ^ (Types.lst_join "." ms) ^ "." ^ s ^ "@")) d, column_config_default)
          :: macargsraw
      in
      Types.with_comment_none (Column lst)
    }
  | opn_tok=L_MATH_TEXT; rwc=math; cls_tok=R_MATH_TEXT {
    Types.with_comment_paren (second opn_tok) (Paren ("${", rwc, "}")) (second cls_tok)
  }
  | opn_tok=L_MATH_TEXT_LIST; lst=math_lst; cls_tok=R_MATH_TEXT_LIST {
    Types.with_comment_paren (second opn_tok) (Paren("${|", Types.with_comment_none (List ("|",lst)), "|}")) (second cls_tok)
  }
  | literal=STRING {
    let (_, d, str, pre, post) = literal in
    (*
      TODO:中の"`"の数のカウントと左右の空白の有無を判定して自動で外側に"#"や"`"を追加する関数を実装せよ
    *)
    let to_str str pre post = str in
    let s = to_str str pre post in
    Types.with_comment (Raw s) d
  }
  | long_ident=VAR_IN_TEXT; tok=SEMICOLON {
    let (_, d, ms, s) = long_ident in
    let s =
      if List.length ms == 0 then
        s
      else
        (Types.lst_join "." ms) ^ "." ^ s
    in
    Types.with_comment_none (Column [
      (Types.with_comment (Raw ("#" ^ s)) d, column_config_default |> set_is_break false);
      (Types.with_comment (Raw ";") (second tok), column_config_default)
    ])
  }
;
inline_elem_text:
  | ichars=nonempty_list(inline_char) {
    Types.break_inline_elem_text ichars
  }
;
inline_char:
  | char=CHAR { let (_,d,s) = char in (d,s) }
  | tok=SPACE { let (_,d) = tok in (d, " ") }
  | tok=BREAK { let (_,d) = tok in (d, " ") }
;
block:
  | belems=list(block_elem) {
    let c = column_config_default in
    Types.with_comment_none (Column (List.map (fun r -> (r,c)) belems))
  }
;
block_elem:
  | bcmd=plus_cmd; nargs=list(cmd_arg_expr); rsargs=cmd_args_text {
    let args = List.append (List.concat nargs) rsargs in
    let c = column_config_default in
    Types.with_comment_none (Column ((bcmd, c) :: (List.map (fun r -> (r, c)) args)))
  }
  | bmacro_raw=PLUS_MACRO; macargsraw=macroargs {
      let (_, d, s) = bmacro_raw in
      let lst =
        (Types.with_comment (Raw ("+" ^ s ^ "@")) d, column_config_default) :: macargsraw
      in
      Types.with_comment_none (Column lst)
    }
  | bmacro=LONG_PLUS_MACRO; macargsraw=macroargs {
      let (_, d, ms, s) = bmacro in
      let lst =
        (Types.with_comment (Raw ("+" ^ (Types.lst_join "." ms) ^ "." ^ s ^ "@")) d, column_config_default)
          :: macargsraw
      in
      Types.with_comment_none (Column lst)
    }
  | long_ident=VAR_IN_TEXT; tok=SEMICOLON {
    let (_, d, ms, s) = long_ident in
    let s =
      if List.length ms == 0 then
        s
      else
        (Types.lst_join "." ms) ^ "." ^ s
    in
    Types.with_comment_none (Column [
      (Types.with_comment (Raw ("#" ^ s)) d, column_config_default |> set_is_break false);
      (Types.with_comment (Raw ";") (second tok), column_config_default)
    ])
  }
;
math:
  | rwc=math_single { rwc }
;
math_lst:
  | rwc=math_single { [rwc] }
  | rwc=math_single BAR; aux=math_lst { rwc::aux }
;
math_single:
  | math_elmes=list(math_elem) {
    let c = column_config_default in
    Types.with_comment_none (Column (List.map (fun r -> (r, c)) math_elmes))
  }
;
math_elem:
  (* a: *)
  | bot=math_bot { bot }
  (* a^p: *)
  | bot=math_bot; tok=SUPERSCRIPT; rsup=math_group {
    Types.with_comment_none (Column [
      (bot, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw "^") (second tok), column_config_default |> set_is_break false |> set_space_size 0);
      (rsup, column_config_default);
    ])
  }
  (* a_b: *)
  | bot=math_bot; tok=SUBSCRIPT; rsub=math_group {
    Types.with_comment_none (Column [
      (bot, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw "_") (second tok), column_config_default |> set_is_break false |> set_space_size 0);
      (rsub, column_config_default);
    ])
  }
  (* a_b^p: *)
  | bot=math_bot; tok1=SUBSCRIPT; rsub=math_group; tok2=SUPERSCRIPT; rsup=math_group {
    Types.with_comment_none (Column [
      (bot, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw "_") (second tok1), column_config_default |> set_is_break false |> set_space_size 0);
      (rsub, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw "^") (second tok2), column_config_default |> set_is_break false |> set_space_size 0);
      (rsup, column_config_default);
    ])
  }
  (* a^p_b: *)
  | bot=math_bot; tok1=SUPERSCRIPT; rsup=math_group; tok2=SUBSCRIPT; rsub=math_group {
    Types.with_comment_none (Column [
      (bot, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw "^") (second tok1), column_config_default |> set_is_break false |> set_space_size 0);
      (rsup, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw "_") (second tok2), column_config_default |> set_is_break false |> set_space_size 0);
      (rsub, column_config_default);
    ])
  }
  (* a': *)
  | bot=math_bot; prime=PRIMES {
    let (_, d, n) = prime in
    let s = Types.str_repeat "'" n in
    Types.with_comment_none (Column [
      (bot, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw s) d, column_config_default);
    ])
  }
  (* a'^p: *)
  | bot=math_bot; prime=PRIMES; tok=SUPERSCRIPT; rsup=math_group {
    let (_, d, n) = prime in
    let s = Types.str_repeat "'" n in
    Types.with_comment_none (Column [
      (bot, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw s) d, column_config_default);
      (Types.with_comment (Raw "^") (second tok), column_config_default |> set_is_break false |> set_space_size 0);
      (rsup, column_config_default);
    ])
  }
  (* a'_b: *)
  | bot=math_bot; prime=PRIMES; tok=SUBSCRIPT; rsub=math_group {
    let (_, d, n) = prime in
    let s = Types.str_repeat "'" n in
    Types.with_comment_none (Column [
      (bot, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw s) d, column_config_default);
      (Types.with_comment (Raw "_") (second tok), column_config_default |> set_is_break false |> set_space_size 0);
      (rsub, column_config_default);
    ])
  }
  (* a'_b^p: *)
  | bot=math_bot; prime=PRIMES; tok1=SUBSCRIPT; rsub=math_group; tok2=SUPERSCRIPT; rsup=math_group {
    let (_, d, n) = prime in
    let s = Types.str_repeat "'" n in
    Types.with_comment_none (Column [
      (bot, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw s) d, column_config_default);
      (Types.with_comment (Raw "_") (second tok1), column_config_default |> set_is_break false |> set_space_size 0);
      (rsub, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw "^") (second tok2), column_config_default |> set_is_break false |> set_space_size 0);
      (rsup, column_config_default);
    ])
  }
  (* a'^p_b: *)
  | bot=math_bot; prime=PRIMES; tok1=SUPERSCRIPT; rsup=math_group; tok2=SUBSCRIPT; rsub=math_group {
    let (_, d, n) = prime in
    let s = Types.str_repeat "'" n in
    Types.with_comment_none (Column [
      (bot, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw s) d, column_config_default);
      (Types.with_comment (Raw "^") (second tok1), column_config_default |> set_is_break false |> set_space_size 0);
      (rsup, column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment (Raw "_") (second tok2), column_config_default |> set_is_break false |> set_space_size 0);
      (rsub, column_config_default);
    ])
  }
;
math_group:
  | opn_tok=L_MATH_TEXT; ms=math_single; cls_tok=R_MATH_TEXT {
    Types.with_comment_paren (second opn_tok) (Paren ("{", ms, "}")) (second cls_tok)
  }
  | bot=math_bot { bot }
;
math_bot:
  | tok=MATHCHARS {
    let (_, d, s) = tok in
    Types.with_comment (Raw s) d
  }
  | mcmd=backslash_cmd; args=list(math_cmd_arg) {
    let lst = mcmd :: (List.concat args) in
    let c = column_config_default in
    Types.with_comment_none (Column (List.map (fun r -> (r, c)) lst))
  }
;
math_cmd_arg:
  | mn_opt=expr_opts; opn_tok=L_MATH_TEXT; rwc=math; cls_tok=R_MATH_TEXT {
    let rwc =
      Types.with_comment_paren
        (second opn_tok)
        (Paren ("{", rwc, "}"))
        (second cls_tok)
    in
    match mn_opt with
    | Some(mn) -> [mn; rwc]
    | None -> [rwc]
  }
  | mn_opt=expr_opts; opn_tok=L_MATH_TEXT_LIST; lst=math_lst; cls_tok=R_MATH_TEXT_LIST {
    let rwc =
      Types.with_comment_paren
        (second opn_tok)
        (Paren ("{|", Types.with_comment_none (List ("|", lst)), "|}"))
        (second cls_tok)
    in
    match mn_opt with
    | Some(mn) -> [mn; rwc]
    | None -> [rwc]
  }
  | mn_opt=expr_opts; opn_tok=L_INLINE_TEXT; rwc=inline; cls_tok=R_INLINE_TEXT {
    let rwc =
      Types.with_comment_paren
        (second opn_tok)
        (Paren ("!{", rwc, "}"))
        (second cls_tok)
    in
    match mn_opt with
    | Some(mn) -> [mn; rwc]
    | None -> [rwc]
  }
  | mn_opt=expr_opts; opn_tok=L_INLINE_TEXT_LIST; lst=inline_lst; cls_tok=R_INLINE_TEXT_LIST {
    let rwc =
      Types.with_comment_paren
        (second opn_tok)
        (Paren ("!{|", Types.with_comment_none (List ("|", lst)), "|}"))
        (second cls_tok)
    in
    match mn_opt with
    | Some(mn) -> [mn; rwc]
    | None -> [rwc]
  }
  | mn_opt=expr_opts; opn_tok=L_BLOCK_TEXT; rwc=block; cls_tok=R_BLOCK_TEXT {
    let rwc =
      Types.with_comment_paren
        (second opn_tok)
        (Paren ("!<", rwc, ">"))
        (second cls_tok)
    in
    match mn_opt with
    | Some(mn) -> [mn; rwc]
    | None -> [rwc]
  }
  | cmdarg=cmd_arg_expr { cmdarg }
;

macroargs:
  | macnargs=list(macronarg); semi_tok=SEMICOLON {
    List.append
      (List.map (fun rwc -> (rwc, column_config_default)) macnargs)
      [(Types.with_comment (Raw ";") (second semi_tok), column_config_default)]
  }
;
macronarg:
  | opn_tok=L_PAREN; expr=expr_bot; cls_tok=R_PAREN {
    Types.with_comment_paren (second opn_tok) (Paren ("(", expr, ")")) (second cls_tok)
  }
  | tilde_tok=EXACT_TILDE; opn_tok=L_PAREN; expr=expr_bot; cls_tok=R_PAREN {
    Types.with_comment_none (Column [
      (Types. with_comment (Raw "~") (second tilde_tok), column_config_default |> set_is_break false |> set_space_size 0);
      (Types.with_comment_paren (second opn_tok) (Paren ("(", expr, ")")) (second cls_tok), column_config_default);
    ])
  }
;
cmd_arg_expr:
  | mn_opt=expr_opts; opn_tok=L_PAREN; rwc=expr; cls_tok=R_PAREN {
    let rwc =
      Types.with_comment_paren
        (second opn_tok)
        (Paren ("(", rwc, ")"))
        (second cls_tok)
    in
    match mn_opt with
    | Some(mn) -> [mn; rwc]
    | None -> [rwc]
  }
  | mn_opt=expr_opts; opn_tok=L_PAREN; cls_tok=R_PAREN {
    let rwc =
      Types.with_comment_paren
        (second opn_tok)
        (Paren ("(", Types.with_comment_none Null, ")"))
        (second cls_tok)
    in
    match mn_opt with
    | Some(mn) -> [mn; rwc]
    | None -> [rwc]
  }
  | mn_opt=expr_opts; rwc=expr_bot_record {
    match mn_opt with
    | Some(mn) -> [mn; rwc]
    | None -> [rwc]
  }
  | mn_opt=expr_opts; rwc=expr_bot_list {
    match mn_opt with
    | Some(mn) -> [mn; rwc]
    | None -> [rwc]
  }
;
expr_opts:
  | q_tok=QUESTION; L_PAREN; mnopts=optterm_nonempty_list(COMMA, expr_opt_entry); cls_tok=R_PAREN {
    let rwc =
      Types.with_comment_paren (second q_tok) (Paren ("?(", Types.with_comment_none (List (",", mnopts)), ")")) (second cls_tok)
    in
    Some(rwc)
  }
  | { None }
;
expr_opt_entry:
  | rlabel=LOWER; eq_tok=EXACT_EQ; rwc=expr {
    let (_,d,s) = rlabel in
    Types.with_comment_none (Column [
      (Types.with_comment (Raw s) d, column_config_default |> set_is_break false);
      (Types.with_comment (Raw "=") (second eq_tok), column_config_default |> set_is_break false);
      (rwc, column_config_default);
    ])
  }
;
cmd_args_text:
  | rng=SEMICOLON { [] }
  | sargs=nonempty_list(cmd_arg_text) { sargs }
;
cmd_arg_text:
  | opn_tok=L_BLOCK_TEXT; rwc=block; cls_tok=R_BLOCK_TEXT {
    Types.with_comment_paren (second opn_tok) (Paren ("<", rwc, ">")) (second cls_tok)
  }
  | opn_tok=L_INLINE_TEXT; rwc=inline; cls_tok=R_INLINE_TEXT {
    Types.with_comment_paren (second opn_tok) (Paren ("{", rwc, "}")) (second cls_tok)
  }
  | opn_tok=L_INLINE_TEXT_LIST; lst=inline_lst; cls_tok=R_INLINE_TEXT_LIST {
    Types.with_comment_paren
      (second opn_tok)
      (Paren ("{|", Types.with_comment_none (List ("|", lst)), "|}"))
      (second cls_tok)
  }
;
backslash_cmd:
  | cs=BACKSLASH_CMD { let (_, d, csnm) = cs in Types.with_comment (Raw ("\\" ^ csnm)) d }
  | long_cs=LONG_BACKSLASH_CMD {
    let (_, d, ms, csnm) = long_cs in
    let s = "\\" ^ (Types.lst_join "." ms) ^ "." ^ csnm in
    Types.with_comment (Raw s) d
  }
;
plus_cmd:
  | cs=PLUS_CMD { let (_, d, csnm) = cs in Types.with_comment (Raw ("+" ^ csnm)) d }
  | long_cs=LONG_PLUS_CMD {
    let (_, d, ms, csnm) = long_cs in
    let s = "+" ^ (Types.lst_join "." ms) ^ "." ^ csnm in
    Types.with_comment (Raw s) d 
  }
;