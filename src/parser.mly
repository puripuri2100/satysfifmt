%{
  open Types
%}

%token <Range.t * Types.token_data * Types.var_name> VAR
%token <Range.t * Types.token_data * Types.ctrlseq_name> HORZCMD
%token <Range.t * Types.token_data * Types.ctrlseq_name> HORZMACRO
%token <Range.t * Types.token_data * Types.ctrlseq_name> VERTCMD
%token <Range.t * Types.token_data * Types.ctrlseq_name> VERTMACRO
%token <Range.t * Types.token_data * Types.ctrlseq_name> MATHCMD
%token <Range.t * Types.token_data * (Types.module_name list) * Types.var_name> VARWITHMOD
%token <Range.t * Types.token_data * (Types.module_name list) * Types.ctrlseq_name> HORZCMDWITHMOD
%token <Range.t * Types.token_data * (Types.module_name list) * Types.ctrlseq_name> VERTCMDWITHMOD
%token <Range.t * Types.token_data * (Types.module_name list) * Types.ctrlseq_name> MATHCMDWITHMOD
%token <Range.t * Types.token_data * (Types.module_name list) * Types.ctrlseq_name> VARINHORZ
%token <Range.t * Types.token_data * (Types.module_name list) * Types.ctrlseq_name> VARINVERT
%token <Range.t * Types.token_data * (Types.module_name list) * Types.var_name> VARINMATH
%token <Range.t * Types.token_data * Types.var_name> TYPEVAR
%token <Range.t * Types.token_data * Types.constructor_name> CONSTRUCTOR
%token <Range.t * Types.token_data * int> INTCONST
%token <Range.t * Types.token_data * float> FLOATCONST
%token <Range.t * Types.token_data * float * Types.length_unit_name> LENGTHCONST
%token <Range.t * Types.token_data * string> CHAR
%token <Range.t * Types.token_data * string * bool * bool> LITERAL
%token <Range.t * Types.token_data * Types.input_position * string> POSITIONED_LITERAL
%token <Range.t> SPACE BREAK
%token <Range.t * Types.token_data * string> MATHCHARS
%token <Range.t * Types.token_data * int> PRIMES
%token <Range.t * Types.token_data> SUBSCRIPT SUPERSCRIPT
%token <Range.t * Types.token_data> LAMBDA ARROW COMMAND
%token <Range.t * Types.token_data> LETREC LETNONREC DEFEQ LETAND IN OPEN
%token <Range.t * Types.token_data * Types.module_name> OPENMODULE
%token <Range.t * Types.token_data> MODULE STRUCT END DIRECT SIG VAL CONSTRAINT
%token <Range.t * Types.token_data> TYPE OF MATCH WITH BAR WILDCARD WHEN AS COLON
%token <Range.t * Types.token_data> LETMUTABLE OVERWRITEEQ
%token <Range.t * Types.token_data> LETHORZ LETVERT LETMATH
%token <Range.t * Types.token_data> IF THEN ELSE
%token <Range.t * Types.token_data * Types.var_name> BINOP_TIMES BINOP_DIVIDES BINOP_PLUS BINOP_MINUS
%token <Range.t * Types.token_data * Types.var_name> BINOP_HAT BINOP_AMP BINOP_BAR BINOP_GT BINOP_LT BINOP_EQ
%token <Range.t * Types.var_name> UNOP_EXCLAM
%token <Range.t * Types.token_data> EXACT_MINUS EXACT_TIMES MOD BEFORE LNOT EXACT_AMP EXACT_TILDE
%token <Range.t * Types.token_data> LPAREN RPAREN
%token <Range.t * Types.token_data> BVERTGRP EVERTGRP
%token <Range.t * Types.token_data> BHORZGRP EHORZGRP
%token <Range.t * Types.token_data> BMATHGRP EMATHGRP
%token <Range.t * Types.token_data> BPATH EPATH PATHLINE PATHCURVE CONTROLS CYCLE
%token <Range.t * Types.token_data> TRUE FALSE
%token <Range.t * Types.token_data> SEP ENDACTIVE COMMA
%token <Range.t * Types.token_data> BLIST LISTPUNCT ELIST CONS BRECORD ERECORD ACCESS
%token <Range.t * Types.token_data> WHILE DO
%token <Range.t * Types.token_data> HORZCMDTYPE VERTCMDTYPE MATHCMDTYPE
%token <Range.t * Types.token_data> OPTIONAL OMISSION OPTIONALTYPE OPTIONALARROW
%token <Range.t * Types.token_data * int> ITEM
%token <Range.t * Types.token_data * string> HEADER_REQUIRE HEADER_IMPORT
%token <Range.t * Types.token_data> HEADER_STAGE0 HEADER_STAGE1 HEADER_PERSISTENT0
%token <Types.token_data> EOI

%left  BINOP_BAR
%left  BINOP_AMP
%right BINOP_EQ BINOP_GT BINOP_LT
%right BINOP_HAT CONS
%right BINOP_PLUS
%left  BINOP_MINUS EXACT_MINUS
%right BINOP_TIMES EXACT_TIMES BINOP_DIVIDES MOD


%start main
%type <Types.stage * Types.header_element list * Types.untyped_abstract_tree> main

%%

optterm_list(sep, X):
  | { [] }
  | x=X; sep?
    { [x] }
  | x=X; sep; xs=optterm_nonempty_list(sep, X)
    { x :: xs }

optterm_nonempty_list(sep, X):
  | x=X; sep?
    { [x] }
  | x=X; sep; xs=optterm_nonempty_list(sep, X)
    { x :: xs }

main:
  | stage=stage; header=list(headerelem); utast=nxtoplevel { (stage, header, utast) }
  | stage=stage; header=list(headerelem); utast=nxwhl; EOI { (stage, header, utast) }
;
