%{
  open OUnitConf_types
%}

%token <string * bool> BOOL
%token <string * int> INT
%token <string * float> FLOAT
%token <string> STRING
%token <Lexing.position * string> VAR 
%token SEMICOLON EQUAL EOF
%start main
%type <(Lexing.position * string * OUnitConf_types.data) list> main
%start data
%type <OUnitConf_types.data> data

%%

main:
  stmt_list EOF { List.rev $1 }
;

stmt_list:
  stmt_list stmt { $2 :: $1 }
|                { [] }
;

stmt:
  VAR EQUAL data_list SEMICOLON { let pos, var = $1 in (pos, var, $3) }
| VAR SEMICOLON                 { let pos, var = $1 in (pos, var, Unit) }
;

data_list:
  data_list data { data_merge $1 $2 }
| data           { $1 }
;

data:
  BOOL    { let str, v = $1 in Bool (str, v) }
| INT     { let str, v = $1 in Int (str, v) }
| FLOAT   { let str, v = $1 in Float (str, v) }
| STRING  { String $1 }
| VAR     { let _, var = $1 in Var var }
;

%%

