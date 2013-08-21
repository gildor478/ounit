{
  open OUnitConf_parser
}

rule token = parse
  [' ' '\t']     { token lexbuf }     (* skip blanks *)
| ['\n' ]        { Lexing.new_line lexbuf; token lexbuf }
| ';'            { SEMICOLON }
| '='            { EQUAL }
| eof            { EOF }
| "true"         { BOOL ("true", true) }
| "false"        { BOOL ("false", false) }
| ['0'-'9']+'.'['0'-'9']* as lxm
    { FLOAT (lxm, float_of_string lxm) }
| ['0'-'9']+ as lxm
    { INT (lxm, int_of_string lxm) }
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as lxm
    { VAR (lexbuf.Lexing.lex_curr_p, lxm) }
| '"' { string (Buffer.create 13) lexbuf }

and string buf = parse
| "\\\"" { Buffer.add_char buf '"'; string buf lexbuf }
| "\\\\" { Buffer.add_char buf '\\'; string buf lexbuf }
| '"'    { STRING (Buffer.contents buf) }
| '\n'   { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; string buf lexbuf }
| _ as c { Buffer.add_char buf c; string buf lexbuf }
