{
    open ASTD_parser_rules ;;
 
    let ebs_lexer_debug = true ;;
    let ebs_lexer_msg m = if (ebs_lexer_debug) 
                           then print_endline m 
                           else ignore m;;
}

let ws = ['\n''\t'' '' ']
let lowerletters = ['a'-'z']
let upperletters = ['A'-'Z']
let letters = lowerletters | upperletters
let digits = ['0'-'9']
let id = "Id"
let underscore = "_"

rule token = parse
 | ws    { token lexbuf }
 | '%'   { LAMBDA }
 | "aut"   { AUTOMATA }
 | '.'   { SEQUENCE }
 | '*'   { KLEENE }
 | '+'   { PLUS }
 | '?'   { QMARK }
 | "|["  { LSYNCHRO }
 | "]|"  { RSYNCHRO }
 | "|||" { INTERLEAVE }
 | "||"  { PARALLEL }
 | '|'   { CHOICE }
 | "=>"  { GUARD }
 | "->"  { LINK }
 | "(["  { LENV }
 | "])"  { RENV }
 | "call" { CALL }
 | "elem" { ELEM }
 | "True" { TRUE }
 | "False" { FALSE }
 | '-'   { REMOVE }
 | "local"  {LOCAL}
 | "to_sub" {TO_SUB}
 | "from_sub" {FROM_SUB}
 | lowerletters+ id?          { ebs_lexer_msg "new VAR" ; VAR     (Lexing.lexeme lexbuf) }
 | lowerletters* upperletters { ebs_lexer_msg "new VAL" ; VAL     (Lexing.lexeme lexbuf) }
 | digits+ as number          { ebs_lexer_msg "new C_INT" ; C_INT   (int_of_string number) } 
 | upperletters+ digits+      { ebs_lexer_msg "new ASTD_NAME" ; ASTD_NAME (Lexing.lexeme lexbuf) } 
 | upperletters lowerletters*	{ ebs_lexer_msg "new TRANSITION_NAME" ; TRANSITION_NAME (Lexing.lexeme lexbuf) }
 | letters+ 		      { ebs_lexer_msg "new PREDICATE_NAME" ; PREDICATE_NAME (Lexing.lexeme lexbuf) }
 | '<'   { BEGIN_ASTD }
 | '>'   { END_ASTD } 
 | '['   { ebs_lexer_msg "new LINT" ; LINT }
 | ']'   { ebs_lexer_msg "new RINT" ; RINT }
 | '{'   { ebs_lexer_msg "new LSET" ; LSET }
 | '}'   { ebs_lexer_msg "new RSET" ; RSET }
 | '('   { ebs_lexer_msg "new LPAR" ; LPAR }
 | ')'   { ebs_lexer_msg "new RPAR" ; RPAR }
 | ':'   { ebs_lexer_msg "new COLON" ; COLON }
 | ';'   { ebs_lexer_msg "new SCOLON" ; SCOLON }
 | ','   { ebs_lexer_msg "new COMMA" ; COMMA }
 | ":="  { IS }
 | '='   { EQUALS }
 | eof   { EOF }
