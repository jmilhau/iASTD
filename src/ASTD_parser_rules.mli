type token =
  | BOOL of (string)
  | ASTD_NAME of (string)
  | TRANSITION_NAME of (string)
  | VAR of (string)
  | VAL of (string)
  | C_INT of (int)
  | PREDICATE_NAME of (string)
  | LAMBDA
  | AUTOMATA
  | ELEM
  | BEGIN_ASTD
  | END_ASTD
  | CALL
  | TRUE
  | FALSE
  | SEQUENCE
  | CHOICE
  | PARALLEL
  | INTERLEAVE
  | LSYNCHRO
  | RSYNCHRO
  | LENV
  | RENV
  | GUARD
  | KLEENE
  | PLUS
  | QMARK
  | LPAR
  | RPAR
  | LINT
  | RINT
  | LSET
  | RSET
  | COLON
  | SCOLON
  | COMMA
  | IS
  | EQUALS
  | LINK
  | REMOVE
  | LOCAL
  | FROM_SUB
  | TO_SUB
  | EOF

val structure :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ASTD_astd.t
val apply_event :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ASTD_event.t list
