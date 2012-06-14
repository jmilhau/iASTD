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

open Parsing;;
# 3 "ASTD_parser_rules.mly"

	open ASTD_variable;;
        open ASTD_constant;;
        open ASTD_term;;
	open ASTD_label;;
        open ASTD_environment;;
	open ASTD_predicate;;
        open ASTD_predicate_definitions;;
        open ASTD_transition;;
	open ASTD_event;;
	open ASTD_arrow;;
	open ASTD_astd;; 

    let astd_parser_debug = true ;;
    let astd_parser_msg m = if (astd_parser_debug) 
                            then (print_endline m )
                            else (ignore m);;

# 68 "ASTD_parser_rules.ml"
let yytransl_const = [|
  264 (* LAMBDA *);
  265 (* AUTOMATA *);
  266 (* ELEM *);
  267 (* BEGIN_ASTD *);
  268 (* END_ASTD *);
  269 (* CALL *);
  270 (* TRUE *);
  271 (* FALSE *);
  272 (* SEQUENCE *);
  273 (* CHOICE *);
  274 (* PARALLEL *);
  275 (* INTERLEAVE *);
  276 (* LSYNCHRO *);
  277 (* RSYNCHRO *);
  278 (* LENV *);
  279 (* RENV *);
  280 (* GUARD *);
  281 (* KLEENE *);
  282 (* PLUS *);
  283 (* QMARK *);
  284 (* LPAR *);
  285 (* RPAR *);
  286 (* LINT *);
  287 (* RINT *);
  288 (* LSET *);
  289 (* RSET *);
  290 (* COLON *);
  291 (* SCOLON *);
  292 (* COMMA *);
  293 (* IS *);
  294 (* EQUALS *);
  295 (* LINK *);
  296 (* REMOVE *);
  297 (* LOCAL *);
  298 (* FROM_SUB *);
  299 (* TO_SUB *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* BOOL *);
  258 (* ASTD_NAME *);
  259 (* TRANSITION_NAME *);
  260 (* VAR *);
  261 (* VAL *);
  262 (* C_INT *);
  263 (* PREDICATE_NAME *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\005\000\017\000\
\017\000\018\000\018\000\019\000\019\000\016\000\016\000\021\000\
\021\000\014\000\022\000\022\000\023\000\015\000\015\000\024\000\
\024\000\025\000\025\000\025\000\026\000\026\000\027\000\027\000\
\028\000\028\000\029\000\020\000\030\000\030\000\007\000\006\000\
\008\000\009\000\010\000\031\000\031\000\032\000\032\000\032\000\
\032\000\033\000\034\000\035\000\035\000\035\000\035\000\011\000\
\012\000\013\000\036\000\036\000\037\000\037\000\038\000\002\000\
\002\000\039\000\039\000\040\000\000\000\000\000"

let yylen = "\002\000\
\003\000\001\000\005\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\011\000\002\000\
\003\000\003\000\001\000\002\000\001\000\003\000\002\000\003\000\
\001\000\003\000\003\000\001\000\005\000\002\000\003\000\003\000\
\001\000\011\000\013\000\013\000\005\000\005\000\002\000\003\000\
\003\000\001\000\004\000\003\000\003\000\001\000\007\000\007\000\
\005\000\010\000\010\000\001\000\003\000\001\000\001\000\003\000\
\003\000\005\000\003\000\003\000\001\000\003\000\001\000\013\000\
\007\000\007\000\002\000\003\000\003\000\001\000\005\000\003\000\
\001\000\002\000\001\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\014\000\000\000\000\000\000\000\002\000\
\004\000\005\000\006\000\007\000\008\000\009\000\010\000\011\000\
\012\000\013\000\000\000\000\000\073\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\074\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\072\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\076\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\039\000\000\000\000\000\000\000\049\000\003\000\
\062\000\060\000\000\000\026\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\
\000\000\000\000\000\000\040\000\000\000\000\000\000\000\027\000\
\000\000\030\000\000\000\000\000\000\000\000\000\067\000\000\000\
\000\000\066\000\047\000\000\000\000\000\000\000\000\000\000\000\
\000\000\048\000\000\000\000\000\020\000\017\000\000\000\000\000\
\000\000\041\000\065\000\000\000\000\000\031\000\000\000\000\000\
\000\000\000\000\068\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\043\000\
\029\000\000\000\000\000\000\000\032\000\000\000\023\000\000\000\
\000\000\000\000\069\000\000\000\059\000\000\000\053\000\056\000\
\057\000\000\000\000\000\044\000\000\000\000\000\000\000\000\000\
\000\000\022\000\000\000\000\000\000\000\051\000\000\000\045\000\
\050\000\000\000\000\000\000\000\024\000\015\000\071\000\058\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\064\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\000\036\000\037\000\038\000"

let yydgoto = "\003\000\
\007\000\020\000\008\000\009\000\010\000\011\000\012\000\013\000\
\014\000\015\000\016\000\017\000\018\000\049\000\087\000\137\000\
\073\000\096\000\208\000\125\000\160\000\064\000\065\000\107\000\
\108\000\209\000\057\000\076\000\077\000\149\000\118\000\119\000\
\120\000\121\000\046\000\089\000\112\000\113\000\021\000\032\000"

let yysindex = "\032\000\
\255\254\016\255\000\000\000\000\005\255\040\255\245\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\255\019\255\000\000\032\255\039\255\041\255\
\022\255\037\255\042\255\043\255\044\255\255\254\056\255\000\000\
\016\255\047\255\073\255\255\254\046\255\255\254\029\255\050\255\
\255\254\055\255\000\000\048\255\049\255\054\255\000\000\058\255\
\052\255\053\255\059\255\085\255\060\255\061\255\065\255\239\254\
\063\255\078\255\062\255\056\255\056\255\000\000\090\255\066\255\
\064\255\069\255\070\255\255\254\068\255\255\254\089\255\004\255\
\071\255\097\255\000\000\072\255\074\255\255\254\000\000\000\000\
\000\000\000\000\075\255\000\000\058\255\240\254\076\255\243\254\
\095\255\096\255\023\255\100\255\080\255\081\255\000\000\083\255\
\082\255\255\254\081\255\000\000\087\255\107\255\255\254\000\000\
\092\255\000\000\091\255\086\255\093\255\119\255\000\000\094\255\
\098\255\000\000\000\000\120\255\056\255\101\255\088\255\103\255\
\104\255\000\000\023\255\127\255\000\000\000\000\129\255\102\255\
\108\255\000\000\000\000\106\255\009\255\000\000\105\255\002\255\
\109\255\099\255\000\000\112\255\110\255\114\255\255\254\023\255\
\023\255\023\255\113\255\115\255\116\255\000\000\255\254\000\000\
\000\000\117\255\118\255\121\255\000\000\122\255\000\000\123\255\
\139\255\136\255\000\000\137\255\000\000\138\255\000\000\000\000\
\000\000\065\255\127\255\000\000\140\255\147\255\153\255\158\255\
\159\255\000\000\150\255\130\255\133\255\000\000\131\255\000\000\
\000\000\132\255\134\255\135\255\000\000\000\000\000\000\000\000\
\255\254\163\255\165\255\167\255\160\255\144\255\141\255\142\255\
\000\000\143\255\172\255\173\255\129\255\151\255\152\255\146\255\
\154\255\148\255\149\255\050\255\000\000\129\255\129\255\155\255\
\161\255\164\255\057\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\176\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\186\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\255\010\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\156\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\162\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\255\000\000\000\000\
\166\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\168\255\000\000\000\000\000\000\000\000\
\169\255\000\000\000\000\000\000\000\000\000\000\046\255\244\254\
\247\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\171\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\170\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\226\255\145\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\000\065\000\189\255\095\000\019\000\112\000\000\000\063\000\
\000\000\111\255\248\255\104\000\000\000\035\000\084\000\157\255\
\000\000\000\000\198\255\000\000\068\000\000\000\177\000\000\000"

let yytablesize = 292
let yytable = "\043\000\
\075\000\081\000\082\000\158\000\097\000\051\000\094\000\053\000\
\004\000\005\000\058\000\105\000\074\000\022\000\110\000\075\000\
\106\000\023\000\019\000\111\000\024\000\025\000\054\000\030\000\
\026\000\055\000\006\000\054\000\027\000\028\000\055\000\063\000\
\001\000\002\000\159\000\063\000\095\000\090\000\061\000\092\000\
\021\000\029\000\061\000\021\000\167\000\168\000\169\000\102\000\
\031\000\154\000\155\000\156\000\116\000\033\000\117\000\037\000\
\038\000\039\000\142\000\097\000\044\000\045\000\054\000\055\000\
\004\000\005\000\034\000\128\000\217\000\218\000\222\000\223\000\
\132\000\035\000\050\000\036\000\040\000\041\000\048\000\042\000\
\052\000\056\000\062\000\060\000\061\000\063\000\066\000\067\000\
\069\000\079\000\080\000\083\000\093\000\068\000\070\000\071\000\
\072\000\078\000\084\000\085\000\086\000\088\000\091\000\099\000\
\100\000\098\000\114\000\115\000\124\000\101\000\109\000\122\000\
\166\000\103\000\123\000\126\000\074\000\127\000\131\000\133\000\
\173\000\135\000\138\000\134\000\136\000\141\000\139\000\144\000\
\145\000\146\000\148\000\094\000\105\000\140\000\153\000\143\000\
\151\000\162\000\152\000\110\000\179\000\180\000\181\000\161\000\
\172\000\164\000\165\000\170\000\186\000\182\000\171\000\185\000\
\174\000\175\000\187\000\178\000\176\000\177\000\191\000\188\000\
\158\000\190\000\197\000\192\000\198\000\193\000\199\000\194\000\
\200\000\195\000\196\000\201\000\202\000\206\000\207\000\077\000\
\203\000\204\000\205\000\210\000\211\000\212\000\213\000\214\000\
\215\000\078\000\059\000\183\000\028\000\220\000\219\000\150\000\
\221\000\129\000\042\000\189\000\104\000\157\000\019\000\046\000\
\033\000\070\000\025\000\216\000\130\000\184\000\147\000\163\000\
\000\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\075\000"

let yycheck = "\030\000\
\000\000\060\000\061\000\002\001\072\000\036\000\003\001\038\000\
\010\001\011\001\041\000\028\001\030\001\009\001\028\001\033\001\
\033\001\013\001\003\001\033\001\016\001\017\001\035\001\035\001\
\020\001\035\001\028\001\040\001\024\001\025\001\040\001\029\001\
\001\000\002\000\033\001\033\001\033\001\068\000\029\001\070\000\
\033\001\002\001\033\001\036\001\144\000\145\000\146\000\078\000\
\028\001\041\001\042\001\043\001\030\001\035\001\032\001\034\001\
\035\001\021\001\117\000\127\000\005\001\006\001\034\001\035\001\
\010\001\011\001\035\001\098\000\214\000\215\000\014\001\015\001\
\103\000\035\001\002\001\035\001\035\001\035\001\032\001\036\001\
\035\001\032\001\029\001\036\001\036\001\028\001\035\001\035\001\
\004\001\012\001\029\001\002\001\004\001\035\001\035\001\035\001\
\032\001\035\001\033\001\036\001\032\001\032\001\035\001\007\001\
\033\001\035\001\012\001\012\001\028\001\036\001\035\001\012\001\
\143\000\039\001\035\001\033\001\030\001\036\001\012\001\028\001\
\151\000\036\001\004\001\033\001\032\001\006\001\033\001\040\001\
\026\001\026\001\004\001\003\001\028\001\036\001\029\001\035\001\
\035\001\039\001\031\001\028\001\002\001\006\001\006\001\035\001\
\029\001\036\001\033\001\035\001\002\001\012\001\036\001\012\001\
\036\001\036\001\002\001\033\001\036\001\036\001\029\001\002\001\
\002\001\012\001\193\000\031\001\002\001\035\001\002\001\036\001\
\002\001\036\001\036\001\012\001\029\001\002\001\002\001\000\000\
\036\001\036\001\036\001\029\001\029\001\036\001\029\001\036\001\
\036\001\000\000\042\000\170\000\033\001\029\001\036\001\127\000\
\029\001\099\000\033\001\177\000\085\000\135\000\033\001\029\001\
\033\001\033\001\033\001\212\000\101\000\171\000\123\000\140\000\
\255\255\033\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\035\001"

let yynames_const = "\
  LAMBDA\000\
  AUTOMATA\000\
  ELEM\000\
  BEGIN_ASTD\000\
  END_ASTD\000\
  CALL\000\
  TRUE\000\
  FALSE\000\
  SEQUENCE\000\
  CHOICE\000\
  PARALLEL\000\
  INTERLEAVE\000\
  LSYNCHRO\000\
  RSYNCHRO\000\
  LENV\000\
  RENV\000\
  GUARD\000\
  KLEENE\000\
  PLUS\000\
  QMARK\000\
  LPAR\000\
  RPAR\000\
  LINT\000\
  RINT\000\
  LSET\000\
  RSET\000\
  COLON\000\
  SCOLON\000\
  COMMA\000\
  IS\000\
  EQUALS\000\
  LINK\000\
  REMOVE\000\
  LOCAL\000\
  FROM_SUB\000\
  TO_SUB\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL\000\
  ASTD_NAME\000\
  TRANSITION_NAME\000\
  VAR\000\
  VAL\000\
  C_INT\000\
  PREDICATE_NAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ASTD_astd.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'astd) in
    Obj.repr(
# 58 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("structure 1st choice");
        print_endline "========================================" ;
        ASTD_astd.global_save_astd _3 ;
        print_endline ("Registered: "^(ASTD_astd.get_name _3)) ;
        _3 
      )
# 389 "ASTD_parser_rules.ml"
               : ASTD_astd.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'astd) in
    Obj.repr(
# 65 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("structure 2nd choice");
        ASTD_astd.global_save_astd _1 ;
        print_endline ("Registered: "^(ASTD_astd.get_name _1)) ;
        _1
      )
# 400 "ASTD_parser_rules.ml"
               : ASTD_astd.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'type_astd) in
    Obj.repr(
# 75 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("astd 1st choice "^_2); let astd2 = ASTD_astd.rename_astd _4 _2 in begin astd2 end )
# 408 "ASTD_parser_rules.ml"
               : 'astd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_astd) in
    Obj.repr(
# 77 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("astd 2nd choice");
        _1 )
# 416 "ASTD_parser_rules.ml"
               : 'astd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'astd_automata) in
    Obj.repr(
# 84 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("type_astd automata "); _1 )
# 423 "ASTD_parser_rules.ml"
               : 'type_astd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'astd_choice) in
    Obj.repr(
# 86 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("type_astd choix "); _1 )
# 430 "ASTD_parser_rules.ml"
               : 'type_astd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'astd_sequence) in
    Obj.repr(
# 88 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("type_astd sequence "); _1 )
# 437 "ASTD_parser_rules.ml"
               : 'type_astd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'astd_kleene) in
    Obj.repr(
# 90 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("type_astd kleene "); _1 )
# 444 "ASTD_parser_rules.ml"
               : 'type_astd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'astd_synchronisation) in
    Obj.repr(
# 92 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("type_astd synchro "); _1 )
# 451 "ASTD_parser_rules.ml"
               : 'type_astd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'astd_qchoice) in
    Obj.repr(
# 94 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("type_astd qchoice "); _1 )
# 458 "ASTD_parser_rules.ml"
               : 'type_astd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'astd_qsynchro) in
    Obj.repr(
# 96 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("type_astd qsynch "); _1 )
# 465 "ASTD_parser_rules.ml"
               : 'type_astd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'astd_guard) in
    Obj.repr(
# 98 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("type_astd guard "); _1 )
# 472 "ASTD_parser_rules.ml"
               : 'type_astd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'astd_call) in
    Obj.repr(
# 100 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("type_astd call "); _1 )
# 479 "ASTD_parser_rules.ml"
               : 'type_astd))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("type_astd elem "); ASTD_astd.elem_of(ASTD_astd.give_name()) )
# 485 "ASTD_parser_rules.ml"
               : 'type_astd))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'list_of_meanings) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'list_of_arrows) in
    let _8 = (Parsing.peek_val __caml_parser_env 3 : 'list_of_names) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 108 "ASTD_parser_rules.mly"
      ( let (a,b)=_4 in ASTD_astd.automata_of (ASTD_astd.give_name ()) b _6 _8 _10 )
# 495 "ASTD_parser_rules.ml"
               : 'astd_automata))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "ASTD_parser_rules.mly"
      ( [] )
# 501 "ASTD_parser_rules.ml"
               : 'list_of_transitions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_transitions_content) in
    Obj.repr(
# 116 "ASTD_parser_rules.mly"
      ( _2 )
# 508 "ASTD_parser_rules.ml"
               : 'list_of_transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'transition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_transitions_content) in
    Obj.repr(
# 122 "ASTD_parser_rules.mly"
      ( _1::_3 )
# 516 "ASTD_parser_rules.ml"
               : 'list_of_transitions_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 124 "ASTD_parser_rules.mly"
      ( _1::[] )
# 523 "ASTD_parser_rules.ml"
               : 'list_of_transitions_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_params) in
    Obj.repr(
# 130 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("Transition construction " ^ _1); 
        ASTD_transition.transition (ASTD_label.of_string _1) (ASTD_term.parameters_of_variables _2) )
# 532 "ASTD_parser_rules.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("Transition without params construction " ^ _1); 
        ASTD_transition.transition (ASTD_label.of_string _1) (ASTD_term.parameters_of_variables []) )
# 540 "ASTD_parser_rules.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_names_content) in
    Obj.repr(
# 140 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("List of names "); 
        _2 )
# 548 "ASTD_parser_rules.ml"
               : 'list_of_names))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("List of names "); 
        [] )
# 555 "ASTD_parser_rules.ml"
               : 'list_of_names))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_names_content) in
    Obj.repr(
# 150 "ASTD_parser_rules.mly"
      ( _1::_3 )
# 563 "ASTD_parser_rules.ml"
               : 'list_of_names_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 152 "ASTD_parser_rules.mly"
      ( _1::[] )
# 570 "ASTD_parser_rules.ml"
               : 'list_of_names_content))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_meanings_content) in
    Obj.repr(
# 158 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("List of meanings "); 
        _2 )
# 578 "ASTD_parser_rules.ml"
               : 'list_of_meanings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'name_astd_link) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_meanings_content) in
    Obj.repr(
# 165 "ASTD_parser_rules.mly"
      ( let (a,b)=_1 and (c,d)=_3 in (a::c,b::d) )
# 586 "ASTD_parser_rules.ml"
               : 'list_of_meanings_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name_astd_link) in
    Obj.repr(
# 167 "ASTD_parser_rules.mly"
      ( let (a,b)=_1 in (a::[],b::[]) )
# 593 "ASTD_parser_rules.ml"
               : 'list_of_meanings_content))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'astd) in
    Obj.repr(
# 173 "ASTD_parser_rules.mly"
      ( (_2,ASTD_astd.rename_astd _4 _2) )
# 601 "ASTD_parser_rules.ml"
               : 'name_astd_link))
; (fun __caml_parser_env ->
    Obj.repr(
# 179 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("List of arrows "); 
        [] )
# 608 "ASTD_parser_rules.ml"
               : 'list_of_arrows))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_arrows_content) in
    Obj.repr(
# 182 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("List of arrows "); 
        _2 )
# 616 "ASTD_parser_rules.ml"
               : 'list_of_arrows))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arrow) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_arrows_content) in
    Obj.repr(
# 189 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("arrow created "); 
        _1::_3 )
# 625 "ASTD_parser_rules.ml"
               : 'list_of_arrows_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arrow) in
    Obj.repr(
# 192 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("arrow created "); 
        _1::[] )
# 633 "ASTD_parser_rules.ml"
               : 'list_of_arrows_content))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'arrow_end) in
    Obj.repr(
# 199 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("arrow local "); 
        let (a,b,c)= _10 in ASTD_arrow.local_arrow _5 _7 a b c )
# 643 "ASTD_parser_rules.ml"
               : 'arrow))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _12 = (Parsing.peek_val __caml_parser_env 1 : 'arrow_end) in
    Obj.repr(
# 202 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("arrow fsub"); 
        let (a,b,c)= _12 in ASTD_arrow.fsub_arrow _5 _7 _9 a b c )
# 654 "ASTD_parser_rules.ml"
               : 'arrow))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _12 = (Parsing.peek_val __caml_parser_env 1 : 'arrow_end) in
    Obj.repr(
# 205 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("arrow tsub"); 
        let (a,b,c)= _12 in ASTD_arrow.tsub_arrow _5 _7 _9 a b c )
# 665 "ASTD_parser_rules.ml"
               : 'arrow))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'transition) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'list_of_predicates) in
    Obj.repr(
# 212 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("detail of arrow t"); 
        (_1,_3,true) )
# 674 "ASTD_parser_rules.ml"
               : 'arrow_end))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'transition) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'list_of_predicates) in
    Obj.repr(
# 215 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("detail of arrow f"); 
        (_1,_3,false) )
# 683 "ASTD_parser_rules.ml"
               : 'arrow_end))
; (fun __caml_parser_env ->
    Obj.repr(
# 222 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("List of predicates "); 
        (ASTD_predicate.predicate "none" (ASTD_term.parameters_of_variables []))::[]   )
# 690 "ASTD_parser_rules.ml"
               : 'list_of_predicates))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_predicates_content) in
    Obj.repr(
# 225 "ASTD_parser_rules.mly"
      (_2)
# 697 "ASTD_parser_rules.ml"
               : 'list_of_predicates))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'predicate) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_predicates_content) in
    Obj.repr(
# 231 "ASTD_parser_rules.mly"
      ( _1::_3 )
# 705 "ASTD_parser_rules.ml"
               : 'list_of_predicates_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 233 "ASTD_parser_rules.mly"
      ( _1::[] )
# 712 "ASTD_parser_rules.ml"
               : 'list_of_predicates_content))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_params) in
    Obj.repr(
# 239 "ASTD_parser_rules.mly"
      ( ASTD_predicate.predicate _2 (ASTD_term.parameters_of_variables _3) )
# 720 "ASTD_parser_rules.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_params_content) in
    Obj.repr(
# 244 "ASTD_parser_rules.mly"
  (_2)
# 727 "ASTD_parser_rules.ml"
               : 'list_of_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_params_content) in
    Obj.repr(
# 249 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("List of params "); 
        (ASTD_variable.of_string _1)::_3 )
# 736 "ASTD_parser_rules.ml"
               : 'list_of_params_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 252 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("List of params "); 
        (ASTD_variable.of_string _1)::[]  )
# 744 "ASTD_parser_rules.ml"
               : 'list_of_params_content))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'astd) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'astd) in
    Obj.repr(
# 259 "ASTD_parser_rules.mly"
      ( ASTD_astd.sequence_of (ASTD_astd.give_name ()) _4 _6 )
# 752 "ASTD_parser_rules.ml"
               : 'astd_sequence))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'astd) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'astd) in
    Obj.repr(
# 265 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("astd_choice "); ASTD_astd.choice_of (ASTD_astd.give_name ()) _4 _6  )
# 760 "ASTD_parser_rules.ml"
               : 'astd_choice))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'astd) in
    Obj.repr(
# 271 "ASTD_parser_rules.mly"
      ( ASTD_astd.kleene_of (ASTD_astd.give_name ()) _4  )
# 767 "ASTD_parser_rules.ml"
               : 'astd_kleene))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'list_of_transitions) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'astd) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'astd) in
    Obj.repr(
# 277 "ASTD_parser_rules.mly"
      ( ASTD_astd.synchronisation_of (ASTD_astd.give_name ()) _5 _7 _9 )
# 776 "ASTD_parser_rules.ml"
               : 'astd_synchronisation))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'complex_value_set_construction) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'astd) in
    Obj.repr(
# 283 "ASTD_parser_rules.mly"
      ( ASTD_astd.qchoice_of (ASTD_astd.give_name ()) (ASTD_variable.of_string _5) (ASTD_term.parameters_of_constants _7) _9  )
# 785 "ASTD_parser_rules.ml"
               : 'astd_qchoice))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value_set_construction) in
    Obj.repr(
# 289 "ASTD_parser_rules.mly"
      ( _1 )
# 792 "ASTD_parser_rules.ml"
               : 'complex_value_set_construction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value_set_construction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value_set_construction) in
    Obj.repr(
# 291 "ASTD_parser_rules.mly"
      ( ASTD_constant.remove_list_from _1 _3 )
# 800 "ASTD_parser_rules.ml"
               : 'complex_value_set_construction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value_set_construction_range) in
    Obj.repr(
# 297 "ASTD_parser_rules.mly"
      ( _1 )
# 807 "ASTD_parser_rules.ml"
               : 'value_set_construction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value_set_construction_explicit) in
    Obj.repr(
# 299 "ASTD_parser_rules.mly"
      ( _1 )
# 814 "ASTD_parser_rules.ml"
               : 'value_set_construction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value_set_construction_range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value_set_construction) in
    Obj.repr(
# 301 "ASTD_parser_rules.mly"
      ( ASTD_constant.add_list_to _1 _3 )
# 822 "ASTD_parser_rules.ml"
               : 'value_set_construction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value_set_construction_explicit) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value_set_construction) in
    Obj.repr(
# 303 "ASTD_parser_rules.mly"
      ( ASTD_constant.add_list_to _1 _3 )
# 830 "ASTD_parser_rules.ml"
               : 'value_set_construction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 309 "ASTD_parser_rules.mly"
      ( astd_parser_msg "Construction from range" ; 
        ASTD_constant.const_list_of_range _2 _4 )
# 839 "ASTD_parser_rules.ml"
               : 'value_set_construction_range))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_value_content) in
    Obj.repr(
# 316 "ASTD_parser_rules.mly"
      ( astd_parser_msg "Explicit construction" ; 
        _2 )
# 847 "ASTD_parser_rules.ml"
               : 'value_set_construction_explicit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_value_content) in
    Obj.repr(
# 323 "ASTD_parser_rules.mly"
      ( (ASTD_constant.of_int _1)::_3 )
# 855 "ASTD_parser_rules.ml"
               : 'list_of_value_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 325 "ASTD_parser_rules.mly"
      ( (ASTD_constant.of_int _1)::[] )
# 862 "ASTD_parser_rules.ml"
               : 'list_of_value_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_value_content) in
    Obj.repr(
# 327 "ASTD_parser_rules.mly"
      ( (ASTD_constant.Symbol (_1))::_3 )
# 870 "ASTD_parser_rules.ml"
               : 'list_of_value_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 329 "ASTD_parser_rules.mly"
      ( (ASTD_constant.Symbol (_1))::[] )
# 877 "ASTD_parser_rules.ml"
               : 'list_of_value_content))
; (fun __caml_parser_env ->
    let _6 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 5 : 'complex_value_set_construction) in
    let _10 = (Parsing.peek_val __caml_parser_env 3 : 'list_of_transitions) in
    let _12 = (Parsing.peek_val __caml_parser_env 1 : 'astd) in
    Obj.repr(
# 337 "ASTD_parser_rules.mly"
      (ASTD_astd.qsynchronisation_of (ASTD_astd.give_name ()) _6 (ASTD_term.parameters_of_constants _8) _10 _12  )
# 887 "ASTD_parser_rules.ml"
               : 'astd_qsynchro))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'list_of_predicates) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'astd) in
    Obj.repr(
# 343 "ASTD_parser_rules.mly"
      ( ASTD_astd.guard_of (ASTD_astd.give_name ()) _4 _6  )
# 895 "ASTD_parser_rules.ml"
               : 'astd_guard))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'fct_vect) in
    Obj.repr(
# 349 "ASTD_parser_rules.mly"
      ( ASTD_astd.call_of (ASTD_astd.give_name ()) _4 _6 )
# 903 "ASTD_parser_rules.ml"
               : 'astd_call))
; (fun __caml_parser_env ->
    Obj.repr(
# 355 "ASTD_parser_rules.mly"
      ( [] )
# 909 "ASTD_parser_rules.ml"
               : 'fct_vect))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fct_vect_content) in
    Obj.repr(
# 357 "ASTD_parser_rules.mly"
      ( _2 )
# 916 "ASTD_parser_rules.ml"
               : 'fct_vect))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fct_assoc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fct_vect_content) in
    Obj.repr(
# 363 "ASTD_parser_rules.mly"
      ( _1::_3 )
# 924 "ASTD_parser_rules.ml"
               : 'fct_vect_content))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fct_assoc) in
    Obj.repr(
# 365 "ASTD_parser_rules.mly"
      ( _1::[] )
# 931 "ASTD_parser_rules.ml"
               : 'fct_vect_content))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 371 "ASTD_parser_rules.mly"
      ( (ASTD_variable.of_string _2,ASTD_term.Const (ASTD_constant.of_int _4)) )
# 939 "ASTD_parser_rules.ml"
               : 'fct_assoc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ASTD_event.t list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'event_to_apply) in
    Obj.repr(
# 391 "ASTD_parser_rules.mly"
     (_1@[_3])
# 947 "ASTD_parser_rules.ml"
               : ASTD_event.t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'event_to_apply) in
    Obj.repr(
# 393 "ASTD_parser_rules.mly"
     (_1::[])
# 954 "ASTD_parser_rules.ml"
               : ASTD_event.t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_of_int) in
    Obj.repr(
# 401 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("Event " ^ _1); 
        ASTD_event.event (ASTD_label.of_string _1) _2 )
# 963 "ASTD_parser_rules.ml"
               : 'event_to_apply))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 404 "ASTD_parser_rules.mly"
      ( astd_parser_msg ("Transition without params construction " ^ _1); 
        ASTD_event.event (ASTD_label.of_string _1) [] )
# 971 "ASTD_parser_rules.ml"
               : 'event_to_apply))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_of_value_content) in
    Obj.repr(
# 412 "ASTD_parser_rules.mly"
      (_2)
# 978 "ASTD_parser_rules.ml"
               : 'list_of_int))
(* Entry structure *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry apply_event *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let structure (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : ASTD_astd.t)
let apply_event (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : ASTD_event.t list)
