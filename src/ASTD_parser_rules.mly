
%{

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

%}  

%token <string> BOOL
%token <string> ASTD_NAME
%token <string> TRANSITION_NAME
%token <string> VAR
%token <string> VAL
%token <int>    C_INT
%token <string> PREDICATE_NAME
%token LAMBDA AUTOMATA ELEM BEGIN_ASTD END_ASTD CALL TRUE FALSE
%token SEQUENCE CHOICE PARALLEL INTERLEAVE
%token LSYNCHRO RSYNCHRO
%token LENV RENV GUARD
%token KLEENE PLUS QMARK
%token LPAR RPAR LINT RINT LSET RSET 
%token COLON SCOLON COMMA 
%token IS EQUALS LINK
%token REMOVE LOCAL FROM_SUB TO_SUB
%token UNDERSCORE
%token EOF

%nonassoc QINTERLEAVE QSYNCHRO QCHOICE
%right PARALLEL INTERLEAVE SYNCHRO
%right CHOICE
%right SEQUENCE
%nonassoc GUARD
%nonassoc KLEENE PLUS QMARK 
%nonassoc CLOSURE
%nonassoc PAR_PE

%start structure
%start apply_event
%type <ASTD_astd.t> structure
%type <ASTD_event.t list> apply_event
%%

structure:
     |structure SCOLON astd
      { astd_parser_msg ("structure 1st choice");
        print_endline "========================================" ;
        ASTD_astd.global_save_astd $3 ;
        print_endline ("Registered: "^(ASTD_astd.get_name $3)) ;
        $3 
      }
     |astd
      { astd_parser_msg ("structure 2nd choice");
        ASTD_astd.global_save_astd $1 ;
        print_endline ("Registered: "^(ASTD_astd.get_name $1)) ;
        $1
      }
  ;


astd:
    |LPAR ASTD_NAME COMMA type_astd RPAR
      { astd_parser_msg ("astd 1st choice "^$2); let astd2 = ASTD_astd.rename_astd $4 $2 in begin astd2 end }
    |type_astd
      { astd_parser_msg ("astd 2nd choice");
        $1 }
    ;


type_astd:
    | astd_automata
      { astd_parser_msg ("type_astd automata "); $1 }
    | astd_choice
      { astd_parser_msg ("type_astd choix "); $1 }
    | astd_sequence
      { astd_parser_msg ("type_astd sequence "); $1 }
    | astd_kleene
      { astd_parser_msg ("type_astd kleene "); $1 }
    | astd_synchronisation
      { astd_parser_msg ("type_astd synchro "); $1 }
    | astd_qchoice
      { astd_parser_msg ("type_astd qchoice "); $1 }
    | astd_qsynchro
      { astd_parser_msg ("type_astd qsynch "); $1 }
    | astd_guard
      { astd_parser_msg ("type_astd guard "); $1 }
    | astd_call
      { astd_parser_msg ("type_astd call "); $1 }
    | ELEM
      { astd_parser_msg ("type_astd elem "); ASTD_astd.elem_of(ASTD_astd.give_name()) }
    ;


astd_automata:
    | BEGIN_ASTD AUTOMATA SCOLON list_of_meanings SCOLON list_of_arrows SCOLON list_of_names SCOLON ASTD_NAME END_ASTD
      { let (a,b)=$4 in ASTD_astd.automata_of (ASTD_astd.give_name ()) b $6 $8 $10 }
    ;


list_of_transitions :
    | LSET RSET
      { [] }
    | LSET list_of_transitions_content RSET
      { $2 }
;


list_of_transitions_content:
    | transition COMMA list_of_transitions_content
      { $1::$3 }
    | transition
      { $1::[] }
    ;


transition :
    | TRANSITION_NAME list_of_params_scheme
      { astd_parser_msg ("Transition construction " ^ $1); 
        ASTD_transition.transition (ASTD_label.of_string $1) $2 }
    | TRANSITION_NAME 
      { astd_parser_msg ("Transition without params construction " ^ $1); 
        ASTD_transition.transition (ASTD_label.of_string $1) [] }
    ;



list_of_params_scheme :
  |LPAR list_of_params_scheme_content RPAR
  {$2}
;

list_of_params_scheme_content :
    | VAR COMMA list_of_params_scheme_content
      { astd_parser_msg ("List of params "); 
        (ASTD_term.Var(ASTD_variable.of_string $1))::$3 }
    | VAR
      { astd_parser_msg ("List of params "); 
        (ASTD_term.Var(ASTD_variable.of_string $1))::[]  }
    | VAL COMMA list_of_params_scheme_content
      { astd_parser_msg ("List of params "); 
        (ASTD_term.Const(ASTD_constant.Symbol($1)))::$3 }
    | VAL
      { astd_parser_msg ("List of params "); 
        (ASTD_term.Const(ASTD_constant.Symbol($1)))::[]  }
    | C_INT COMMA list_of_params_scheme_content
      { astd_parser_msg ("List of params "); 
        (ASTD_term.Const(ASTD_constant.of_int ($1)))::$3 }
    | C_INT
      { astd_parser_msg ("List of params "); 
        (ASTD_term.Const(ASTD_constant.of_int ($1)))::[]  }
    | UNDERSCORE COMMA list_of_params_scheme_content
      { astd_parser_msg ("List of params "); 
        (ASTD_term.Const(ASTD_constant.Symbol("ANY VALUE")))::$3 }
    | UNDERSCORE
      { astd_parser_msg ("List of params "); 
        (ASTD_term.Const(ASTD_constant.Symbol("ANY VALUE")))::[]  }
    ;


list_of_names :
    |LSET list_of_names_content RSET
      { astd_parser_msg ("List of names "); 
        $2 }
    | LSET RSET
      { astd_parser_msg ("List of names "); 
        [] }
    ;


list_of_names_content :
    | ASTD_NAME COMMA list_of_names_content
      { $1::$3 }
    | ASTD_NAME
      { $1::[] }
    ;


list_of_meanings :
    | LSET list_of_meanings_content RSET
      { astd_parser_msg ("List of meanings "); 
        $2 }
    ;


list_of_meanings_content :
    | name_astd_link COMMA list_of_meanings_content
      { let (a,b)=$1 and (c,d)=$3 in (a::c,b::d) }
    | name_astd_link
      { let (a,b)=$1 in (a::[],b::[]) }
    ;


name_astd_link :
    | LPAR ASTD_NAME LINK astd RPAR
      { ($2,ASTD_astd.rename_astd $4 $2) }
    ;


list_of_arrows :
    | LSET RSET
      { astd_parser_msg ("List of arrows "); 
        [] }
    | LSET list_of_arrows_content RSET
      { astd_parser_msg ("List of arrows "); 
        $2 }
    ;


list_of_arrows_content :
    |  arrow COMMA list_of_arrows_content
      { astd_parser_msg ("arrow created "); 
        $1::$3 }
    |  arrow
      { astd_parser_msg ("arrow created "); 
        $1::[] }
    ;


arrow :
    | LPAR LPAR LOCAL COMMA ASTD_NAME COMMA ASTD_NAME RPAR COMMA arrow_end RPAR
      { astd_parser_msg ("arrow local "); 
        let (a,b,c)= $10 in ASTD_arrow.local_arrow $5 $7 a b c }
    | LPAR LPAR FROM_SUB COMMA ASTD_NAME COMMA ASTD_NAME COMMA ASTD_NAME RPAR COMMA arrow_end RPAR
      { astd_parser_msg ("arrow fsub"); 
        let (a,b,c)= $12 in ASTD_arrow.fsub_arrow $5 $7 $9 a b c }
    | LPAR LPAR TO_SUB COMMA ASTD_NAME COMMA ASTD_NAME COMMA ASTD_NAME RPAR COMMA arrow_end RPAR
      { astd_parser_msg ("arrow tsub"); 
        let (a,b,c)= $12 in ASTD_arrow.tsub_arrow $5 $7 $9 a b c }
    ;


arrow_end :
    | transition COMMA list_of_predicates COMMA TRUE
      { astd_parser_msg ("detail of arrow t"); 
        ($1,$3,true) }
    | transition COMMA list_of_predicates COMMA FALSE
      { astd_parser_msg ("detail of arrow f"); 
        ($1,$3,false) }
    ;


list_of_predicates :
    | LSET RSET 
      { astd_parser_msg ("List of predicates "); 
        (ASTD_predicate.predicate "none" (ASTD_term.parameters_of_variables []))::[]   }   
    | LSET list_of_predicates_content RSET
      {$2}
    ;


list_of_predicates_content :
    | predicate COMMA list_of_predicates_content
      { $1::$3 }
    | predicate
      { $1::[] }
    ;


predicate :
    | LINT PREDICATE_NAME list_of_params RINT 
      { ASTD_predicate.predicate $2 (ASTD_term.parameters_of_variables $3) }
    ;

list_of_params :
  |LPAR list_of_params_content RPAR
  {$2}
;

list_of_params_content :
    | VAR COMMA list_of_params_content
      { astd_parser_msg ("List of params "); 
        (ASTD_variable.of_string $1)::$3 }
    | VAR
      { astd_parser_msg ("List of params "); 
        (ASTD_variable.of_string $1)::[]  }
    ;


astd_sequence :
    | BEGIN_ASTD SEQUENCE SCOLON astd SCOLON astd END_ASTD
      { ASTD_astd.sequence_of (ASTD_astd.give_name ()) $4 $6 }
    ;


astd_choice :
    | BEGIN_ASTD CHOICE SCOLON astd SCOLON astd END_ASTD
      { astd_parser_msg ("astd_choice "); ASTD_astd.choice_of (ASTD_astd.give_name ()) $4 $6  }
    ;


astd_kleene :
    | BEGIN_ASTD KLEENE SCOLON astd END_ASTD
      { ASTD_astd.kleene_of (ASTD_astd.give_name ()) $4  }
    ;


astd_synchronisation :
    | BEGIN_ASTD LSYNCHRO RSYNCHRO SCOLON list_of_transitions SCOLON astd SCOLON astd END_ASTD
      { ASTD_astd.synchronisation_of (ASTD_astd.give_name ()) $5 $7 $9 }
    ;


astd_qchoice :
    | BEGIN_ASTD CHOICE COLON SCOLON VAR SCOLON complex_val_construction SCOLON astd END_ASTD
      { ASTD_astd.qchoice_of (ASTD_astd.give_name ()) (ASTD_variable.of_string $5) ($7) $9  }
    ;


complex_val_construction :
    |val_construction
      { $1 }
    |string_val_construction
      { ASTD_constant.order $1 }
    |val_construction REMOVE val_construction
      { ASTD_constant.remove_domain_from $1 $3 }       
    ;


val_construction : 
    | val_construction_range
      { $1 }
    | val_construction_explicit 
      { $1 }
    | val_construction_range PLUS val_construction
      { ASTD_constant.fusion $1 $3 }
    | val_construction_explicit PLUS val_construction
      { ASTD_constant.fusion $1 $3 }
    ;


val_construction_range :
    | LINT C_INT COMMA C_INT RINT
      { astd_parser_msg "Construction from range" ; 
        [ASTD_constant.range_of $2 $4] }
    ;


val_construction_explicit :
    | LSET list_val_content RSET 
      { astd_parser_msg "Explicit construction" ; 
        ASTD_constant.order $2 }
    ;


list_val_content :
    | C_INT COMMA list_val_content
      { (ASTD_constant.value_of(ASTD_constant.of_int $1))::$3 }
    | C_INT 
      { (ASTD_constant.value_of(ASTD_constant.of_int $1))::[] }
    ;

string_val_construction :
    | LSET string_list_content RSET 
      { astd_parser_msg "Explicit construction" ; 
        ASTD_constant.order $2 }
    ;


string_list_content :
    | VAL COMMA string_list_content
      { (ASTD_constant.value_of(ASTD_constant.Symbol ($1)))::$3 }
    | VAL
      { (ASTD_constant.value_of(ASTD_constant.Symbol ($1)))::[] }
    ;



astd_qsynchro :
    | BEGIN_ASTD LSYNCHRO RSYNCHRO COLON SCOLON VAR SCOLON complex_value_set_construction SCOLON list_of_transitions SCOLON astd END_ASTD
      {ASTD_astd.qsynchronisation_of (ASTD_astd.give_name ()) $6 (ASTD_term.parameters_of_constants $8) $10 $12  }
    ;



complex_value_set_construction :
    |value_set_construction
      { $1 }
    |value_set_construction REMOVE value_set_construction
      { ASTD_constant.remove_list_from $1 $3 }       
    ;


value_set_construction : 
    | value_set_construction_range
      { $1 }
    | value_set_construction_explicit 
      { $1 }
    | value_set_construction_range PLUS value_set_construction
      { ASTD_constant.add_list_to $1 $3 }
    | value_set_construction_explicit PLUS value_set_construction
      { ASTD_constant.add_list_to $1 $3 }
    ;


value_set_construction_range :
    | LINT C_INT COMMA C_INT RINT
      { astd_parser_msg "Construction from range" ; 
        ASTD_constant.const_list_of_range $2 $4 }
    ;


value_set_construction_explicit :
    | LSET list_of_value_content RSET 
      { astd_parser_msg "Explicit construction" ; 
        $2 }
    ;


list_of_value_content :
    | C_INT COMMA list_of_value_content
      { (ASTD_constant.of_int $1)::$3 }
    | C_INT 
      { (ASTD_constant.of_int $1)::[] }
    | VAL COMMA list_of_value_content
      { (ASTD_constant.Symbol ($1))::$3 }
    | VAL
      { (ASTD_constant.Symbol ($1))::[] }
    ;


astd_guard :
    | BEGIN_ASTD GUARD SCOLON list_of_predicates SCOLON astd END_ASTD
      { ASTD_astd.guard_of (ASTD_astd.give_name ()) $4 $6  }
    ;


astd_call :
    | BEGIN_ASTD CALL SCOLON ASTD_NAME SCOLON fct_vect END_ASTD
      { ASTD_astd.call_of (ASTD_astd.give_name ()) $4 $6 }
    ;


fct_vect :
    | LSET RSET
      { [] }     
    | LSET fct_vect_content RSET
      { $2 }
    ;


fct_vect_content :
    |fct_assoc COMMA fct_vect_content
      { $1::$3 }
    |fct_assoc
      { $1::[] }
    ;


fct_assoc :
    | LPAR VAR LINK C_INT RPAR
      { (ASTD_variable.of_string $2,ASTD_term.Const (ASTD_constant.of_int $4)) }
    ;
















apply_event:
  | apply_event SCOLON event_to_apply
     {$1@[$3]}
  | event_to_apply
     {$1::[]}
;




event_to_apply:
  | TRANSITION_NAME list_of_int
      { astd_parser_msg ("Event " ^ $1); 
        ASTD_event.event (ASTD_label.of_string $1) $2 }
  | TRANSITION_NAME 
      { astd_parser_msg ("Transition without params construction " ^ $1); 
        ASTD_event.event (ASTD_label.of_string $1) [] }
;



list_of_int:
  |LPAR list_of_value_content RPAR
      {$2}
;


