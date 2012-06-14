(* Converts an ASTD state in tuples for a database storage  *)  

open ASTD_astd
open ASTD_state
open ASTD_constant
open Sqlite3

exception DBError of string 

let val_debug = ref false;;
let debug m = if (!val_debug) 
                        then (print_endline m )
                        else begin end;;
let debug_on () = (val_debug := true);;


let size = 50 
let historyTable = "history" 
let mapTable = "map" 
let domainTable = "domain" 
let stateTable = "state" 

let db = Sqlite3.db_open "aaaASTDstates"

let executeQuery callbackF sql = 
      begin debug ("SQL : "^ sql); 
      try let res = Sqlite3.exec db sql ~cb:(callbackF) in
        match res with
        | Sqlite3.Rc.OK -> ()
        | r -> prerr_endline ("Sqlite3 ERROR : "^Sqlite3.Rc.to_string r^". "^ Sqlite3.errmsg db);    
      with | Sqlite3.Error s -> prerr_endline ("Sqlite3 ERROR : "^ s)
           | DBError s -> prerr_endline ("State2DB ERROR : "^ s)  
      end
           
let executeQueries callbackF sqls = List.iter (executeQuery callbackF) sqls 

let emptyCB row h =  () 

let constant2String a = match a with | Integer a -> string_of_int a | Symbol a -> a | _ -> ""
let constant2Type a = match a with | Integer a -> "integer" | Symbol a -> "string" | _ -> ""

let createDB = [ "DROP TABLE IF EXISTS "^stateTable ; "DROP TABLE IF EXISTS "^mapTable ; "DROP TABLE IF EXISTS "^historyTable ; "DROP TABLE IF EXISTS "^domainTable ; "CREATE TABLE "^stateTable^" (stateKey TEXT PRIMARY KEY, stateType TEXT NOT NULL, name TEXT, s1 TEXT, s2 TEXT, side TEXT )" ; "CREATE TABLE "^historyTable^" (historyKey TEXT , name TEXT, stateKey TEXT , PRIMARY KEY ( historyKey, name) )" ; "CREATE TABLE "^mapTable^" (mapKey TEXT NOT NULL, valueType TEXT, value TEXT, stateKey TEXT ) " ; "CREATE TABLE "^domainTable^" (domainKey TEXT NOT NULL, type TEXT, begin INT, end INT, ValueType TEXT, value TEXT ) " ] 

(** DOMAIN TUPLES *)
type domainTuple = { 
                domainKey : string
              ; dtype : string
              ; dbegin : int option
              ; dend : int option
              ; valueType : string option
              ; value : string option
              } ;;

let emptydomainTuple = { 
                domainKey = ""
              ; dtype = ""
              ; dbegin = None
              ; dend = None
              ; valueType = None
              ; value = None
              }

let createDomainTuple dKey domain =  match domain with 
      | Range (b, e) -> { emptydomainTuple with domainKey = dKey ; dtype = "range" ; dbegin = Some b ; dend = Some e }
      | Val a ->  { emptydomainTuple with domainKey = dKey ; dtype = "val" ; valueType = Some (constant2Type a) ; value = Some (constant2String a) }
      | _ -> { emptydomainTuple with domainKey = dKey } 

let getSomeNoneString x = match x with
    | Some x -> x
    | None -> ""

let getSomeNoneInt x = match x with
    | Some x -> string_of_int x
    | None -> ""


let domainTuple2sql d = 
    "INSERT INTO "^ domainTable ^" VALUES ('"^ d.domainKey ^"', '"^ d.dtype ^"', '"^ getSomeNoneInt d.dbegin ^"', '"^ getSomeNoneInt d.dend ^"', '"^ getSomeNoneString d.valueType ^"', '"^ getSomeNoneString d.value ^"')"

(** HISTORY TUPLES *)

(* historyTuple : tuple storing state of a sub-state in an automaton *)
(* historyTuples might be stored in a list in order to be saved in database *)
type historyTuple = { 
                historyKey : string
              ; name : string
              ; stateKey : string
              } 


(* Returns an history tuple from an history key, a name and a state key *)             
let createHistoryTuple hKey hName hStateKey = 
    { historyKey = hKey ; name = hName ; stateKey = hStateKey } 

let attributeKeyHistoryTuple key htuple = { htuple with historyKey = key }

(* Returns a String containing an sql insert allowing to store an historyTuple in DB  *) 
let historyTuple2sql h = 
    "INSERT OR REPLACE INTO "^ historyTable ^" VALUES ('"^ h.historyKey ^"', '"^ h.name ^"', '"^ h.stateKey ^"')"


(** MAP TUPLES *)

(* mapTuple : storing the link between value of quantified variable and state *)
(* mapTuple might be stored in a list in order to be saved in database *)
type mapTuple = {
                mapKey : string
              ; valueType : string   
              ; value : string 
              ; stateKey : string
              } ;;

(* Returns an mapTuple from an map key and an ASTD_constant *)             
let createMapTuple mKey constant sKey = {
                mapKey = mKey
              ; valueType = ( constant2Type constant )
              ; value =  ( constant2String constant )
              ; stateKey = sKey
              } 

(* Returns a String containing an sql insert allowing to store an mapTuple in DB  *) 
let mapTuple2sql m = 
    "INSERT OR REPLACE INTO "^ mapTable ^" VALUES ('"^ m.mapKey ^"', '"^ m.valueType ^"', '"^ m.value ^"', '"^ m.stateKey ^"')"

(** STATE TUPLES *)

(* stateTuple : storing an astd state in a tuple *)
type stateTuple = {
                stateKey : string           (* the key of the state *)
              ; stateType : string          (* type of astd *)
              (* values : AUT SEQ CHO KLE SYN QCH QSY GUA CAL *)
              ; name : string               (* AUT: name of automaton current substate *)
              ; s1 : string                 (* AUT SEQ CHO KLE SYN QSY GUA: stateKey of sub-state 1 *)
              ; s2 : string                 (* SYN: stateKey of sub-state 2 *)
              ; side : string               (* CHO SEQ KLE GUA CAL *)
              }

let stateTuple2sql s = 
    "INSERT OR REPLACE INTO "^ stateTable ^" VALUES ('"^ s.stateKey ^"', '"^ s.stateType ^"', '"^ s.name ^"', '"^ s.s1 ^"', '"^ s.s2 ^"', '"^ s.side ^"')"

 
(* emptyStateTuple : an empty statetuple *)
let emptyStateTuple = {
                stateKey = "" 
              ; stateType = ""
              ; name = ""
              ; s1 = ""
              ; s2 = ""
              ; side = ""
              }

let automataStateTuple name subkey = {
         emptyStateTuple with
            name = name ;
            s1 = subkey ;
            stateType = "aut" }
              
let sequenceStateTuple s subkey = {
         emptyStateTuple with
            side = ( match s with 
                | Fst  -> "first"
                | Snd -> "second" ) ;  
            s1 = subkey ;
            stateType = "seq" }
            
let choiceStateTuple s subkey = {
         emptyStateTuple with
            side = ( match s with 
                | Undef -> "undef" 
                | Left  -> "left"
                | Right -> "right" ) ;
            s1 = subkey ;
            stateType = "cho" }
 
let kleeneStateTuple s subkey = {
         emptyStateTuple with
            side = ( match s with | true -> "started" | false -> "neverexecuted" ) ;
            s1 = subkey ;
            stateType = "kle" } 

let syncStateTuple s1Key s2Key =  {
         emptyStateTuple with
            s1 = s1Key ;
            s2 = s2Key ;
            stateType = "syn" }
            
let qChoiceStateTuple init = {
         emptyStateTuple with
            s1 =  init ;
            stateType = "qch" }
            
let qSyncStateTuple  init = {
         emptyStateTuple with
            s1 = init ;
            stateType = "qsy" }
            
            
let guardStateTuple s subkey = {
         emptyStateTuple with
            side = ( match s with | true -> "after" | false -> "before" ) ;
            s1 = subkey ;
            stateType = "gua" }
            
let callStateTuple s subkey = {
         emptyStateTuple with
            side =  ( match s with | true -> "called" | false -> "nevercalled" ) ;
            s1 = subkey ;
            stateType = "cal" }
            
let attributeKeyStateTuple key statetuple = { statetuple with stateKey = key }


let rec history2sql astd key history = match history with
    | name, s -> begin 
                    let hStateKey = key ^ "/" ^ name in
                    let htuple = createHistoryTuple key name hStateKey in
                    let subastd = (ASTD_astd.find_subastd name (ASTD_astd.get_sub astd)) in
                        ( historyTuple2sql htuple ) :: ( state2sql subastd s hStateKey )
                 end 

and synchro2sql astd state key value =
    let sub_astd= ASTD_astd.get_qastd astd in
    let substatekey = key^":"^(constant2String value)^"/"^(ASTD_astd.get_name sub_astd ) in
    let substate = state2sql sub_astd state substatekey in
    let maptuple = createMapTuple key value substatekey in
    (mapTuple2sql maptuple) :: substate

and state2sql astd state key = 
    match state with 
        | Automata_s ( position , history , substate ) ->
            begin debug ("State2DB : state2sql for automata @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let substateKey = (key^"/"^ position) in
                let statetuple = attributeKeyStateTuple key (automataStateTuple position substateKey) in
                let sqlFromHistory =  List.concat ( List.map (history2sql astd key) history ) in
                let substatesql = state2sql (ASTD_astd.find_subastd position (ASTD_astd.get_sub astd)) substate substateKey in
                (stateTuple2sql statetuple) :: ( sqlFromHistory @ substatesql )
            end 
            
        | Sequence_s ( side , substate ) ->
            begin debug ("State2DB : state2sql for sequence @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let subastd = (match side with | Fst -> ASTD_astd.get_seq_l astd | Snd -> ASTD_astd.get_seq_r astd) in
                let substateKey =  key^"/"^(ASTD_astd.get_name subastd) in
                let statetuple = attributeKeyStateTuple key ( sequenceStateTuple side substateKey ) in
                let substatesql = state2sql subastd substate substateKey in
                (stateTuple2sql statetuple) :: substatesql
            end

        | Choice_s ( side , substate ) ->  
            begin debug ("State2DB : state2sql for choice @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let subastd = (match side with | Left -> ASTD_astd.get_choice1 astd | Right -> ASTD_astd.get_choice2 astd | _ -> astd) in
                let substateKey = (match side with | Undef -> "notdefined" | _ ->  key^"/"^(ASTD_astd.get_name subastd)) in
                let statetuple = attributeKeyStateTuple key ( choiceStateTuple side substateKey ) in
                let substatesql = state2sql subastd substate substateKey in
                 (stateTuple2sql statetuple) :: substatesql        
            end
            
        | Kleene_s ( started , substate ) ->
            begin debug ("State2DB : state2sql for kleene @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let subastd =  ASTD_astd.get_astd_kleene astd  in
                let substateKey = key^"/"^(ASTD_astd.get_name subastd) in
                let statetuple = attributeKeyStateTuple key ( kleeneStateTuple started substateKey ) in
                let substatesql = state2sql subastd substate substateKey in
                 (stateTuple2sql statetuple) :: substatesql
            end             

        | Synchronisation_s ( s1 , s2 ) ->
            begin debug ("State2DB : state2sql for synchro @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let subastd1 =  ASTD_astd.get_synchro_astd1 astd in
                let subastd2 =  ASTD_astd.get_synchro_astd2 astd in
                let s1key = key^"/"^(ASTD_astd.get_name subastd1) in
                let s2key = key^"/"^(ASTD_astd.get_name subastd2) in
                let statetuple = attributeKeyStateTuple key ( syncStateTuple s1key s2key ) in
                let s1sql = state2sql subastd1 s1 s1key  in
                let s2sql = state2sql subastd2 s2 s2key  in
                (stateTuple2sql statetuple) :: s1sql @ s2sql
            end                               

        | QChoice_s ( qchoice , d1 , d2 , substate ) ->
             begin debug ("State2DB : state2sql for qchoice @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let f a b  = domainTuple2sql (createDomainTuple a b) in
                let d1sql = ASTD_constant.map_dom (f (key^":d1")) d1 in
                let d2sql = ASTD_constant.map_dom (f (key^":d2")) d2 in
                let subastd =  ASTD_astd.get_qastd astd  in
                let maptuple = ( match qchoice with
                    | ASTD_state.Val term -> let c = ASTD_term.extract_constant_from_term term in
                         [ mapTuple2sql ( createMapTuple key c (key^":"^(constant2String c)^"/"^(ASTD_astd.get_name subastd) ) ) ]
                    | _ -> [] ) in
                let substateKey = ( match qchoice with
                    | ASTD_state.Val term -> (let c = ASTD_term.extract_constant_from_term term in key^":"^(constant2String c)^"/"^(ASTD_astd.get_name subastd) )
                    | ASTD_state.ChoiceNotMade -> (key^"/"^(ASTD_astd.get_name subastd)) ) in
                let substatesql = state2sql subastd substate substateKey in    
                let statetuple = attributeKeyStateTuple key ( qChoiceStateTuple substateKey ) in
                (stateTuple2sql statetuple) :: ( maptuple @ d1sql @ d2sql @ substatesql )
             end

        | QSynchronisation_s  ( d1 , d2 , d3 , substate ) ->
              begin debug ("State2DB : state2sql for qsynch @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                executeQuery emptyCB ("DELETE FROM "^domainTable^" WHERE domainkey IN ('"^(key^":d1")^"','"^(key^":d2")^"','"^(key^":d3")^"')");
                let f a b  = domainTuple2sql (createDomainTuple a b) in
                let d1sql = ASTD_constant.map_dom (f (key^":d1")) d1 in
                let d2sql = ASTD_constant.map_dom (f (key^":d2")) d2 in
                let d3sql = ASTD_constant.map_dom (f (key^":d3")) d3 in
                let subastd =  ASTD_astd.get_qastd astd  in
                let substateKey = key^"/"^(ASTD_astd.get_name subastd) in
                let substatesql = state2sql subastd substate substateKey in    
                let statetuple = attributeKeyStateTuple key ( qSyncStateTuple substateKey ) in
                (stateTuple2sql statetuple) :: ( d1sql @ d2sql @ d3sql @ substatesql )
             end

        | Guard_s ( boolean , substate ) ->
            begin debug ("State2DB : state2sql for guard @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let subastd = ASTD_astd.get_guard_astd astd in
                let substateKey = key^"/"^(ASTD_astd.get_name subastd) in
                let statetuple = attributeKeyStateTuple key ( guardStateTuple boolean substateKey ) in
                let substatesql =  state2sql subastd substate substateKey in
                 (stateTuple2sql statetuple) :: substatesql
            end                 

        | Call_s ( boolean , substate ) ->
            begin debug ("State2DB : state2sql for call @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let subastd = ASTD_astd.get_astd (ASTD_astd.get_called_name astd)  in
                let substateKey = key^"/"^(ASTD_astd.get_name subastd) in
                let statetuple = attributeKeyStateTuple key ( callStateTuple boolean substateKey ) in
                let substatesql = state2sql subastd substate substateKey in
                 (stateTuple2sql statetuple) :: substatesql
            end                
        
        | Elem -> begin debug ("State2DB : state2sql for elem @ "^key^" with astd name "^(ASTD_astd.get_name astd));
            ["INSERT OR REPLACE INTO "^ stateTable ^" VALUES ('"^key^"','elem','','','','')"]
		end
            
        | NotDefined -> begin debug ("State2DB : state2sql for qchoice @ "^key^" with astd name "^(ASTD_astd.get_name astd));
            ["INSERT OR REPLACE INTO "^ stateTable ^" VALUES ('"^key^"','notdefined','','','','')"]
		end


let statePrintSQL astd state key = let sqls = state2sql astd state key in List.map print_endline sqls 

let initdb () = let sqls = ( createDB ) in executeQueries emptyCB sqls
          
let state2db astd state key = begin debug "State2DB : SQL generation starts now " ; 
				let sqls = ( (state2sql astd state key) ) 
				in begin debug "State2DB : SQL generation in progress ";
					executeQueries emptyCB sqls
					end
				end
    
let synchroPrintSQL astd state key c = let sqls = synchro2sql astd state key c in List.map print_endline sqls 
          
let synchro2db astd state key c = let sqls =  ( synchro2sql astd state key c ) in executeQueries emptyCB sqls

let register2db astd state key value = match value with | Some c -> synchro2db astd state key c | None -> state2db astd state key

let register2PrintSQL astd state key value = match value with | Some c -> synchroPrintSQL astd state key c | None -> statePrintSQL astd state key




let dbNewState = ref Elem

let dbHistoryList = ref []

let dbMapList = ref []

let dbDomainList = ref ASTD_constant.empty_dom

let rec array2history row header = 
        let savehisto = !dbHistoryList in let saveState = !dbNewState in
        dbHistoryList := [] ;
        let nom = ( match row.(1) with | Some  x -> x  | _ -> raise ( DBError ("Unexpected side value in "^historyTable ) ) ) in
        let subkey = match row.(2) with | Some x -> x | _ -> raise ( DBError ("Unexpected side value in "^historyTable ) )  in
        ignore ( executeQuery (array2state subkey) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^ subkey ^"'") ) ;
        let sub = !dbNewState in dbNewState := saveState;
        dbHistoryList := ((nom,sub) :: savehisto);

and array2map row header =
        let saveMap = !dbMapList in let saveState =  ! dbNewState in
        dbMapList := [] ; 
        executeQuery (array2state (getSomeNoneString row.(3))) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^(getSomeNoneString row.(3))^"'");
        let sub = !dbNewState in
        let tuple = ( match row.(1) with
            | Some "integer" -> (ASTD_term.Const(ASTD_constant.Integer(int_of_string ( getSomeNoneString row.(2) ))), sub )
            | Some  "string"  -> (ASTD_term.Const(ASTD_constant.Symbol(( getSomeNoneString row.(2) ))) , sub )
            | _ -> raise ( DBError ("Unexpected valueType value in "^mapTable ) ) ) in    
        dbNewState := saveState;
        dbMapList := tuple::saveMap;

and array2domain row header = let saveDom = !dbDomainList in
        let newValue = (match row.(1),row.(4) with
            | (Some "range", _) -> ASTD_constant.Range( int_of_string ( getSomeNoneString row.(2) ) , int_of_string ( getSomeNoneString row.(3) ) )
            | (Some "val", Some "integer" ) -> ASTD_constant.Val( ASTD_constant.of_int ( int_of_string ( getSomeNoneString row.(5) ) ) )
            | (Some "val", Some "string" ) -> ASTD_constant.Val( ASTD_constant.Symbol (getSomeNoneString row.(5) ) )
            | _ -> raise ( DBError ("Unexpected dtype value in "^domainTable ) ) ) in                 
        dbDomainList := ASTD_constant.insert newValue saveDom;
       
and array2state currentkey row header = debug ("State2DB ======> get state : "^currentkey) ; match row.(1) with
        | Some "aut" -> debug "Je suis un automate" ;
                   let position = ( match row.(2) with | Some  x -> x  | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) ) ) in
                   let subkey = match row.(3) with | Some x -> x | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) )  in
                   begin  let savehisto = !dbHistoryList in
                          dbHistoryList := [];  
                          executeQuery array2history ("SELECT * FROM "^historyTable^" WHERE historyKey ='"^ currentkey ^"'")  ;                          
                          let histo = !dbHistoryList in                           
                          executeQuery (array2state subkey) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^ subkey ^"'")  ;
                          let sub = !dbNewState in                       
                          dbHistoryList := savehisto ;
                          debug  ("Taille de l'historique de "^currentkey^" : "^(string_of_int (List.length histo) )) ; 
                          dbNewState :=  ASTD_state.automata_s_of position histo sub
                   end
        | Some "seq" -> debug "Je suis une sequence" ; 
                   let step = ( match row.(5) with | Some  "first" -> Fst | Some "second" -> Snd | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) ) ) in
                   let subkey = match row.(3) with | Some x -> x | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) )  in
                   begin  ignore ( executeQuery (array2state subkey) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^ subkey ^"'") ) ;
                          let sub = !dbNewState in
                          dbNewState :=  ASTD_state.sequence_s_of step sub
                   end
        | Some "cho" -> debug "Je suis un choix" ; 
                   let side = ( match row.(5) with | Some  "left" -> Left | Some "right" -> Right | Some "undef" -> Undef | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) ) ) in
                   let subkey = match row.(3) with | Some x -> x | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) )  in
                   begin  ignore ( executeQuery (array2state subkey) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^ subkey ^"'") ) ;
                          let sub = !dbNewState in
                          dbNewState :=  ASTD_state.choice_s_of side sub
                   end
        | Some "kle" -> debug "Je suis une kleene" ; 
                   let started = ( match row.(5) with | Some  "neverexecuted" -> false | Some "started" -> true | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) ) ) in
                   let subkey = match row.(3) with | Some x -> x | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) )  in
                   begin  ignore ( executeQuery (array2state subkey) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^ subkey ^"'") ) ;
                          let sub = !dbNewState in
                          dbNewState :=  ASTD_state.kleene_s_of started sub
                   end
        | Some "syn" -> debug "Je suis une syn" ;
                   let subkey1 = match row.(3) with | Some x -> x | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) )  in
                   let subkey2 = match row.(4) with | Some x -> x | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) )  in
                   begin  ignore ( executeQuery (array2state subkey1) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^ subkey1 ^"'") ) ;
                          let sub1 = !dbNewState in
                          ignore ( executeQuery (array2state subkey2) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^ subkey2 ^"'") ) ;
                          let sub2 = !dbNewState in
                          dbNewState :=  ASTD_state.synchronisation_s_of sub1 sub2
                   end
        | Some "gua" -> debug "Je suis une gua" ;
                   let tested = ( match row.(5) with | Some  "before" -> false | Some "after" -> true | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) ) ) in
                   let subkey = match row.(3) with | Some x -> x | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) )  in
                   begin  ignore ( executeQuery (array2state subkey) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^ subkey ^"'") ) ;
                          let sub = !dbNewState in
                          dbNewState :=  ASTD_state.guard_s_of tested sub
                   end
       
        | Some "cal" -> debug "Je suis une cal" ; 
                   let called = ( match row.(5) with | Some  "nevercalled" -> false | Some "called" -> true | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) ) ) in
                   let subkey = match row.(3) with | Some x -> x | _ -> raise ( DBError ("Unexpected side value in "^stateTable ) )  in
                   begin  ignore ( executeQuery (array2state subkey) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^ subkey ^"'") ) ;
                          let sub = !dbNewState in
                          dbNewState :=  ASTD_state.call_s_of called sub
                   end
        
        | Some "qsy" -> debug "Je suis une qsy" ; 
                   begin  let saveDomain = !dbDomainList in
                          executeQuery array2domain ("SELECT * FROM "^domainTable^" WHERE domainKey ='"^ currentkey ^":d1'")  ;
                          let newdomain1 = !dbDomainList in  dbDomainList := ASTD_constant.empty_dom;
                          executeQuery array2domain ("SELECT * FROM "^domainTable^" WHERE domainKey ='"^ currentkey ^":d2'")  ;
                          let newdomain2 = !dbDomainList in  dbDomainList := ASTD_constant.empty_dom;
                          executeQuery array2domain ("SELECT * FROM "^domainTable^" WHERE domainKey ='"^ currentkey ^":d3'")  ;
                          let newdomain3 = !dbDomainList in
                          dbDomainList := saveDomain;         
                          ignore ( executeQuery (array2state (getSomeNoneString row.(3))) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^ (getSomeNoneString row.(3))^"'") ) ;
                          let substate = !dbNewState in
                          dbNewState :=  ASTD_state.qsynchronisation_s_of newdomain1 newdomain2 newdomain3 substate
                   end  

        | Some "qch" -> debug "Je suis une qch" ; 
                   begin  let saveMap = !dbMapList in let saveDomain = !dbDomainList in
                          dbMapList := [];  dbDomainList := ASTD_constant.empty_dom;  
                          executeQuery array2map  ("SELECT * FROM "^mapTable^" WHERE mapKey ='"^ currentkey ^"'")  ;                          
                          let newmap = !dbMapList in                           
                          executeQuery array2domain ("SELECT * FROM "^domainTable^" WHERE domainKey ='"^ currentkey ^":d1'")  ;
                          let newdomain1 = !dbDomainList in  dbDomainList := ASTD_constant.empty_dom;
                          executeQuery array2domain ("SELECT * FROM "^domainTable^" WHERE domainKey ='"^ currentkey ^":d2'")  ;
                          let newdomain2 = !dbDomainList in
                          dbMapList := saveMap ;
                          dbDomainList := saveDomain;         
                          let valmap = ( match newmap with
                            | (a,b)::_ -> (ASTD_state.Val(a),b)
                            | [] -> begin ignore ( executeQuery (array2state (getSomeNoneString row.(3))) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^ (getSomeNoneString row.(3))^"'") ) ;
                                          let substate = !dbNewState in
                                          (ASTD_state.ChoiceNotMade , substate )  
                                    end  )
                            in
                          dbNewState :=  ASTD_state.qchoice_s_of (fst valmap) newdomain1 newdomain2 (snd valmap)
                   end
        
        
        | Some "elem" -> debug "Je suis un Elem" ; dbNewState := ASTD_state.Elem
        | Some "notdefined" -> debug "Je suis une Nulle" ; dbNewState := ASTD_state.NotDefined
        | _ ->  raise ( DBError ("Unexpected side value in "^stateTable ) )

let db2state key = begin  debug ("State2DB ======> looking for "^ key);
                          ignore ( executeQuery (array2state key) ("SELECT * FROM "^stateTable^" WHERE stateKey ='"^ key ^"'") ) ;
                          !dbNewState 
                   end
                   
let deleteTuplesDB key = let sqls = [ 
    "DELETE FROM "^stateTable^" WHERE stateKey LIKE '"^key^"%'" ;
    "DELETE FROM "^mapTable^" WHERE mapKey LIKE '"^key^"%'" ;
    "DELETE FROM "^domainTable^" WHERE domainKey LIKE '"^key^"%'" ;
    "DELETE FROM "^historyTable^" WHERE historyKey LIKE '"^key^"%'" ;] in                   
    executeQueries emptyCB sqls
