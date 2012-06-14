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

let createDB = [ "DROP TABLE IF EXISTS "^stateTable ; "DROP TABLE IF EXISTS "^mapTable ; "DROP TABLE IF EXISTS "^historyTable ; "DROP TABLE IF EXISTS "^domainTable ; "CREATE TABLE "^stateTable^" (stateKey TEXT PRIMARY KEY, stateType TEXT NOT NULL, name TEXT, s1 TEXT, s2 TEXT, side TEXT )" ; "CREATE TABLE "^historyTable^" (historyKey TEXT , name TEXT, stateKey TEXT , PRIMARY KEY ( historyKey, name) )" ; "CREATE TABLE "^mapTable^" (mapKey TEXT NOT NULL, valueType TEXT, value TEXT, stateKey TEXT ) " ; "CREATE TABLE "^domainTable^" (domainKey TEXT NOT NULL, type TEXT, begin INT, end INT, ValueType TEXT, value TEXT ) " ] 

let rcTest rc = match rc with
            | Sqlite3.Rc.OK -> ()
            | Sqlite3.Rc.ROW -> ()
            | Sqlite3.Rc.DONE -> ()
            | r -> prerr_endline ("Sqlite3 Statement ERROR : "^Sqlite3.Rc.to_string r^". "^ Sqlite3.errmsg db)


(** CONVERSIONS *)

let constant2String a = match a with | Integer a -> string_of_int a | Symbol a -> a | _ -> ""
let constant2Type a = match a with | Integer a -> "integer" | Symbol a -> "string" | _ -> ""

let getSomeNoneString a = match a with
    | Some x -> x
    | None -> ""

let getSomeNoneInt a = match a with
    | Some x -> string_of_int x
    | None -> ""

let getSomeNoneText a = match a with
    | Some x -> Sqlite3.Data.TEXT x
    | None -> Sqlite3.Data.TEXT ""

let getSomeNoneIntText a = match a with
    | Some x -> Sqlite3.Data.TEXT (string_of_int x)
    | None -> Sqlite3.Data.TEXT ""

let getSomeNoneInt64 a = match a with
    | Some x -> Sqlite3.Data.INT (Int64.of_int x)
    | None -> Sqlite3.Data.NULL

let getEmptyText a = match a with
    | "" -> Sqlite3.Data.NULL
    | x -> Sqlite3.Data.TEXT x


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

let preparedInsertDom = Sqlite3.prepare db ( "INSERT OR REPLACE INTO "^ domainTable ^" VALUES (? , ? , ? , ? , ? , ? )" ) 

let insertDomain d =
        begin
            debug ("@@@@ insert domain "^ d.domainKey ); 
            rcTest (Sqlite3.reset preparedInsertDom) ;
            rcTest (Sqlite3.bind preparedInsertDom 1 (getEmptyText d.domainKey ));            
            rcTest (Sqlite3.bind preparedInsertDom 2 (getEmptyText d.dtype ));            
            rcTest (Sqlite3.bind preparedInsertDom 3 (getSomeNoneInt64 d.dbegin ));            
            rcTest (Sqlite3.bind preparedInsertDom 4 (getSomeNoneInt64 d.dend ));            
            rcTest (Sqlite3.bind preparedInsertDom 5 (getSomeNoneText d.valueType ));            
            rcTest (Sqlite3.bind preparedInsertDom 6 (getSomeNoneText d.value ));
            rcTest (Sqlite3.step preparedInsertDom)
        end

let preparedDeleteDom = Sqlite3.prepare db ( "DELETE FROM "^domainTable^" WHERE domainkey IN ( ? , ? , ? )" )

let deleteDomain k =
        begin
            debug ("@@@@ delete domain "^ k ); 
            rcTest (Sqlite3.reset preparedDeleteDom) ;
            rcTest (Sqlite3.bind preparedDeleteDom 1 (getEmptyText (k^":d1") ));            
            rcTest (Sqlite3.bind preparedDeleteDom 2 (getEmptyText (k^":d2") ));            
            rcTest (Sqlite3.bind preparedDeleteDom 3 (getEmptyText (k^":d3") ));            
            rcTest (Sqlite3.step preparedDeleteDom)
        end

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

let preparedInsertHisto = Sqlite3.prepare db ( "INSERT OR REPLACE INTO "^ historyTable ^" VALUES ( ? , ? , ? )" ) 

let insertHistory h =
        begin
            debug ("@@@@ insert history "^  h.historyKey); 
            rcTest (Sqlite3.reset preparedInsertHisto) ;
            rcTest (Sqlite3.bind preparedInsertHisto 1 (getEmptyText h.historyKey ));            
            rcTest (Sqlite3.bind preparedInsertHisto 2 (getEmptyText h.name ));            
            rcTest (Sqlite3.bind preparedInsertHisto 3 (getEmptyText h.stateKey ));            
            rcTest (Sqlite3.step preparedInsertHisto)
        end

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

let preparedInsertMap = Sqlite3.prepare db ( "INSERT OR REPLACE INTO "^ mapTable ^" VALUES ( ? , ? , ? , ? )" ) 

let insertMap m =
        begin
            debug ("@@@@ insert map "^ m.mapKey  ); 
            rcTest (Sqlite3.reset preparedInsertMap) ;
            rcTest (Sqlite3.bind preparedInsertMap 1 (getEmptyText m.mapKey ));            
            rcTest (Sqlite3.bind preparedInsertMap 2 (getEmptyText m.valueType ));            
            rcTest (Sqlite3.bind preparedInsertMap 3 (getEmptyText m.value ));            
            rcTest (Sqlite3.bind preparedInsertMap 4 (getEmptyText m.stateKey ));            
            rcTest (Sqlite3.step preparedInsertMap)
        end

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

let preparedInsertState = Sqlite3.prepare db ( "INSERT OR REPLACE INTO "^ stateTable ^" VALUES ( ? , ? , ? , ? , ? , ? )" ) 

let insertState s =
        begin
            debug ("@@@@ insert state "^  s.stateKey  ); 
            rcTest (Sqlite3.reset preparedInsertState) ;
            rcTest (Sqlite3.bind preparedInsertState 1 (getEmptyText s.stateKey ));            
            rcTest (Sqlite3.bind preparedInsertState 2 (getEmptyText s.stateType ));            
            rcTest (Sqlite3.bind preparedInsertState 3 (getEmptyText s.name ));            
            rcTest (Sqlite3.bind preparedInsertState 4 (getEmptyText s.s1  ));            
            rcTest (Sqlite3.bind preparedInsertState 5 (getEmptyText s.s2  ));            
            rcTest (Sqlite3.bind preparedInsertState 6 (getEmptyText s.side  ));            
            rcTest (Sqlite3.step preparedInsertState)
        end


 
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

let elemStateTuple key = {
         emptyStateTuple with
         stateKey = key ;
         stateType = "elem" }
         
let notStateTuple key = {
         emptyStateTuple with
         stateKey = key ;
         stateType = "notdefined" }
         

let attributeKeyStateTuple key statetuple = { statetuple with stateKey = key }


let rec history2sql astd key history = match history with
    | name, s -> begin 
                    let hStateKey = key ^ "/" ^ name in
                    let htuple = createHistoryTuple key name hStateKey in
                        insertHistory htuple;
                    let subastd = (ASTD_astd.find_subastd name (ASTD_astd.get_sub astd)) in
                        ( state2sql subastd s hStateKey )
                 end 

and synchro2sql astd state key value =
    let sub_astd= ASTD_astd.get_qastd astd in
    let substatekey = key^":"^(constant2String value)^"/"^(ASTD_astd.get_name sub_astd ) in
    let substate = state2sql sub_astd state substatekey in
    let maptuple = createMapTuple key value substatekey in insertMap maptuple ;
    substate

and state2sql astd state key = 
    match state with 
        | Automata_s ( position , history , substate ) ->
            begin debug ("State2DB : state2sql for automata @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let substateKey = (key^"/"^ position) in
                let statetuple = attributeKeyStateTuple key (automataStateTuple position substateKey) in
                List.map (history2sql astd key) history  ;
                state2sql (ASTD_astd.find_subastd position (ASTD_astd.get_sub astd)) substate substateKey ;
                insertState statetuple
            end 
            
        | Sequence_s ( side , substate ) ->
            begin debug ("State2DB : state2sql for sequence @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let subastd = (match side with | Fst -> ASTD_astd.get_seq_l astd | Snd -> ASTD_astd.get_seq_r astd) in
                let substateKey =  key^"/"^(ASTD_astd.get_name subastd) in
                let statetuple = attributeKeyStateTuple key ( sequenceStateTuple side substateKey ) in
                state2sql subastd substate substateKey ;
                    insertState statetuple
            end

        | Choice_s ( side , substate ) ->  
            begin debug ("State2DB : state2sql for choice @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let subastd = (match side with | Left -> ASTD_astd.get_choice1 astd | Right -> ASTD_astd.get_choice2 astd | _ -> astd) in
                let substateKey = (match side with | Undef -> "notdefined" | _ ->  key^"/"^(ASTD_astd.get_name subastd)) in
                let statetuple = attributeKeyStateTuple key ( choiceStateTuple side substateKey ) in
                state2sql subastd substate substateKey;
                    insertState statetuple
            end
            
        | Kleene_s ( started , substate ) ->
            begin debug ("State2DB : state2sql for kleene @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let subastd =  ASTD_astd.get_astd_kleene astd  in
                let substateKey = key^"/"^(ASTD_astd.get_name subastd) in
                let statetuple = attributeKeyStateTuple key ( kleeneStateTuple started substateKey ) in
                state2sql subastd substate substateKey;
                    insertState statetuple
            end             

        | Synchronisation_s ( s1 , s2 ) ->
            begin debug ("State2DB : state2sql for synchro @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let subastd1 =  ASTD_astd.get_synchro_astd1 astd in
                let subastd2 =  ASTD_astd.get_synchro_astd2 astd in
                let s1key = key^"/"^(ASTD_astd.get_name subastd1) in
                let s2key = key^"/"^(ASTD_astd.get_name subastd2) in
                let statetuple = attributeKeyStateTuple key ( syncStateTuple s1key s2key ) in
                state2sql subastd1 s1 s1key ;
                state2sql subastd2 s2 s2key ;
                insertState statetuple
            end                               

        | QChoice_s ( qchoice , d1 , d2 , substate ) ->
             begin debug ("State2DB : state2sql for qchoice @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let f a b  = insertDomain (createDomainTuple a b) in
                ASTD_constant.map_dom (f (key^":d1")) d1 ;
                ASTD_constant.map_dom (f (key^":d2")) d2 ;
                let subastd =  ASTD_astd.get_qastd astd  in
                (match qchoice with
                    | ASTD_state.Val term -> let c = ASTD_term.extract_constant_from_term term in
                         insertMap ( createMapTuple key c (key^":"^(constant2String c)^"/"^(ASTD_astd.get_name subastd) ) )
                    | _ -> () ) ;
                let substateKey = ( match qchoice with
                    | ASTD_state.Val term -> (let c = ASTD_term.extract_constant_from_term term in key^":"^(constant2String c)^"/"^(ASTD_astd.get_name subastd) )
                    | ASTD_state.ChoiceNotMade -> (key^"/"^(ASTD_astd.get_name subastd)) ) in
                state2sql subastd substate substateKey ;    
                let statetuple = attributeKeyStateTuple key ( qChoiceStateTuple substateKey ) in
                    insertState statetuple
             end

        | QSynchronisation_s  ( d1 , d2 , d3 , substate ) ->
              begin debug ("State2DB : state2sql for qsynch @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                deleteDomain key ;
                let f a b  = insertDomain (createDomainTuple a b) in
                ASTD_constant.map_dom (f (key^":d1")) d1;
                ASTD_constant.map_dom (f (key^":d2")) d2;
                ASTD_constant.map_dom (f (key^":d3")) d3; 
                let subastd =  ASTD_astd.get_qastd astd  in
                let substateKey = key^"/"^(ASTD_astd.get_name subastd) in
                state2sql subastd substate substateKey ;
                let statetuple = attributeKeyStateTuple key ( qSyncStateTuple substateKey ) in
                    insertState statetuple
             end

        | Guard_s ( boolean , substate ) ->
            begin debug ("State2DB : state2sql for guard @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let subastd = ASTD_astd.get_guard_astd astd in
                let substateKey = key^"/"^(ASTD_astd.get_name subastd) in
                let statetuple = attributeKeyStateTuple key ( guardStateTuple boolean substateKey ) in
                state2sql subastd substate substateKey ;
                insertState statetuple
            end                 

        | Call_s ( boolean , substate ) ->
            begin debug ("State2DB : state2sql for call @ "^key^" with astd name "^(ASTD_astd.get_name astd));
                let subastd = ASTD_astd.get_astd (ASTD_astd.get_called_name astd)  in
                let substateKey = key^"/"^(ASTD_astd.get_name subastd) in
                let statetuple = attributeKeyStateTuple key ( callStateTuple boolean substateKey ) in
                state2sql subastd substate substateKey ;
                    insertState statetuple
            end                
        
        | Elem -> begin debug ("State2DB : state2sql for elem @ "^key^" with astd name "^(ASTD_astd.get_name astd));
            insertState (elemStateTuple key)
		end
            
        | NotDefined -> begin debug ("State2DB : state2sql for qchoice @ "^key^" with astd name "^(ASTD_astd.get_name astd));
            insertState (notStateTuple key)
		end

let initdb () = let sqls = ( createDB ) in executeQueries emptyCB sqls
          
let state2db astd state key = begin 
                                debug "State2DB : SQL generation starts now " ; 
                				state2sql astd state key
                    		  end
          
let synchro2db astd state key c = synchro2sql astd state key c

let register2db astd state key value = match value with | Some c -> synchro2db astd state key c | None -> state2db astd state key
               
let deleteTuplesDB key = let sqls = [ 
    "DELETE FROM "^stateTable^" WHERE stateKey LIKE '"^key^"%'" ;
    "DELETE FROM "^mapTable^" WHERE mapKey LIKE '"^key^"%'" ;
    "DELETE FROM "^domainTable^" WHERE domainKey LIKE '"^key^"%'" ;
    "DELETE FROM "^historyTable^" WHERE historyKey LIKE '"^key^"%'" ;] in                   
    executeQueries emptyCB sqls

let preparedSelectState = Sqlite3.prepare db ("SELECT * FROM "^stateTable^" WHERE stateKey = ?")

let stateRow num =  begin 
                        debug("RETOUR DE state STATEMENT : "^ Sqlite3.Data.to_string (Sqlite3.column preparedSelectState num)); 
                        Sqlite3.column preparedSelectState num;                
                    end

let histoRow preparedSelectHisto num =  begin 
                        debug("RETOUR DE histo STATEMENT : "^ Sqlite3.Data.to_string (Sqlite3.column preparedSelectHisto num));
                        Sqlite3.column preparedSelectHisto num;                
                    end

let mapRow preparedSelectMap num =  begin 
                        debug("RETOUR DE map STATEMENT : "^ Sqlite3.Data.to_string (Sqlite3.column preparedSelectMap num));
                        Sqlite3.column preparedSelectMap num;                
                    end

let domRow preparedSelectDom num =  begin 
                        debug("RETOUR DE domain STATEMENT : "^ Sqlite3.Data.to_string (Sqlite3.column preparedSelectDom num));
                        Sqlite3.column preparedSelectDom num;                
                    end

let rec newArray2history preparedSelectHisto historyKey = 
        debug ("State2DB ======> get history with statements : "^historyKey) ; 
        let nom =  match (histoRow preparedSelectHisto 1) with | Sqlite3.Data.TEXT  x -> x  | _ -> raise ( DBError ("Unexpected name value in "^historyTable ) )  in
        let subkey = match (histoRow preparedSelectHisto 2) with | Sqlite3.Data.TEXT x -> x | _ -> raise ( DBError ("Unexpected subkey value in "^historyTable ) )  in
        let sub = selectState subkey in
        (nom,sub) 

and newrecArray2history preparedSelectHisto historyKey retour = debug ("---------- RECURSIF pour "^historyKey^" ------------") ;
    match Sqlite3.step preparedSelectHisto with 
    | Sqlite3.Rc.ROW -> newrecArray2history preparedSelectHisto historyKey ((newArray2history preparedSelectHisto historyKey ):: retour)
    | Sqlite3.Rc.OK -> retour
    | Sqlite3.Rc.DONE -> retour
    | r -> prerr_endline ("newrecArray2history Statement ERROR : "^Sqlite3.Rc.to_string r^". "^ Sqlite3.errmsg db); retour

and selectHistory hId = 
        begin
            let preparedSelectHisto = Sqlite3.prepare db ("SELECT * FROM "^historyTable^" WHERE historyKey = ?") in
            debug ("Find histo "^ hId ); 
            rcTest (Sqlite3.reset preparedSelectHisto) ;
            rcTest (Sqlite3.bind preparedSelectHisto 1 (Sqlite3.Data.TEXT hId ));
            newrecArray2history preparedSelectHisto hId []
        end


and newArray2map preparedSelectMap mapKey =
        debug ("State2DB ======> get map with statements : "^mapKey) ; 
        let sub = selectState (Sqlite3.Data.to_string (mapRow preparedSelectMap 3)) in        
        match (mapRow preparedSelectMap 1) with
            | Sqlite3.Data.TEXT "integer" -> (ASTD_term.Const(ASTD_constant.Integer(int_of_string (Sqlite3.Data.to_string (mapRow preparedSelectMap 2) ) ) ), sub )
            | Sqlite3.Data.TEXT  "string"  -> (ASTD_term.Const(ASTD_constant.Symbol( Sqlite3.Data.to_string(mapRow preparedSelectMap 2))) , sub )
            | _ -> raise ( DBError ("Unexpected valueType value in "^mapTable ) ) 

and newrecArray2map preparedSelectMap mapKey retour = debug ("---------- RECURSIF pour "^mapKey^" ------------") ;
    match Sqlite3.step preparedSelectMap with 
    | Sqlite3.Rc.ROW -> newrecArray2map preparedSelectMap mapKey ((newArray2map preparedSelectMap mapKey ):: retour) 
    | Sqlite3.Rc.OK -> retour
    | Sqlite3.Rc.DONE -> retour
    | r -> prerr_endline ("newrecArray2map Statement ERROR : "^Sqlite3.Rc.to_string r^". "^ Sqlite3.errmsg db); retour

and selectMap mId = 
        begin
            let preparedSelectMap = Sqlite3.prepare db ("SELECT * FROM "^mapTable^" WHERE mapKey = ?") in
            debug ("Find map "^ mId ); 
            rcTest (Sqlite3.reset preparedSelectMap) ;
            rcTest (Sqlite3.bind preparedSelectMap 1 (Sqlite3.Data.TEXT mId ));
            newrecArray2map preparedSelectMap mId []
        end


and newrecArray2dom preparedSelectDom domKey retour = debug ("---------- RECURSIF pour "^domKey^" ------------") ;
    match Sqlite3.step preparedSelectDom with 
    | Sqlite3.Rc.ROW -> debug ("***** ROW") ; let retourNew = (newArray2dom preparedSelectDom domKey retour) in newrecArray2dom preparedSelectDom domKey retourNew
    | Sqlite3.Rc.OK -> retour
    | Sqlite3.Rc.DONE -> debug ("+++++ domain found for "^domKey) ;  retour
    | r -> prerr_endline ("newrecArray2dom Statement ERROR : "^Sqlite3.Rc.to_string r^". "^ Sqlite3.errmsg db); retour


and newArray2dom preparedSelectDom domKey retour =
        debug ("State2DB ======> get domain with statements : "^domKey) ; 
        debug (">>>> domain : "^ Sqlite3.Data.to_string ( domRow preparedSelectDom 1 ) ^ Sqlite3.Data.to_string ( domRow preparedSelectDom 2 ) ^ Sqlite3.Data.to_string ( domRow preparedSelectDom 3 ) ^ Sqlite3.Data.to_string ( domRow preparedSelectDom 4 )^ Sqlite3.Data.to_string ( domRow preparedSelectDom 5 ));
        let newValue = ( match (domRow preparedSelectDom 1, domRow preparedSelectDom 4) with
            | (Sqlite3.Data.TEXT "range", _) -> 
                    ASTD_constant.Range( int_of_string (Sqlite3.Data.to_string ( domRow preparedSelectDom 2 )) , int_of_string ( Sqlite3.Data.to_string ( domRow preparedSelectDom 3 ))  ) 
            | (Sqlite3.Data.TEXT "val", Sqlite3.Data.TEXT "integer" ) -> ASTD_constant.Val( ASTD_constant.of_int ( int_of_string ( Sqlite3.Data.to_string ( domRow preparedSelectDom 5 ) ) ) )
            | (Sqlite3.Data.TEXT "val", Sqlite3.Data.TEXT "string" ) -> ASTD_constant.Val( ASTD_constant.Symbol ( Sqlite3.Data.to_string (domRow preparedSelectDom 5 ) ) )
            | _ -> raise ( DBError ("Unexpected dtype value in "^domainTable ) ) 
        ) in ASTD_constant.insert newValue retour;

and selectDomain dId = 
        begin
            let preparedSelectDom = Sqlite3.prepare db ("SELECT * FROM "^domainTable^" WHERE domainKey = ?") in
            debug ("Find domain "^ dId ); 
            rcTest (Sqlite3.reset preparedSelectDom) ;
            rcTest (Sqlite3.bind preparedSelectDom 1 (Sqlite3.Data.TEXT dId ));
            newrecArray2dom preparedSelectDom dId ASTD_constant.empty_dom
        end
    

and newArray2state currentkey = 
    debug ("State2DB ======> get state with statements : "^currentkey) ; 
    match stateRow(1) with
        | Sqlite3.Data.TEXT "aut" -> debug "Je suis un automate" ;
                   let position = ( match stateRow(2) with | Sqlite3.Data.TEXT x -> x  | _ -> raise ( DBError ("Unexpected name value in "^stateTable ) ) ) in
                   let subkey = match stateRow(3) with | Sqlite3.Data.TEXT x -> x | _ -> raise ( DBError ("Unexpected s1 value in "^stateTable ) )  in
                   begin  let histo = selectHistory currentkey in                           
                          let sub = selectState subkey in                       
                          debug  ("Taille de l'historique de "^currentkey^" : "^(string_of_int (List.length histo) )) ; 
                          ASTD_state.automata_s_of position histo sub
                   end

        | Sqlite3.Data.TEXT "seq" -> debug "Je suis une sequence" ; 
                   let step = ( match stateRow(5) with | Sqlite3.Data.TEXT  "first" -> Fst | Sqlite3.Data.TEXT "second" -> Snd | _ -> raise ( DBError ("Unexpected seq side value in "^stateTable ) ) ) in
                   let subkey = match stateRow(3) with | Sqlite3.Data.TEXT x -> x | _ -> raise ( DBError ("Unexpected s1 value in "^stateTable ) )  in
                   let sub = selectState subkey in
                      ASTD_state.sequence_s_of step sub

        | Sqlite3.Data.TEXT "cho" -> debug "Je suis un choix" ; 
                   let side = ( match stateRow(5) with | Sqlite3.Data.TEXT  "left" -> Left | Sqlite3.Data.TEXT "right" -> Right | Sqlite3.Data.TEXT "undef" -> Undef | _ -> raise ( DBError ("Unexpected cho side value in "^stateTable ) ) ) in
                   let subkey = match stateRow(3) with | Sqlite3.Data.TEXT x -> x | _ -> raise ( DBError ("Unexpected s1 value in "^stateTable ) )  in
                   let sub = selectState subkey in
                      ASTD_state.choice_s_of side sub

        | Sqlite3.Data.TEXT "kle" -> debug "Je suis une kleene" ; 
                   let started = ( match stateRow(5) with | Sqlite3.Data.TEXT  "neverexecuted" -> false | Sqlite3.Data.TEXT "started" -> true | _ -> raise ( DBError ("Unexpected kle side value in "^stateTable ) ) ) in
                   let subkey = match stateRow(3) with | Sqlite3.Data.TEXT x -> x | _ -> raise ( DBError ("Unexpected s1 value in "^stateTable ) )  in
                   let sub = selectState subkey in
                      ASTD_state.kleene_s_of started sub             

        | Sqlite3.Data.TEXT "syn" -> debug "Je suis une syn" ;
                   let subkey1 = match stateRow(3) with | Sqlite3.Data.TEXT x -> x | _ -> raise ( DBError ("Unexpected s1 value in "^stateTable ) )  in
                   let subkey2 = match stateRow(4) with | Sqlite3.Data.TEXT x -> x | _ -> raise ( DBError ("Unexpected s2 value in "^stateTable ) )  in
                   let sub1 = selectState subkey1 in
                   let sub2 = selectState subkey2 in
                      ASTD_state.synchronisation_s_of sub1 sub2          
        | Sqlite3.Data.TEXT "gua" -> debug "Je suis une gua" ;
                   let tested = ( match stateRow(5) with | Sqlite3.Data.TEXT  "before" -> false | Sqlite3.Data.TEXT "after" -> true | _ -> raise ( DBError ("Unexpected gua side value in "^stateTable ) ) ) in
                   let subkey = match stateRow(3) with | Sqlite3.Data.TEXT x -> x | _ -> raise ( DBError ("Unexpected s1 value in "^stateTable ) )  in
                   let sub = selectState subkey in
                      ASTD_state.guard_s_of tested sub
       
        | Sqlite3.Data.TEXT "cal" -> debug "Je suis une cal" ; 
                   let called = ( match stateRow(5) with | Sqlite3.Data.TEXT  "nevercalled" -> false | Sqlite3.Data.TEXT "called" -> true | _ -> raise ( DBError ("Unexpected cal side value in "^stateTable ) ) ) in
                   let subkey = match stateRow(3) with | Sqlite3.Data.TEXT x -> x | _ -> raise ( DBError ("Unexpected s1 value in "^stateTable ) )  in
                   let sub = selectState subkey in
                      ASTD_state.call_s_of called sub
                   
        | Sqlite3.Data.TEXT "qsy" -> debug "Je suis une qsy" ; 
                   begin  
                          let newdomain1 = selectDomain (currentkey^":d1") in
                          let newdomain2 = selectDomain (currentkey^":d2") in
                          let newdomain3 = selectDomain (currentkey^":d3") in
                          let subkey = match stateRow(3) with | Sqlite3.Data.TEXT x -> x | _ -> raise ( DBError ("Unexpected s1 value in "^stateTable ) )  in
                          let sub = selectState subkey in
                          ASTD_state.qsynchronisation_s_of newdomain1 newdomain2 newdomain3 sub
                   end  

        | Sqlite3.Data.TEXT "qch" -> debug "Je suis une qch" ; 
                   begin  
                          let newmap = selectMap currentkey in                           
                          let newdomain1 = selectDomain (currentkey^":d1'") in
                          let newdomain2 = selectDomain (currentkey^":d2'") in        
                          let valmap = ( match newmap with
                            | (a,b)::_ -> (ASTD_state.Val(a),b)
                            | [] -> let subkey = match stateRow(3) with | Sqlite3.Data.TEXT x -> x | _ -> raise ( DBError ("Unexpected s1 value in "^stateTable ) )  in
                                    let sub = selectState subkey in
                                          ( ASTD_state.ChoiceNotMade , sub )  
                                    )
                            in
                          ASTD_state.qchoice_s_of (fst valmap) newdomain1 newdomain2 (snd valmap)
                   end
        
        
        | Sqlite3.Data.TEXT "elem" -> debug "Je suis un Elem" ; ASTD_state.Elem
        | Sqlite3.Data.TEXT "notdefined" -> debug "Je suis une Nulle" ; ASTD_state.NotDefined
        | _ ->  raise ( DBError ("Unexpected stateType value in "^stateTable ) ) ; 

and selectState sId = 
        begin
            debug ("Find State "^ sId ); 
            rcTest (Sqlite3.reset preparedSelectState) ;
            rcTest (Sqlite3.bind preparedSelectState 1 (Sqlite3.Data.TEXT sId ));
            rcTest (Sqlite3.step preparedSelectState );
            newArray2state sId ;
        end

let db2state key = debug ("State2DB using STATEMENTS ======> looking for "^ key); 
                   selectState key ;

