    function
    | True               -> 
    | False              -> 
    | Process (n,env0)   -> 
    | Predicate (p,env0) -> 
    | And (df1, df2)     -> 
    | Or (df1, df2)      -> 
    | Forall (v, s, df)  -> 
    | Exists (v, s, df)  -> 

    function 
    | Box                              -> 
    | Lambda                           ->
    | Action (label, params)           ->
    | Closure (env, proc)              ->
    | Guard (name, params, proc)       ->
    | Kleene proc                      ->
    | Qsynchro (delta, var, set, proc) ->
    | Qchoice (var, set, proc)         ->
    | Synchro (delta, left, right)     ->
    | Choice (left, right)             ->
    | Sequence (left, right)           ->
    | ProcessCall (name, params)       ->
