%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Data.Global:
%

% initialize the predicate containing the global value if called for the
% first time:
initGlobalTmpValue(GlobName,Exp,Val) :-
	evalToken(Eval),
	user:hnf(Exp,GlobVal,Eval,_E1),
        GlobVal = 'Data.Global.GlobalT'(Val),
	GlobalHead =.. [GlobName,_],
        user:retractClause(GlobalHead,_),
	NewGlobalCall =.. [GlobName,Val],
	% temporary globals directly contain its value:
	assertz(user:NewGlobalCall),
	!.

% read a global value:
'Data.Global.prim_readGlobalT'(Global,Val) :-
        checkGlobalEntity(Global,GlobName),
	GlobalCall =.. [GlobName,Val],
	call(user:GlobalCall), !.

% update a global value:
'Data.Global.prim_writeGlobalT'(Global,NewVal,'Prelude.()') :-
        checkGlobalEntity(Global,GlobName),
	GlobalCall =.. [GlobName,_],
	(retract(user:GlobalCall) ; user:retractClause(GlobalCall,_)),
	NewGlobalCall =.. [GlobName,NewVal],
	assertz(user:NewGlobalCall), !.

checkGlobalEntity('Data.Global.GlobalT'(GlobName),GlobName) :-
        atom(GlobName), !.
checkGlobalEntity(Global,_) :-
        %writeLnErr(Global),
        raise_exception('Illegal global entity occurred (maybe overloaded?)').
