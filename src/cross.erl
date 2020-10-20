%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Cross reference 
%%%    list functions/modules/applications used
%%%    can be used to find out application dependencies
%%% @end
%%% Created : 16 Oct 2020 by Tony Rogvall <tony@rogvall.se>

-module(cross).

-export([start/1]).
-export([load/1]).
-export([load_beam/1, load_beam/2]).
-export([calling_functions/1]).
-export([calling_modules/1]).
-export([calling_apps/1]).


-define(VAR_EXPR, '$F_EXPR').
-define(MOD_EXPR, '$M_EXPR').


start(Ms=[_Arg|_]) when is_atom(_Arg) ->
    lists:foreach(
	fun(Arg) ->
		case load(Arg) of
		    {error, Err} ->
			io:format("error: ~w\n", [Err]),
			ignore;
		    {ok,_Module,Info} ->
			maps:fold(
			  fun({{M,F,A},{Mu,Fu,Au}},_Ln,_Acc) ->
				  io:format("U ~s:~s/~w "
					    "called from ~s:~s/~w\n",
					    [Mu,Fu,Au, M,F,A])
			  end, ok, maps:get(unresolved, Info, #{}))
		end
	end, Ms).

%% external functions beeing called
calling_functions(Info) ->
    Calls = maps:get(external_call, Info, #{}),
    maps:fold(
      fun(_From, Called, Acc) ->
	      ordsets:union(Called,Acc)
      end, ordsets:new(), Calls).
    
%% calculate modules beeing called
calling_modules(Info) ->
    Calls = maps:get(external_call, Info, #{}),
    maps:fold(
      fun(_From, Called, Acc) ->
	      Ms = ordsets:from_list([M||{M,_,_}<-ordsets:to_list(Called)]),
	      ordsets:union(Ms,Acc)
      end, ordsets:new(), Calls).

calling_apps(Info) ->
    Modules = calling_modules(Info),
    ordsets:from_list([module_app(M) || M <- ordsets:to_list(Modules)]).

module_app(?MOD_EXPR) -> ?MOD_EXPR;
module_app(Mod) ->
    BeamFile = code:which(Mod),
    case lists:reverse(filename:split(BeamFile)) of
	[_Beam, "ebin", AppVsn | _] ->
	    case string:split(AppVsn, "-") of
		[App,_Vsn] -> list_to_atom(App);
		[App] -> list_to_atom(App)
	    end;
	_ ->
	    unknown
    end.
	

load(Filename) when is_list(Filename) ->
    case filename:extension(Filename) of
	".beam" ->
	    load_beam(Filename);
	"" ->
	    case code:which(list_to_atom(Filename)) of
		non_existing ->
		    {error, enoent};
		Path ->
		    load(Path)
	    end
    end;
load(Arg) when is_atom(Arg) ->
    Filename = atom_to_list(Arg),
    case filename:extension(Filename) of
	".beam" ->
	    load_beam(Filename);
	"" ->
	    case code:which(Arg) of
		non_existing ->
		    {error, enoent};
		Path ->
		    load(Path)
	    end
    end.

%% read abstract code from beam file

load_beam(File) ->
    load_beam(File,false).

load_beam(File, Builtins) ->
    case beam_lib:chunks(File, [abstract_code, exports, attributes]) of
	{ok, {M,[{abstract_code,NoA},_X,_A]}} when NoA =:= no_abstract_code ->
	    {ok, M, NoA};
	{ok, {M, [{abstract_code, {abstract_v1, Forms}},
                  {exports,X0}, {attributes,A}]}} ->
	    %% R7.
	    X = xref_utils:fa_to_mfa(X0, M),
            D = deprecated(A, X, M),
	    scan_module(M, Forms, Builtins, X, D);
	{ok, {M, [{abstract_code, {abstract_v2, Forms}},
                  {exports,X0}, {attributes,A}]}} ->
	    %% R8-R9B.
	    X = xref_utils:fa_to_mfa(X0, M),
            D = deprecated(A, X, M),
	    scan_module(M, Forms, Builtins, X, D);
	{ok, {M, [{abstract_code, {raw_abstract_v1, Code}},
                  {exports,X0}, {attributes,A}]}} ->
	    %% R9C-
            Forms0 = epp:interpret_file_attribute(Code),
	    Forms1 = erl_expand_records:module(Forms0, []),
	    Forms = erl_internal:add_predefined_functions(Forms1),
	    X = mfa_exports(X0, A, M),
            D = deprecated(A, X, M),
	    scan_module(M, Forms, Builtins, X, D);
	Error when element(1, Error) =:= error ->
	    Error
    end.

mfa_exports(X0, Attributes, M) ->
    %% Adjust arities for abstract modules.
    X1 = case xref_utils:is_abstract_module(Attributes) of
             true ->
                 [{F,adjust_arity(F,A)} || {F,A} <- X0];
             false ->
                 X0
         end,
    xref_utils:fa_to_mfa(X1, M).

adjust_arity(F, A) ->
    case xref_utils:is_static_function(F, A) of
        true -> A;
        false -> A - 1
    end.
	    
deprecated(A, X, M) ->
    DF = {[],[],[],[]},
    case lists:keysearch(deprecated, 1, A) of
        {value, {deprecated, D0}} ->
            depr(D0, M, DF, X, []);
        false ->
            {DF,[]}
    end.

depr([D | Depr], M, DF, X, Bad) ->
    case depr_cat(D, M, X) of
        {I,Dt} ->
            NDF = setelement(I, DF, Dt ++ element(I, DF)),
            depr(Depr, M, NDF, X, Bad);
        undefined ->
            depr(Depr, M, DF, X, [D | Bad])
    end;
depr([], _M, DF, _X, Bad) ->
    {DF, lists:reverse(Bad)}.

depr_cat({F, A, Flg}, M, X) ->
    case deprecated_flag(Flg) of
        undefined -> undefined;
        I -> depr_fa(F, A, X, M, I)
    end;
depr_cat({F, A}, M, X) ->
    depr_fa(F, A, X, M, 4);
depr_cat(module, M, X) ->
    depr_fa('_', '_', X, M, 4);
depr_cat(_D, _M, _X) ->
    undefined.

depr_fa('_', '_', X, _M, I) ->
    {I, X};
depr_fa(F, '_', X, _M, I) when is_atom(F) ->
    {I, lists:filter(fun({_,F1,_}) -> F1 =:= F end, X)};
depr_fa(F, A, _X, M, I) when is_atom(F), is_integer(A), A >= 0 ->
    {I, [{M,F,A}]};
depr_fa(_F, _A, _X, _M, _I) ->
    undefined.

%% deprecated_flag(Flag) -> integer() | undefined
%%   Maps symbolic flags for deprecated functions to their category indexes
%%   in the deprecation tuple.
%%
%%   {DF_1, DF_2, DF_3, DF}

deprecated_flag(next_version) -> 1;
deprecated_flag(next_major_release) -> 2;
deprecated_flag(eventually) -> 3;
deprecated_flag(String) -> depr_desc(String).

%% Strings fall into the general category, index 4.
depr_desc([Char | Str]) when is_integer(Char) -> depr_desc(Str);
depr_desc([]) -> 4;
depr_desc(_) -> undefined.


-record(xrefr,
	{module=[],
	 function=[],
	 def_at=[],
	 el = #{},        %%  #{ mfa() => ordset(mfa()) }
	 ex = #{},        %%  #{ mfa() => ordset(mfa()) }
	 l_call_at = #{}, %%  #{ {mfa(),mfa()} => line() }
	 x_call_at = #{}, %%  #{ {mfa(),mfa()} => line() }
	 x = [],
         df,
	 builtins_too=false,
         is_abstr,            % abstract module?
	 funvars=[],          % records variables bound to funs
			      % (for coping with list comprehension)
	 matches=[],          % records other bound variables
	 ur = #{},            % unresolved calls, #{ {mfa(),mfa()} => line()}
	 %% experimental; -xref(FunEdge) is recognized.
	 lattrs=[],            % local calls, {{mfa(),mfa()},Line}
	 xattrs=[],            % external calls, -"-
	 battrs=[],	       % badly formed xref attributes, term().
         on_load               % function name
	 }).

%% The versions of the abstract code are as follows:
%% R7:  abstract_v1
%% R8:  abstract_v2
%% R9C: raw_abstract_v1

%% -> {ok, Module, {DefAt, CallAt, LC, XC, X, Attrs}, Unresolved}} | EXIT
%% Attrs = {ALC, AXC, Bad}
%% ALC, AXC and Bad are extracted from the attribute 'xref'. An experiment.
scan_module(Module, Forms, CollectBuiltins, X, DF) ->
    Attrs = [{Attr,V} || {attribute,_Line,Attr,V} <- Forms],
    IsAbstract = xref_utils:is_abstract_module(Attrs),
    S = #xrefr{module = Module, builtins_too = CollectBuiltins,
               is_abstr = IsAbstract, x = X, df = DF},
    forms(Forms, S).

forms([F | Fs], S) ->
    S1 = form(F, S),
    forms(Fs, S1);
forms([], S) ->
    #xrefr{module = M, 
	   def_at = DefAt,
	   l_call_at = LCallAt, x_call_at = XCallAt,
	   el = LC, ex = XC, x = X, df = Depr, on_load = OnLoad,
	   lattrs = AL, xattrs = AX, battrs = B, ur = U} = S,
    OL = case OnLoad of
             undefined -> [];
             F ->
                 [{M, F, 0}]
         end,
    Attrs = {lists:reverse(AL), lists:reverse(AX), lists:reverse(B)},
    {ok, M, #{defined_at=>DefAt,
	      local_call_at=>LCallAt,
	      external_call_at=>XCallAt,
	      local_call=>LC,
	      external_call=>XC, 
	      exports=>X, 
	      attributes=>Attrs, 
	      deprecated=>Depr, 
	      on_load => OL,
	      unresolved => U }}.

form({attribute, Line, xref, Calls}, S) -> % experimental
    #xrefr{module = M, function = Fun,
	   lattrs = L, xattrs = X, battrs = B} = S,
    attr(Calls, erl_anno:line(Line), M, Fun, L, X, B, S);
form({attribute, _, on_load, {F, 0}}, S) ->
    S#xrefr{on_load = F};
form({attribute, _Line, _Attr, _Val}, S) ->
    S;
form({function, _, module_info, 0, _Clauses}, S) ->
    S;
form({function, _, module_info, 1, _Clauses}, S) ->
    S;
form({function, Anno, Name, Arity, Clauses}, S) ->
    MFA0 = {S#xrefr.module, Name, Arity},
    MFA = adj_arity(S, MFA0),
    S1 = S#xrefr{function = MFA},
    Line = erl_anno:line(Anno),
    S2 = S1#xrefr{def_at = [{MFA,Line} | S#xrefr.def_at]},
    S3 = clauses(Clauses, S2),
    S3#xrefr{function = []};
form(_, S) ->
    %% OTP 20. Other uninteresting forms such as {eof, _} and {warning, _}.
    %% Exposed because sys_pre_expand is no longer run.
    S.


clauses(Cls, S) ->
    #xrefr{funvars = FunVars, matches = Matches} = S,
    clauses(Cls, FunVars, Matches, S).

clauses([{clause, _Line, _H, G, B} | Cs], FunVars, Matches, S) ->
    S1 = case S#xrefr.builtins_too of
	     true -> expr(G, S);
	     false -> S
	 end,
    S2 = expr(B, S1),
    S3 = S2#xrefr{funvars = FunVars, matches = Matches},
    clauses(Cs, S3);
clauses([], _FunVars, _Matches, S) ->
    S.

attr(NotList, Ln, M, Fun, AL, AX, B, S) when not is_list(NotList) ->
    attr([NotList], Ln, M, Fun, AL, AX, B, S);
attr([E={From, To} | As], Ln, M, Fun, AL, AX, B, S) ->
    case mfa(From, M) of
	{_, _, MFA} when MFA =:= Fun; [] =:= Fun ->
	    attr(From, To, Ln, M, Fun, AL, AX, B, S, As, E);
	{_, _, _} ->
	    attr(As, Ln, M, Fun, AL, AX, [E | B], S);
	_ ->
	    attr(Fun, E, Ln, M, Fun, AL, AX, B, S, As, E)
    end;
attr([To | As], Ln, M, Fun, AL, AX, B, S) ->
    attr(Fun, To, Ln, M, Fun, AL, AX, B, S, As, To);
attr([], _Ln, _M, _Fun, AL, AX, B, S) ->
    S#xrefr{lattrs = AL, xattrs = AX, battrs = B}.

attr(From, To, Ln, M, Fun, AL, AX, B, S, As, E) ->
    case {mfa(From, M), mfa(To, M)} of
	{{true,_,F}, {_,external,T}} ->
	    attr(As, Ln, M, Fun, AL, [{{F,T},Ln} | AX], B, S);
	{{true,_,F}, {_,local,T}} ->
	    attr(As, Ln, M, Fun, [{{F,T},Ln} | AL], AX, B, S);
	_ -> attr(As, Ln, M, Fun, AL, AX, [E | B], S)
    end.

mfa({F,A}, M) when is_atom(F), is_integer(A) ->
    {true, local, {M,F,A}};
mfa(MFA={M,F,A}, M1) when is_atom(M), is_atom(F), is_integer(A) ->
    {M=:=M1, external, MFA};
mfa(_, _M) -> false.

expr({'if', _Line, Cs}, S) ->
    clauses(Cs, S);
expr({'case', _Line, E, Cs}, S) ->
    S1 = expr(E, S),
    clauses(Cs, S1);
expr({'receive', _Line, Cs}, S) ->
    clauses(Cs, S);
expr({'receive', _Line, Cs, To, ToEs}, S) ->
    S1 = expr(To, S),
    S2 = expr(ToEs, S1),
    clauses(Cs, S2);
expr({'try',_Line,Es,Scs,Ccs,As}, S) ->
    S1 = expr(Es, S),
    S2 = clauses(Scs, S1),
    S3 = clauses(Ccs, S2),
    expr(As, S3);
expr({'fun', Line, {function,M,F,A}}, S)
  when is_atom(M), is_atom(F), is_integer(A) ->
    %% This is the old format for external funs, generated by a pre-R15
    %% compiler. Exposed in OTP 20 because sys_pre_expand is no longer
    %% run.
    Fun = {'fun', Line, {function, {atom,Line,M},
			 {atom,Line,F},
			 {integer,Line,A}}},
    expr(Fun, S);
expr({'fun', Line, {function, {atom,_,Mod},
		    {atom,_,Name},
		    {integer,_,Arity}}}, S) ->
    %% New format in R15. M:F/A (literals).
    As = lists:duplicate(Arity, {atom, Line, foo}),
    external_call(Mod, Name, As, Line, false, S);
expr({'fun', Line, {function, Mod, Name, {integer,_,Arity}}}, S) ->
    %% New format in R15. M:F/A (one or more variables).
    As = lists:duplicate(Arity, {atom, Line, foo}),
    external_call(erlang, apply, [Mod, Name, list2term(As)], Line, true, S);
expr({'fun', Line, {function, Mod, Name, _Arity}}, S) ->
    %% New format in R15. M:F/A (one or more variables).
    As = {var, Line, '_'},
    external_call(erlang, apply, [Mod, Name, As], Line, true, S);
%% Only abstract_v1 and abstract_v2.
expr({'fun', Line, {function, Name, Arity}, _Extra}, S) ->
    %% Added in R8.
    handle_call(local, S#xrefr.module, Name, Arity, Line, S);
expr({'fun', _Line, {clauses, Cs}, _Extra}, S) ->
    clauses(Cs, S);
%% End abstract_v1 and abstract_v2.
expr({'fun', Line, {function, Name, Arity}}, S) ->
    %% Added in OTP 20.
    handle_call(local, S#xrefr.module, Name, Arity, Line, S);
expr({'fun', _Line, {clauses, Cs}}, S) ->
    clauses(Cs, S);
expr({named_fun, _Line, '_', Cs}, S) ->
    clauses(Cs, S);
expr({named_fun, _Line, Name, Cs}, S) ->
    S1 = S#xrefr{funvars = [Name | S#xrefr.funvars]},
    clauses(Cs, S1);
expr({call, Line, {atom, _, Name}, As}, S) ->
    S1 = handle_call(local, S#xrefr.module, Name, length(As), Line, S),
    expr(As, S1);
expr({call, Line, {remote, _Line, {atom,_,Mod}, {atom,_,Name}}, As}, S) ->
    external_call(Mod, Name, As, Line, false, S);
expr({call, Line, {remote, _Line, Mod, Name}, As}, S) ->
    %% Added in R8.
    external_call(erlang, apply, [Mod, Name, list2term(As)], Line, true, S);
expr({call, Line, F, As}, S) ->
    external_call(erlang, apply, [F, list2term(As)], Line, true, S);
expr({match, _Line, {var,_,Var}, {'fun', _, {clauses, Cs}, _Extra}}, S) ->
    %% This is what is needed in R7 to avoid warnings for the functions
    %% that are passed around by the "expansion" of list comprehension.
    S1 = S#xrefr{funvars = [Var | S#xrefr.funvars]},
    clauses(Cs, S1);
expr({match, _Line, {var,_,Var}, {'fun', _, {clauses, Cs}}}, S) ->
    %% OTP 20. Exposed because sys_pre_expand is no longer run.
    S1 = S#xrefr{funvars = [Var | S#xrefr.funvars]},
    clauses(Cs, S1);
expr({match, _Line, {var,_,Var}, {named_fun, _, _, _} = Fun}, S) ->
    %% OTP 20. Exposed because sys_pre_expand is no longer run.
    S1 = S#xrefr{funvars = [Var | S#xrefr.funvars]},
    expr(Fun, S1);
expr({match, _Line, {var,_,Var}, E}, S) ->
    %% Used for resolving code like
    %%     Args = [A,B], apply(m, f, Args)
    S1 = S#xrefr{matches = [{Var, E} | S#xrefr.matches]},
    expr(E, S1);
expr({op, _Line, 'orelse', Op1, Op2}, S) ->
    expr([Op1, Op2], S);
expr({op, _Line, 'andalso', Op1, Op2}, S) ->
    expr([Op1, Op2], S);
expr({op, Line, Op, Operand1, Operand2}, S) ->
    external_call(erlang, Op, [Operand1, Operand2], Line, false, S);
expr({op, Line, Op, Operand}, S) ->
    external_call(erlang, Op, [Operand], Line, false, S);
expr(T, S) when is_tuple(T) ->
    expr(tuple_to_list(T), S);
expr([E | Es], S) ->
    expr(Es, expr(E, S));
expr(_E, S) ->
    S.

%% Mod and Fun may not correspond to something in the abstract code,
%% which is signalled by X =:= true.
external_call(Mod, Fun, ArgsList, Line, X, S) ->
    Arity = length(ArgsList),
    W = case xref_utils:is_funfun(Mod, Fun, Arity) of
	    true when erlang =:= Mod, apply =:= Fun, 2 =:= Arity -> apply2;
	    true when erts_debug =:= Mod, apply =:= Fun,4 =:= Arity -> debug4;
	    true when erlang =:= Mod, spawn_opt =:= Fun -> Arity - 1;
	    true -> Arity;
	    false when Mod =:= erlang ->
		case erl_internal:type_test(Fun, Arity) of
		    true -> type;
		    false -> false
		end;
	    false -> false
	end,
    S1 = if
	     W =:= type; X ->
		 S;
	     true ->
		 handle_call(external, Mod, Fun, Arity, Line, S)
	 end,
    case {W, ArgsList} of
	{false, _} ->
	    expr(ArgsList, S1);
	{type, _} ->
	    expr(ArgsList, S1);
	{apply2, [{tuple, _, [M,F]}, ArgsTerm]} ->
	    eval_args(M, F, ArgsTerm, Line, S1, ArgsList, []);
	{1, [{tuple, _, [M,F]} | R]} ->	% R = [] unless spawn_opt
	    eval_args(M, F, list2term([]), Line, S1, ArgsList, R);
	{2, [Node, {tuple, _, [M,F]} | R]} -> % R = [] unless spawn_opt
	    eval_args(M, F, list2term([]), Line, S1, ArgsList, [Node | R]);
	{3, [M, F, ArgsTerm | R]} -> % R = [] unless spawn_opt
	    eval_args(M, F, ArgsTerm, Line, S1, ArgsList, R);
	{4, [Node, M, F, ArgsTerm | R]} -> % R = [] unless spawn_opt
	    eval_args(M, F, ArgsTerm, Line, S1, ArgsList, [Node | R]);
	{debug4, [M, F, ArgsTerm, _]} ->
	    eval_args(M, F, ArgsTerm, Line, S1, ArgsList, []);
	_Else -> % apply2, 1 or 2
	    check_funarg(W, ArgsList, Line, S1)
    end.

eval_args(Mod, Fun, ArgsTerm, Line, S, ArgsList, Extra) ->
    {IsSimpleCall, M, F} = mod_fun(Mod, Fun),
    case term2list(ArgsTerm, [], S) of
	undefined ->
	    S1 = unresolved(M, F, -1, Line, S),
	    expr(ArgsList, S1);
	ArgsList2 when not IsSimpleCall ->
	    S1 = unresolved(M, F, length(ArgsList2), Line, S),
	    expr(ArgsList, S1);
	ArgsList2 when IsSimpleCall ->
	    S1 = expr(Extra, S),
	    external_call(M, F, ArgsList2, Line, false, S1)
    end.

mod_fun({atom,_,M1}, {atom,_,F1}) -> {true, M1, F1};
mod_fun({atom,_,M1}, _) -> {false, M1, ?VAR_EXPR};
mod_fun(_, {atom,_,F1}) -> {false, ?MOD_EXPR, F1};
mod_fun(_, _) -> {false, ?MOD_EXPR, ?VAR_EXPR}.

check_funarg(W, ArgsList, Line, S) ->
    {FunArg, Args} = fun_args(W, ArgsList),
    S1 = case funarg(FunArg, S) of
	     true ->
		 S;
	     false when is_integer(W) -> % 1 or 2
		 unresolved(?MOD_EXPR, ?VAR_EXPR, 0, Line, S);
	     false -> % apply2
		 N = case term2list(Args, [], S) of
			 undefined -> -1;
			 As -> length(As)
		     end,
		 unresolved(?MOD_EXPR, ?VAR_EXPR, N, Line, S)
	 end,
    expr(ArgsList, S1).

funarg({'fun', _, _Clauses, _Extra}, _S) -> true;
funarg({'fun', _, {clauses, _}}, _S) ->
    %% OTP 20. sys_pre_expand not run.
    true;
funarg({'fun', _, {function, _, _}}, _S) ->
    %% OTP 20. sys_pre_expand not run.
    true;
funarg({'fun', _, {function,_,_,_}}, _S) ->
    %% New abstract format for fun M:F/A in R15.
    true;
funarg({named_fun, _, _, _}, _S) ->
    %% OTP 20. sys_pre_expand not run.
    true;
funarg({var, _, Var}, S) -> lists:member(Var, S#xrefr.funvars);
funarg(_, _S) -> false.

fun_args(apply2, [FunArg, Args]) -> {FunArg, Args};
fun_args(1, [FunArg | Args]) -> {FunArg, Args};
fun_args(2, [_Node, FunArg | Args]) -> {FunArg, Args}.

list2term(L) ->
    A = erl_anno:new(0),
    list2term(L, A).

list2term([A | As], Anno) ->
    {cons, Anno, A, list2term(As)};
list2term([], Anno) ->
    {nil, Anno}.

term2list({cons, _Line, H, T}, L, S) ->
    term2list(T, [H | L], S);
term2list({nil, _Line}, L, _S) ->
    lists:reverse(L);
term2list({var, _, Var}, L, S) ->
    case lists:keysearch(Var, 1, S#xrefr.matches) of
	{value, {Var, E}} ->
	    term2list(E, L, S);
	false ->
	    undefined
    end;
term2list(_Else, _L, _S) ->
    undefined.

unresolved(M, F, A, Line, S) ->
    handle_call(external, {M,F,A}, Line, S, true).

handle_call(Locality, Module, Name, Arity, Line, S) ->
    case xref_utils:is_builtin(Module, Name, Arity) of
	true when not S#xrefr.builtins_too -> S;
	_Else ->
	    To = {Module, Name, Arity},
	    handle_call(Locality, To, Line, S, false)
    end.

handle_call(Locality, To0, Anno, S, IsUnres) ->
    From = S#xrefr.function,
    To = adj_arity(S, To0),
    Line = erl_anno:line(Anno),
    S1 = if
             IsUnres ->
		 Ur = update_map_list(S#xrefr.ur,{From,To},Line),
                 S#xrefr{ ur = Ur };
             true ->
                 S
         end,
    case Locality of
        local ->
	    LCall = update_map_ordset(S#xrefr.el, From, To),
	    LCallAt = update_map_list(S#xrefr.l_call_at,{From,To},Line),
	    S1#xrefr{el = LCall, l_call_at = LCallAt };
        external ->
	    XCall = update_map_ordset(S#xrefr.ex, From, To),
	    XCallAt = update_map_list(S#xrefr.x_call_at,{From,To},Line),
            S1#xrefr{ex = XCall, x_call_at = XCallAt}
    end.

update_map_ordset(Map, A, B) ->
    case maps:find(A, Map) of
	error -> Map#{ A => ordsets:from_list([B]) };
	{ok,Bs} ->  Map#{ A => ordsets:add_element(B,Bs) }
    end.

update_map_list(Map, A, B) ->
    case maps:find(A, Map) of
	error -> Map#{ A => [B] };
	{ok,Bs} ->  Map#{ A => [B|Bs] }
    end.

adj_arity(#xrefr{is_abstr = true, module = M}, {M, F, A} = MFA) ->
    case xref_utils:is_static_function(F, A) of
        true ->
            MFA;
        false ->
            {M,F,A-1}
    end;
adj_arity(_S, MFA) ->
    MFA.
