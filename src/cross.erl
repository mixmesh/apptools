%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    xref front end
%%% @end
%%% Created : 16 Oct 2020 by Tony Rogvall <tony@rogvall.se>

-module(cross).

-export([start/1]).

start(Ms=[_Arg|_]) when is_atom(_Arg) ->
    lists:foreach(
	fun(Arg) ->
		Mod = try_load(Arg),
		case xref:m(Mod) of
		    {error,Module,Message} ->
			String = apply(Module, format_error, [Message]),
			io:format("E ~s\n", [String]),
			ignore;
		    XRef ->
			lists:foreach(
			  fun({{M,F,A},{Mu,Fu,Au}}) ->
				  io:format("U ~s:~s/~w "
					    "called from ~s:~s/~w\n",
					    [Mu,Fu,Au, M,F,A])
			  end, proplists:get_value(undefined, XRef, []))
		end
	end, Ms).

try_load(Arg) ->
    Filename = atom_to_list(Arg),
    case filename:extension(Filename) of
	".beam" ->
	    case file:read_file(Filename) of
		{ok,Binary} ->
		    Mod = list_to_atom(filename:basename(Filename,".beam")),
		    %% io:format("load ~s file=~s\n", [Mod, Filename]),
		    case code:load_binary(Mod, Filename, Binary) of
			{module, M} ->
			    M
		    end
	    end;
	"" ->
	    Module = list_to_atom(filename:basename(Filename)),
	    code:purge(Module),
	    {module,M} = code:load_file(Module),
	    M
    end.

	    
			
	    
	
