%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Trace utils, chasing cpu hoggs and such
%%%x
%%%    test run:
%%%       trace:start(existing, [{time,5000}])
%%% @end
%%% Created : 10 May 2022 by Tony Rogvall <tony@rogvall.se>

-module(trace).

-export([start/0, start/1,start/2, start/3, start/4]).
-export([help/0]).
-export([options/0, options/1]).

-record(emon,
	{
	  ptab,         %% process/port table
	  ntab,         %% process/port name table
	  etab,         %% event table
	  esize         %% max event table size
	}).

-define(DEFAULT_BUFFER,    (100*1024)).  %% 100K default sample buffer
-define(DEFAULT_TIME,      1000).  %% 1s default sample time
-define(DEFAULT_TIMEOUT,   5000).  %% 5s trigger timeout
-define(DEFAULT_DELAY,     0).     %% 0s delay before trigger check
-define(DEFAULT_PREBUFFER, 50).    %% default prebuffer size
-define(DEFAULT_INIT,      []).
-define(DEFAULT_TRIGGER,   fun(_Evt,_St) -> true end).
-define(DEFAULT_FLAGS, []).

-define(DEFAULT_MAX_SCHEDULE_TIME, 50).

start() ->
    start(all).

start(Spec) ->
    start_(Spec,[]).

start(Spec, Opts) ->
    start_(Spec, Opts).

start(M,F,A) when is_atom(M),is_atom(F),is_list(A) ->
    start_({M,F,A},[]).

start(M,F,A,Opts) when is_atom(M),is_atom(F),is_list(A),is_list(Opts) ->
    start_({M,F,A},Opts).

start_(Pid, Opts) when is_pid(Pid), is_list(Opts) ->
    start__(Pid,Opts);
start_(Fun, Opts) when is_function(Fun), is_list(Opts) ->
    start__(Fun,Opts);
start_(S={M,F,A},Opts) when is_atom(M),is_atom(F),is_list(A),is_list(Opts) ->
    start__(S,Opts);
start_(existing, Opts) ->
    start__(existing, Opts);
start_(new, Opts) ->
    start__(new, Opts);
start_(all, Opts) ->
    start__(all, Opts).

start__(Spec, Opts) ->
    spawn_link(fun() -> init(Spec,Opts) end).

options() ->
    [buffer,time,timeout,delay,prebuffer,trigger,init,flags].

options(flags) ->
    [set_on_spawn, set_on_first_spawn,
     set_on_link, set_on_first_link,
     send, 'receive', 
     procs, ports, exiting, garbage_collection].

default(buffer)      -> ?DEFAULT_BUFFER;
default(time)        -> ?DEFAULT_TIME;
default(timeout)     -> ?DEFAULT_TIMEOUT;
default(delay)       -> ?DEFAULT_DELAY;
default(prebuffer)   -> ?DEFAULT_PREBUFFER;
default(trigger)     -> ?DEFAULT_TRIGGER;
default(init)        -> ?DEFAULT_INIT;
default(flags)       -> ?DEFAULT_FLAGS;
default(add)         -> [];
default(delete)      -> [];
default(max_schedule_time) -> ?DEFAULT_MAX_SCHEDULE_TIME;
default(_)           -> undefined.

help() ->
    io:format("trace:start(Spec[,Options])\n"),
    io:format("  Spec: {M,F,A} | fun(Evt) -> boolean | existing | new | all\n"),
    io:format("  Options:\n"),
    io:format("    {buffer,N}    - Max number of samples stored (~w)\n",
	      [default(buffer)]),
    io:format("    {prebuffer,N} - Max number of samples before trigger (~w)\n",
	      [default(prebuffer)]),
    io:format("    {time,T}      - Max time to buffer in ms (~w)\n",
	      [default(time)]),
    io:format("    {max_schedule_time,T} - Max time to buffer in ms (~w)\n",
	      [default(max_schedule_time)]),
    io:format("    {delay,T}     - Delay in ms before trigger check (~w)\n",
	      [default(delay)]),
    io:format("    {timeout,T}    - Max time to wait for trigger in ms (~w)\n",
	      [default(timeout)]),
    io:format("    {flags,Fs}     - Trace flags to use (~w)\n",
	      [default(flags)]),
    io:format("    {add,Fs}       - Add trace flags to default flags\n", []),
    io:format("    {delete,Fs}    - Delete trace flags from default flags\n", []),
    io:format("    {trigger,Fun}  - Trigger function fun(Evt,St) -> true|St'\n",
	      []),
    io:format("    {init,Term}    - Trigger initial state (~w)\n",
	      [default(init)]).

init(Spec, Opts) ->
    ESize = getopt(buffer, Opts),
    Em = #emon { ptab = ets:new(emon, [public,set]),
		 ntab = ets:new(emon, [public,set]),
		 etab = ets:new(emon, [public,ordered_set]),
		 esize = ESize
	       },
    Collector = spawn_link(
		  fun() -> 
			  collect(Spec, Em, Opts)
		  end),
    %% Remove Collector / self from ptab 
    del_id(Em, Collector),
    del_id(Em, self()),

    Ref = erlang:monitor(process,Collector),

    receive
	{'DOWN',Ref,_, _, _} ->
	    display(Em, getopt(max_schedule_time,Opts))
    end.

display(Em, MaxScheduleTime) ->
    T = ets:first(Em#emon.etab),
    %% calculate all {T1,ID,in} - {T2,ID,out} warn when
    %% T2 - T1 > max
    io:format("CPU hoggers:\n"),
    io:format("-------------------\n"),
    display(Em, T, #{}, MaxScheduleTime),
    io:format("-------------------\n"),
    ok.
    

display(_Em, '$end_of_table', _IN, _MaxScheduleTime) ->
    ok;
display(Em, Ti, IN, MaxScheduleTime) ->
    case ets:lookup(Em#emon.etab, Ti) of
	[{Ti,Pid,in}] ->
	    Tj = ets:next(Em#emon.etab, Ti),
	    display(Em, Tj, IN#{ Pid => Ti }, MaxScheduleTime);
	[{Ti,Pid,out}] ->
	    case maps:get(Pid, IN, undefined) of
		undefined ->
		    ok;
		Tin ->
		    Td = erlang:convert_time_unit((Ti - Tin),
						  nanosecond, millisecond),
		    if Td > MaxScheduleTime ->
			    io:format("Warning: pid=~p running for ~wms\n",
				      [Pid,Td]);
		       true ->
			    ok
		    end
	    end,
	    Tj = ets:next(Em#emon.etab, Ti),
	    display(Em, Tj, IN#{ Pid => undefined },MaxScheduleTime);
	[{_T,_Pid,trigger}] ->
	    Tj = ets:next(Em#emon.etab, Ti),
	    display(Em, Tj, IN, MaxScheduleTime)
    end.
   

collect(Spec, Em, Opts) ->
    Trigger = getopt(trigger, Opts),
    TriggerArg = getopt(trigger_arg, Opts),
    Time = getopt(time, Opts),
    Delay = getopt(delay, Opts),
    Timeout = getopt(timeout,Opts),
    Flags = filter_flags(getopt(flags, Opts) ++ getopt(add,Opts)) --
	getopt(delete, Opts),
    What = run(Spec),
    erlang:trace(What, true, [monotonic_timestamp, running | Flags]),
    PreBuffer = getopt(prebuffer, Opts),
    Buf0 = evt_buffer_new(PreBuffer),
    %% Wait for Delay ms before checking for trigger
    DelayRef = erlang:start_timer(Delay, self(), delay),
    Buf1 = delay_loop(DelayRef,Buf0),
    %% Wait for Trigger or Timeout ms
    TimeoutRef = erlang:start_timer(Timeout,self(),trigger),
    trigger_loop(Trigger,TriggerArg,Em,TimeoutRef,Buf1),
    DoneRef = erlang:start_timer(Time, self(), collect),
    collect_loop(DoneRef,Em).

%%
%% Delay before trigger test
%%
delay_loop(Timer,Buf) ->
    receive
	Ts when is_tuple(Ts), element(1,Ts) =:= trace_ts ->
	    case internal_ts(Ts) of
		undefined ->
		    delay_loop(Timer,Buf);
		Evt ->
		    delay_loop(Timer,evt_buffer_add_element(Evt,Buf))
	    end;
	{timeout,Timer,_} ->
	    Buf
    end.

%%
%% Wait for trigger condition before buffering
%%
trigger_loop(Fun,Arg,Em,Timer,Buf) ->
    receive
	Ts when is_tuple(Ts), element(1,Ts) =:= trace_ts ->
	    case internal_ts(Ts) of
		undefined ->
		    trigger_loop(Fun,Arg,Em,Timer,Buf);
		Evt ->
		    case Fun(Evt,Arg) of
			true ->
			    cancel(Timer),
			    add_buffer_events(Em,Buf),
			    add_event(Em,Evt),
			    add_event(Em,{element(1,Evt),self(),trigger});
			Arg1 ->
			    trigger_loop(Fun,Arg1,Em,Timer,
					 evt_buffer_add_element(Evt,Buf))
		    end
	    end;
	{timeout,Timer,_} ->
	    add_event(Em,{erlang:monotonic_time(nanosecond),self(),trigger}),
	    add_buffer_events(Em,Buf)
    end.

add_buffer_events(Em,Buf) ->
    EvtList = evt_buffer_to_list(Buf),
    io:format("Adding ~w pre buffered events\n", [length(EvtList)]),
    lists:foreach(fun(Evt) ->
			  add_event(Em,Evt) 
		  end, EvtList).

%%
%% Collect data until done message is received
%%
collect_loop(Timer,Em) ->
    receive
	Ts when is_tuple(Ts), element(1,Ts) =:= trace_ts ->
	    Evt = internal_ts(Ts),
	    add_event(Em,Evt),
	    collect_loop(Timer,Em);
	{timeout,Timer,_} ->
	    ok;
	What ->
	    io:format("emon:collect_loop got message ~p\n", [What]),
	    collect_loop(Timer,Em)
    end.

%%
run({M,F,A}) ->
    spawn(M,F,A);  %% spawn link? monitor?
run(Fun) when is_function(Fun) ->
    spawn(Fun);    %% spawn link? monitor?
run(What) -> What.

%% running
internal_ts({trace_ts,ID,in,_,T}) -> {T,ID,in};
internal_ts({trace_ts,ID,out,_,T}) -> {T,ID,out};
%% garbage_collection
internal_ts({trace_ts,ID,gc_start,_Info,T}) ->{T,ID,gc_start};
internal_ts({trace_ts,ID,gc_end,_Info,T}) -> {T,ID,gc_end};
internal_ts({trace_ts,ID,gc_minor_start,_Info,T}) ->{T,ID,gc_start};
internal_ts({trace_ts,ID,gc_minor_end,_Info,T}) -> {T,ID,gc_end};
internal_ts({trace_ts,ID,gc_major_start,_Info,T}) ->{T,ID,gc_start};
internal_ts({trace_ts,ID,gc_major_end,_Info,T}) -> {T,ID,gc_end};
%% send
internal_ts({trace_ts,ID,send,_Msg,DST,T}) -> {T,ID,{send,DST}};
internal_ts({trace_ts,ID,send_to_non_existing,_Msg,DST,T}) -> {T,ID,{send,DST}};
%% receive
internal_ts({trace_ts,ID,'receive',timeout,T})-> {T,ID,{'receive',timeout}};
internal_ts({trace_ts,ID,'receive',_,T})-> {T,ID,{'receive',message}};
%% exiting
internal_ts({trace_ts,ID,in_exiting,_,T}) ->{T,ID,in_exiting};
internal_ts({trace_ts,ID,out_exiting,_,T}) ->{T,ID,out_exiting};
internal_ts({trace_ts,ID,out_exited,_,T}) -> {T,ID,out_exited};
%% proc
internal_ts({trace_ts,ID,spawn,ID2,_MFa,T}) -> {T,ID,{spawn,ID2}};
internal_ts({trace_ts,ID,exit,Reason,T}) -> {T,ID,{exit,Reason}};
internal_ts({trace_ts,ID,register,Name,T}) -> {T,ID,{register,Name}};    
internal_ts({trace_ts,ID,unregister,Name,T}) -> {T,ID,{unregister,Name}};
internal_ts({trace_ts,ID,link,ID2,T}) -> {T,ID,{link,ID2}};
internal_ts({trace_ts,ID,unlink,ID2,T}) -> {T,ID,{unlink,ID2}};
internal_ts({trace_ts,ID,getting_linked,ID2,T}) -> {T,ID,{linked,ID2}};
internal_ts({trace_ts,ID,getting_unlinked,ID2,T}) -> {T,ID,{unlinked,ID2}};
%% Other
internal_ts(Ts) ->
    io:format("internal_ts: not handled ~p\n", [Ts]),
    undefined.

add_event(Em,undefined) ->
    Em;
add_event(Em,{Ts,ID,Info}) ->
    add_id(Em, ID),
    case Info of
	%% Only ports! processes are handled trough set_on_link,
	%% set_on_spawn ...
	{send,ID2} when is_port(ID2) -> add_id(Em,ID2);
	{link,ID2} when is_port(ID2) -> add_id(Em,ID2);
	{unlink,ID2} when is_port(ID2) -> add_id(Em,ID2);
	%% Maybe check names while drawing them for the current view?
	{register,Name} -> add_name(Em,ID,Name);
	{unregister,_Name} -> %% mark when name disappered? 
	    ok;
	_ -> 
	    ok
    end,
    ets:insert(Em#emon.etab, {Ts,ID,Info}),
    Size = ets:info(Em#emon.etab, size),
    if Size > trunc(Em#emon.esize*1.25) ->
	    del_evt(Em#emon.etab, ets:first(Em#emon.etab),
		      Size-trunc(Em#emon.esize*0.75)),
	    Em;
       true ->
	    Em
    end.


add_id(Em,ID) ->
    case ets:member(Em#emon.ptab, ID) of
	false ->
	    I = ets:info(Em#emon.ptab, size) div 2,
	    io:format("Trace: add id=~p index=~w\n", [ID,I]),
	    ets:insert(Em#emon.ptab, {ID,I,{out,false}}),
	    ets:insert(Em#emon.ptab, {I,ID});
	true ->
	    ok
    end.

del_id(Em, ID) ->
    case ets:lookup(Em#emon.ptab, ID) of
	[] ->
	    ok;
	[{ID,I,_}] ->
	    ets:delete(Em#emon.ptab, ID),
	    ets:delete(Em#emon.ptab, I)
    end.

add_name(_Em,_ID,undefined) ->
    ok;
add_name(Em,ID,Name) ->
    ets:insert(Em#emon.ntab, {ID,Name}),
    ets:insert(Em#emon.ntab, {Name,ID}).

%% remove events
del_evt(_ETab, _Ti, 0) ->
    ok;
del_evt(_ETab, '$end_of_table', 0) ->
    ok;
del_evt(ETab, Ti, I) ->
    Tj = ets:next(ETab, Ti),
    ets:delete(ETab, Ti),
    del_evt(ETab, Tj, I-1).


filter_flags(Fs) ->
    filter_flags(Fs,[]).

filter_flags([Flag|Fs],Ac) ->
    case Flag of
	running -> filter_flags(Fs,Ac);   %% always added
	timestamp -> filter_flags(Fs,Ac);   %% always added
	monotonic_timestamp -> filter_flags(Fs,Ac);   %% always added
	strict_monotonic_timestamp -> filter_flags(Fs,Ac);   %% always added
	cpu_timestamp -> filter_flags(Fs,Ac);   %% always added
	set_on_spawn -> filter_flags(Fs,[Flag|Ac]);
	set_on_link  -> filter_flags(Fs,[Flag|Ac]);
	set_on_first_spawn -> filter_flags(Fs,[Flag|Ac]);
	set_on_first_link  -> filter_flags(Fs,[Flag|Ac]);
	send -> filter_flags(Fs,[Flag|Ac]);
	'receive' -> filter_flags(Fs,[Flag|Ac]);
	garbage_collection -> filter_flags(Fs,[Flag|Ac]);
	procs -> filter_flags(Fs,[Flag|Ac]);
	ports -> filter_flags(Fs,[Flag|Ac]);
	exiting -> filter_flags(Fs,[Flag|Ac]);
	_ -> filter_flags(Fs,Ac)
    end;
filter_flags([], Ac) ->
    lists:usort(Ac).


getopt(Key, Opts) ->
    case lists:keysearch(Key, 1, Opts) of
	false ->
	    case lists:member(Key, Opts) of
		true -> true;  %% boolean, present = true
		false -> default(Key)
	    end;
	{value,{_,Value}} ->
	    Value
    end.

%% Cancel and flush timer
cancel(Ref) ->
    case erlang:cancel_timer(Ref) of
	false ->
	    receive {timer,Ref,_} -> ok
	    after 0 -> ok
	    end;
	_ -> ok
    end.


%% Fixed size event buffer
-record(evt_buffer,
	{
	  max_size,
	  cur_size,
	  buf1,
	  buf2
	 }).

evt_buffer_new(Size) ->
    #evt_buffer { max_size=Size, cur_size=0, buf1=[], buf2=[] }.

%% evt_buffer_new(Size,Buffer) ->
%%     Buf1 = lists:sublist(Buffer,1,Size),
%%     CurSize = length(Buf1),
%%     #evt_buffer { max_size=Size, cur_size=CurSize, 
%% 		  buf1=lists:reverse(Buf1),buf2=[] }.

evt_buffer_add_element(_Evt,EvtBuf) when EvtBuf#evt_buffer.max_size == 0->
    EvtBuf;
evt_buffer_add_element(Evt,EvtBuf) ->
    Size = EvtBuf#evt_buffer.cur_size+1,
    if Size > EvtBuf#evt_buffer.max_size ->
	    %% swap
	    EvtBuf#evt_buffer { cur_size=1, 
				buf1=[Evt], buf2=EvtBuf#evt_buffer.buf1};
       true ->
	    EvtBuf#evt_buffer { cur_size=Size,
				buf1=[Evt|EvtBuf#evt_buffer.buf1]}
    end.

evt_buffer_to_list(EvtBuf) when EvtBuf#evt_buffer.max_size == 0 ->
    [];
evt_buffer_to_list(EvtBuf) ->
    lists:reverse(lists:sublist(EvtBuf#evt_buffer.buf2,1,
				EvtBuf#evt_buffer.max_size -
				    EvtBuf#evt_buffer.cur_size)) ++ 
	lists:reverse(EvtBuf#evt_buffer.buf1).
