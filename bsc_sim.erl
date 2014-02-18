%%%==============================================================
%%% @Copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @Author Robert Virding <robert.virding@erlang-solutions.com>
%%% @doc Simulate traffic for a BSC.
%%% @end
%%%==============================================================

%% This is a traffic generator for the BSC. When starting we specify
%% how many phones we want to run. The simulator then starts this many
%% phones. For each phone a standard phone controller FSM is started
%% together with a phone simulator. For a run we give a tick time,
%% every so many msecs a call is made from a random phone to a random
%% phone. Here we don't check whether the phone are in use or not,
%% that is left to the software to handle.

-module(bsc_sim).

-behaviour(gen_server).

%% User API.
-export([start_link/1,start/1,stop/1]).
-export([start_run/2,stop_run/1,get_status/1]).

%% Behaviour callbacks.
-export([init/1,terminate/2,handle_call/3,handle_cast/2,
	 handle_info/2,code_change/3]).

-record(st, {n,calls,arr,to=infinity}).

%% Management API.

start_link(N) ->
    gen_server:start_link(?MODULE, N, []).

start(N) ->
    gen_server:start(?MODULE, N, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%% User API for starting and stopping a run.

start_run(Pid, Tick) ->
    gen_server:call(Pid, {start_run,Tick}).

stop_run(Pid) ->
    gen_server:call(Pid, stop_run).

get_status(Pid) ->
    gen_server:call(Pid, get_status).

%% Behaviour callbacks.

init(N) ->
    random:seed(now()),				%Seed the RNG
    Arr0 = array:new(),
    Arr1 = lists:foldl(fun (I, Arr) ->
			       {ok,P} = start_phone(I),
			       array:set(I, P, Arr)
		       end, Arr0, lists:seq(1, N)),
    {ok,#st{n=N,calls=0,arr=Arr1,to=infinity}}.

start_phone(I) ->
    Str = integer_to_list(I),
    phone_fsm:start_link(Str),
    {ok,P} = phone_sim:start_link(Str),
    %%sys:trace(P, true),
    {ok,P}.

terminate(_, _) -> ok.

handle_call({start_run,Tick}, _, St) ->
    {reply,ok,St#st{calls=0,to=Tick},Tick};
handle_call(get_status, _, St) ->
    {reply,St#st.calls,St,St#st.to};		%Must include the tick
handle_call(stop_run, _, St) ->
    {reply,ok,St#st{to=infinity},infinity};
handle_call(stop, _, St) ->
    %% Do everything in terminate.
    {stop,normal,ok,St}.

handle_info(timeout, #st{n=N,calls=Cs,arr=Arr,to=To}=St) ->
    A = random:uniform(N),			%A-side
    B = random:uniform(N),			%B-side
    %%io:format("A: ~p calling B: ~p\n", [A,B]),
    APid = array:get(A, Arr),
    phone_sim:action(APid, {call,integer_to_list(B)}),
    {noreply,St#st{calls=Cs+1},To}.

%% Unused callbacks.
handle_cast(_, St) -> {noreply,St}.

code_change(_, St, _) -> {ok,St}.
