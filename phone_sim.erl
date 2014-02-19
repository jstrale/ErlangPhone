%%%==============================================================
%%% @Copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @Author Robert Virding <robert.virding@erlang-solutions.com>
%%% @doc Phone simulator process for the Oxford CPR assignment.
%%% @end
%%%==============================================================

%% This is a simulator phone for testing the phone controller FSM. It
%% is a drop-in replacement to phone.erl. The 'phone_fsm' will use
%% calls to the 'phone' module but we will be at the other end so we
%% MUST have exactly the same interface. This is unfortunate as a we
%% need to be state aware so an FSM would have been better here.
%%
%% We implement an FSM by keeping a state field in the state
%% data. Instead of trying to process the different states inside the
%% handle_call/cast/info callbacks we explicitly call state callback
%% functions to do the work. This allows us to be a gen_server while
%% most of the code looks like an FSM. A much better solution.

-module(phone_sim).

-behaviour(gen_server).

%% User API.
-export([start_link/1,start/1,stop/1,get_status/1]).
-export([action/2,reply/2]).

%% Behaviour callbacks.
-export([init/1,terminate/2,handle_call/3,handle_cast/2,
	 handle_info/2,code_change/3]).

-record(st, {state=idle,num=none,fsm=none,to=infinity,stats={}}).

%% Time limits for timeouts
-define(MIN_TIME, 2000).
-define(MAX_TIME, 10000).

-define(CONN_RATE, 80).				%Connection accept percentage

%% Management API for each phone.
start_link(Number) ->
    gen_server:start_link(?MODULE, Number, []).

start(Number) ->
    gen_server:start(?MODULE, Number, []).

stop(Fsm) ->
    gen_server:call(Fsm, stop).

get_status(Fsm) ->
    gen_server:call(Fsm, get_status).

%% User API, these must be the same as for phone as phone_fsm uses
%% them!
action(Pid, Action) ->
    gen_server:cast(Pid, {action,Action}).

reply(Pid, Reply) ->
    gen_server:cast(Pid, {reply,Reply}).

%% Behaviour callbacks.

init(Number) ->
    random:seed(now()),				%Seed the RNG
    case hlr:lookup_id(Number) of
	{ok,Pid} ->
	    ok = phone_fsm:connect(Pid),
	    {ok,#st{num=Number,fsm=Pid}};
	{error,invalid} -> {stop,invalid}
    end.

terminate(_, #st{fsm=Fsm}) ->
    phone_fsm:disconnect(Fsm),
    ok.

handle_call(get_status, _, #st{stats=S}=St) ->
    {reply,S,St};
handle_call(stop, _, St) ->
    %% Do everything in terminate.
    {stop,normal,ok,St}.

handle_info(timeout, #st{state=State}=St) ->
    %% All timeouts go here, call state functions.
    %%io:format("~p: timedout in ~w\n", [St#st.num,State]),
    case State of
	idle -> idle(timeout, St);
	calling -> calling(timeout, St);
	connected -> connected(timeout, St)
    end;
handle_info(_, St) -> {noreply,St}.		%Ignore everything else

handle_cast(Cast, #st{state=State}=St) ->
    %% All "events" go here, call state functions.
    %%io:format("~p: action ~p in ~w\n", [St#st.num,Cast,State]),
    case State of
	idle -> idle(Cast, St);
	calling -> calling(Cast, St);
	connected -> connected(Cast, St)
    end.

%% State callback functions.

idle({action,{call,Number}}, #st{fsm=Fsm}=St) ->
    phone_fsm:action(Fsm, {outbound,Number}),
    next_state(calling, St);
idle({reply,{inbound,_}}, #st{fsm=Fsm}=St) ->
    Chance = random:uniform(100),
    if Chance < ?CONN_RATE ->			%Accept call
	    phone_fsm:action(Fsm, accept),
	    to_connected(St);
       true ->					%Reject call
	    phone_fsm:action(Fsm, reject),
	    to_idle(St)
    end;
idle(_, St) -> {noreply,St}.		       %Ignore everything else

calling({reply,accept}, St) ->
    to_connected(St);
calling({reply,reject}, St) ->
    to_idle(St);
calling({reply,busy}, St) ->
    to_idle(St);
calling(timeout, #st{fsm=Fsm}=St) ->
    phone_fsm:action(Fsm, hangup),
    to_idle(St);
calling(_, St) ->				%Ignore everything else
    {noreply,St,St#st.to}.

connected({reply,hangup}, St) ->
    next_state(idle, St);
connected(timeout, #st{fsm=Fsm}=St) ->		%We timed out, hangup
    phone_fsm:action(Fsm, hangup),
    next_state(idle, St);
connected(_, St) ->				%Ignore everything else
    {noreply,St,St#st.to}.

to_idle(St) -> next_state(idle, St).

to_connected(St) ->
    %% Generate a random timeout between ?MIN_TIME and ?MAX_TIME.
    To = random:uniform(?MAX_TIME-?MIN_TIME) + ?MIN_TIME,
    next_state(connected, To, St).

next_state(State, St) ->
    next_state(State, infinity, St).

next_state(State, To, St) ->
    %%io:format("~p: to state ~w\n", [St#st.num,State]),
    {noreply,St#st{to=To,state=State},To}.

%% Unused callbacks.
code_change(_, St, _) -> {ok,St}.
