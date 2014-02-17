%%%==============================================================
%%% @Copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @Author Robert Virding <robert.virding@erlang-solutions.com>
%%% @doc Phone process for the Oxford CPR assignment.
%%% @end
%%%==============================================================

%% This phone module is very simple and basically just sends commands
%% on to the phone controller FSM and display replies from it. We
%% don't need a state machine and use a gen_server behaviour for this
%% phone. It is asynchronous so the 2 user API functions are
%% gen_server:casts. The local state contains our phone number and the
%% pid of the controller process.

-module(phone).

-behaviour(gen_server).

%% User API.
-export([start_link/1,start/1,stop/1]).
-export([action/2,reply/2]).

%% Behaviour callbacks.
-export([init/1,terminate/2,handle_call/3,handle_cast/2,
	 handle_info/2,code_change/3]).

-record(st, {num=none,contr=none}).

%% Management API.
start_link(Number) ->
    gen_server:start_link(?MODULE, Number, []).

start(Number) ->
    gen_server:start(?MODULE, Number, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%% User API.
action(Pid, Action) ->
    gen_server:cast(Pid, {action,Action}).

reply(Pid, Reply) ->
    gen_server:cast(Pid, {reply,Reply}).

%% Behaviour callbacks.

%% init(Args) -> {ok,State} | {stop,Reason}.
%%  Initialise the phone. Use the HLR to find the phone controller and
%%  connect to it. If unavailable then return stop to terminate phone.

init(Number) ->
    case hlr:lookup_id(Number) of
	{ok,Pid} ->
	    ok = phone_fsm:connect(Pid),
	    {ok,#st{num=Number,contr=Pid}};
	{error,invalid} -> {stop,invalid}
    end.

%% terminate(Reason, State) -> ok.
%%  Terminate the phone. Just disconnect the phone controller.

terminate(_, #st{contr=Fsm}) ->
    phone_fsm:disconnect(Fsm),
    ok.

handle_call(stop, _, St) ->
    %% Do everything in terminate.
    {stop,normal,ok,St}.

handle_cast({action,Action}, #st{contr=Fsm}=St) ->
    %% This gives us a check of Action.
    case Action of
	{call,Number} -> phone_fsm:action(Fsm, {outbound,Number});
	accept -> phone_fsm:action(Fsm, accept);
	reject -> phone_fsm:action(Fsm, reject);
	hangup -> phone_fsm:action(Fsm, hangup)
    end,
    {noreply,St};
handle_cast({reply,Reply}, #st{num=Num}=St) ->
    %% This gives us a check of Reply.
    {F,As} = case Reply of
		 {inbound,Number} -> {"inbound call from ~p",[Number]};
		 accept -> {"call accepted",[]};
		 reject -> {"call rejected",[]};
		 hangup -> {"remote hangup",[]};
		 busy -> {"busy",[]};
		 invalid -> {"invalid call",[]}
	     end,
    io:format("~p: ~p: " ++ F ++ "\n", [self(),Num|As]),
    {noreply,St}.

%% Unused callbacks.
handle_info(_, St) -> {noreply,St}.

code_change(_, St, _) -> {ok,St}.
