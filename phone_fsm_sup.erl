%%%---------------------------------------------------------------------
%%% @author Johan Stråle <jstrale@kth.se>, Helena Lindén <pkhli@kth.se>
%%%--------------------------------------------------------------------- 
%%% Description module phone_fsm_sup.erl
%%%--------------------------------------------------------------------- 
%%% This module represents a supervisor handling phone_fsm controllers
%%%--------------------------------------------------------------------- 
-module(phone_fsm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% Function:   start_link/0
%% Purpose:    start supervisor
%% Params:     ---
%% Returns:    {ok, Pid}
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Function:   init/1
%% Purpose:    init supervisor
%% Params:     ignore
%% Returns:    {ok, {supSpecs, [childSpecs]}}
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {'AName', {'AModule', start_link, []},
	      Restart, Shutdown, Type, ['AModule']},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
