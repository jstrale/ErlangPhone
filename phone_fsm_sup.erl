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
-export([start_link/0, stop/1, add_controller/2, remove_controller/2]).

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
    supervisor:start_link(?MODULE, []).

%%--------------------------------------------------------------------
%% Function:   stop/1
%% Purpose:    Terminates all phone controllers started by supervisor
%%			   with SupPid and stops the supervisor.
%% Params:     SupPid
%% Returns:    ok
%%--------------------------------------------------------------------
stop(SupPid) -> 
	exit(SupPid, shutdown).
	
%%--------------------------------------------------------------------
%% Function:   add_controller/2
%% Purpose:    Adds a phone controller attached to PhoneNumber
%%			   to the supervisor with SupPid
%% Params:     SupPid, PhoneNumber
%% Returns:    {ok, Pid}
%%--------------------------------------------------------------------
add_controller(SupPid, PhoneNumber) ->
	Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {PhoneNumber, {phone_fsm, start_link, [PhoneNumber]},
	      Restart, Shutdown, Type, [phone_fsm, hlr, phone]},

	ok = supervisor:check_childspecs([AChild]),

	supervisor:start_child(SupPid, AChild).

%%--------------------------------------------------------------------
%% Function:   remove_controller/2
%% Purpose:    Removes and terminates the phone controller attached
%%			   to PhoneNumber from the supervisor with SupPid
%% Params:     SupPid, PhoneNumber
%% Returns:    ok
%%--------------------------------------------------------------------
remove_controller(SupPid, PhoneNumber) ->
	supervisor:terminate_child(SupPid, PhoneNumber),
	supervisor:delete_child(SupPid, PhoneNumber).	

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

    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
