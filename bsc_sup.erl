%%%---------------------------------------------------------------------
%%% @author Johan Stråle <jstrale@kth.se>, Helena Lindén <pkhli@kth.se>
%%%--------------------------------------------------------------------- 
%%% Description module bsc_sup.erl
%%%--------------------------------------------------------------------- 
%%% This module represents a supervisor handling a hlr and a 
%%% phone controller supervisor.
%%%--------------------------------------------------------------------- 
-module(bsc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_controller/1, remove_controller/1]).

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
	
%%--------------------------------------------------------------------
%% Function:   add_controller/2
%% Purpose:    Adds a phone controller attached to PhoneNumber
%%			   to the supervisor with SupPid
%% Params:     SupPid, PhoneNumber
%% Returns:    {ok, Pid}
%%--------------------------------------------------------------------
add_controller(PhoneNumber) ->
	[_, {_, FsmSupPid, _, _}] = supervisor:which_children(bsc_sup),
	phone_fsm_sup:add_controller(FsmSupPid, PhoneNumber).

%%--------------------------------------------------------------------
%% Function:   remove_controller/2
%% Purpose:    Removes and terminates the phone controller attached
%%			   to PhoneNumber from the supervisor with SupPid
%% Params:     SupPid, PhoneNumber
%% Returns:    ok
%%--------------------------------------------------------------------
remove_controller(PhoneNumber) ->
	[_, {_, FsmSupPid, _, _}] = supervisor:which_children(bsc_sup),
	phone_fsm_sup:remove_controller(FsmSupPid, PhoneNumber).

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
    HlrType = worker,
	FsmSupType = supervisor,

    HlrChild = {hlr, {hlr, start_link, []},
	      Restart, Shutdown, HlrType, [phone_fsm, hlr, phone]},

	PhoneFsmSupChild = {phone_fsm_sup, {phone_fsm_sup, start_link, []},
	      Restart, Shutdown, FsmSupType, [phone_fsm_sup, phone_fsm]},
	
    {ok, {SupFlags, [PhoneFsmSupChild, HlrChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
