%%%---------------------------------------------------------------------
%%% @author Johan Stråle <jstrale@kth.se>, Helena Lindén <pkhli@kth.se>
%%%--------------------------------------------------------------------- 
%%% Description module hlr.erl
%%%--------------------------------------------------------------------- 
%%% 
%%%---------------------------------------------------------------------
-module(test).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% Function:   start_link/0
%% Purpose:    start fsm
%% Params:     ---
%% Returns:    {ok, Pid}
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% Client functions
%%%===================================================================






%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Function:   init/1
%% Purpose:    init ets table
%% Params:     ignore
%% Returns:    {reply, Reply, LoopData}
%%--------------------------------------------------------------------
init([]) ->
    {ok, state_name, #state{}}.

%%--------------------------------------------------------------------
%% Function:   init/1
%% Purpose:    init ets table
%% Params:     ignore
%% Returns:    {reply, Reply, LoopData}
%%--------------------------------------------------------------------
state_name(_Event, State) ->
    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% Function:   init/1
%% Purpose:    init ets table
%% Params:     ignore
%% Returns:    {reply, Reply, LoopData}
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function:   init/1
%% Purpose:    init ets table
%% Params:     ignore
%% Returns:    {reply, Reply, LoopData}
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function:   init/1
%% Purpose:    init ets table
%% Params:     ignore
%% Returns:    {reply, Reply, LoopData}
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.


%%--------------------------------------------------------------------
%% Function:   init/1
%% Purpose:    init ets table
%% Params:     ignore
%% Returns:    {reply, Reply, LoopData}
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%%===================================================================
%%% Stubbed functions. Do nothing except preventing error being thrown
%%%===================================================================
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
