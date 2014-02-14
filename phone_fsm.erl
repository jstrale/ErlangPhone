%%%---------------------------------------------------------------------
%%% @author Johan Stråle <jstrale@kth.se>, Helena Lindén <pkhli@kth.se>
%%%--------------------------------------------------------------------- 
%%% Description module phone_fsm.erl
%%%--------------------------------------------------------------------- 
%%% 
%%%---------------------------------------------------------------------
-module(phone_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, idle/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
%% client functions
-export([stop/1, connect/1, disconnect/1, action/2]).

-define(SERVER, ?MODULE).

-record(data, {phonePid, fsmPid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% Function:   start_link/1
%% Purpose:    start fsm
%% Params:     ---
%% Returns:    {ok, FsmPid}
%%--------------------------------------------------------------------
start_link(PhoneNumber) ->
    gen_fsm:start_link(?MODULE, PhoneNumber, []).

%%%===================================================================
%%% Client functions
%%%===================================================================

stop(FsmPid) -> gen_fsm:send_event(FsmPid, stop).
connect(FsmPid) -> gen_fsm:send_event(FsmPid, {connect, self()}).
disconnect(FsmPid) -> gen_fsm:send_event(FsmPid, {disconnect, self()}).
action(FsmPid, Action) -> gen_fsm:send_event(FsmPid, {action, Action}).

%%%===================================================================
%%% Internal events
%%%===================================================================

busy(FsmPid) -> gen_fsm:send_event(FsmPid, {event, busy}).
reject(FsmPid) -> gen_fsm:send_event(FsmPid, {event, reject}).
accept(FsmPid) -> gen_fsm:send_event(FsmPid, {event, accept}).
hangup(FsmPid) -> gen_fsm:send_event(FsmPid, {event, hangup}).
inbound(FsmPid) -> gen_fsm:send_event(FsmPid, {event, inbound, self()}).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Function:   init/1
%% Purpose:    init fsm state
%% Params:     ignore
%% Returns:    {ok, State, LoopData}
%%--------------------------------------------------------------------
init(PhoneNumber) ->
	hlr:attach(PhoneNumber),
    {ok, idle, null}.

%%--------------------------------------------------------------------
%% Function:   idle/2
%% Purpose:    Phone fsm is idle
%% Params:     Event to handle, LoopData is pid of phone connected to fsm
%% Returns:    {ok, Reply, LoopData}
%%--------------------------------------------------------------------
idle(stop, ConnectedDevices) ->
    {stop, normal, ConnectedDevices};
idle({connect, PhonePid}, null) ->
	{next_state, idle, #data{phonePid=PhonePid}};
idle(_, null) ->
	{next_state, idle, null};
idle({disconnect, _PhonePid}, _ConnectedDevices) ->
	{next_state, idle, null};
idle({action, {outbound, PhoneNumber}}, ConnectedDevices) ->
	case hlr:lookup_id(PhoneNumber) of
		{ok, ToPid} -> inbound(ToPid),
					   {next_state, calling, ConnectedDevices};
		{error, invalid} -> phone:reply(ConnectedDevices#data.phonePid, invalid),
							{next_state, idle, ConnectedPhone}
	end;
idle({event, inbound, FromPid}, ConnectedDevices) ->
	case hlr:lookup_phone(FromPid) of
		{ok, FromNumber} -> phone:reply(ConnectedDevices#data.phonePid, FromNumber),
							{next_state, receiving, ConnectedDevices#data{fsmPid=FromPid}};
		{error, invalid} -> {next_state, idle, ConnectedPhone}
	end;
idle(_, ConnectedDevices) ->
	{next_state, idle, ConnectedDevices}.

	

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
%% Function:   terminate/3
%% Purpose:    detaches phone_fsm from hlr
%% Params:     ignore
%% Returns:    ok
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, LoopData) ->
	io:format("ConnectedPhone: ~w~n",[LoopData]),
	hlr:detach().

%%%===================================================================
%%% Stubbed functions. Do nothing except preventing error being thrown
%%%===================================================================
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
