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
-export([init/1, idle/2, calling/2, receiving/2, connected/2, handle_event/3,
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
action(FsmPid, Action) -> gen_fsm:send_event(FsmPid, {action, Action, self()}).

%%%===================================================================
%%% Internal events
%%%===================================================================

busy(FsmPid) -> gen_fsm:send_event(FsmPid, {event, busy, self()}).
reject(FsmPid) -> gen_fsm:send_event(FsmPid, {event, reject, self()}).
accept(FsmPid) -> gen_fsm:send_event(FsmPid, {event, accept, self()}).
hangup(FsmPid) -> gen_fsm:send_event(FsmPid, {event, hangup, self()}).
inbound(FsmPid) -> gen_fsm:send_event(FsmPid, {event, inbound, self()}).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Function:   init/1
%% Purpose:    init fsm state
%% Params:     PhoneNumber to attach in hlr
%% Returns:    {ok, State, LoopData}
%%--------------------------------------------------------------------
init(PhoneNumber) ->
	hlr:attach(PhoneNumber),
    {ok, idle, null}.

%%--------------------------------------------------------------------
%% Function:   idle/2
%% Purpose:    Phone fsm is in idle state
%%			   The events that are processed: inbound
%%			   The action that are processed: stop, connect, disconnect, outbound
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
idle({action, {outbound, PhoneNumber}, _FromPid}, ConnectedDevices) ->
	case hlr:lookup_id(PhoneNumber) of
		{ok, ToPid} -> inbound(ToPid),
			       {next_state, calling, ConnectedDevices#data{fsmPid=ToPid}};
		{error, invalid} -> phone:reply(ConnectedDevices#data.phonePid, invalid),
				    {next_state, idle, ConnectedDevices}
	end;
idle({event, inbound, FromPid}, ConnectedDevices) ->
	case hlr:lookup_phone(FromPid) of
	    {ok, FromNumber} -> phone:reply(ConnectedDevices#data.phonePid, {inbound, FromNumber}),
				{next_state, receiving, ConnectedDevices#data{fsmPid=FromPid}};
		{error, invalid} -> {next_state, idle, ConnectedDevices}
	end;
idle(_, ConnectedDevices) ->
	{next_state, idle, ConnectedDevices}.

%%--------------------------------------------------------------------
%% Function:   calling/2
%% Purpose:    Phone fsm is in calling state
%%			   The events that are processed: accept, reject, busy
%%			   The action that are processed: hangup
%% Params:     Event to handle, ConnectedDevices holds pid of phone 
%%             connected to fsm and the currently connected phone fsm
%% Returns:    {ok, Reply, LoopData}
%%--------------------------------------------------------------------
calling({action, hangup, _FromPid}, ConnectedDevices) ->
    hangup(ConnectedDevices#data.fsmPid),
    {next_state, idle, ConnectedDevices#data{fsmPid=null}};
calling({event, accept, FromPid}, ConnectedDevices) when FromPid == ConnectedDevices#data.fsmPid ->
    phone:reply(ConnectedDevices#data.phonePid, accept),
    {next_state, connected, ConnectedDevices};
calling({event, reject, FromPid}, ConnectedDevices) when FromPid == ConnectedDevices#data.fsmPid ->
    phone:reply(ConnectedDevices#data.phonePid, reject),
    {next_state, idle, ConnectedDevices#data{fsmPid=null}};
calling({event, busy, FromPid}, ConnectedDevices) when FromPid == ConnectedDevices#data.fsmPid ->
    phone:reply(ConnectedDevices#data.phonePid, busy),
    {next_state, idle, ConnectedDevices#data{fsmPid=null}};
calling({_,_,FromPid}, ConnectedDevices) when FromPid == ConnectedDevices#data.phonePid ->
    {next_state, calling, ConnectedDevices};
calling({_,_,FromPid}, ConnectedDevices) ->
    busy(FromPid),
    {next_state, calling, ConnectedDevices}.
						 

%%--------------------------------------------------------------------
%% Function:   receiving/2
%% Purpose:    Phone fsm is in receiving state
%%			   The events that are processed: hangup
%%			   The action that are processed: accept, reject
%% Params:     Event to handle, ConnectedDevices holds pid of phone 
%%             connected to fsm and the currently connected phone fsm
%% Returns:    {ok, Reply, LoopData}
%%--------------------------------------------------------------------
receiving({event, hangup, FromPid}, ConnectedDevices) when FromPid == ConnectedDevices#data.fsmPid ->
    phone:reply(ConnectedDevices#data.phonePid, hangup),
    {next_state, idle, ConnectedDevices#data{fsmPid=null}};
receiving({action, accept, _FromPid}, ConnectedDevices) ->
    accept(ConnectedDevices#data.fsmPid),
    {next_state, connected, ConnectedDevices};  
receiving({action, reject, _FromPid}, ConnectedDevices) ->
    reject(ConnectedDevices#data.fsmPid),
    {next_state, idle, ConnectedDevices#data{fsmPid=null}};
receiving({_,_,FromPid}, ConnectedDevices) when FromPid == ConnectedDevices#data.phonePid ->
    {next_state, receiving, ConnectedDevices}; 
receiving({_,_,FromPid}, ConnectedDevices) ->
    busy(FromPid),
    {next_state, receiving, ConnectedDevices}.

%%--------------------------------------------------------------------
%% Function:   connected/2
%% Purpose:    Phone fsm is in connected state
%%			   The events that are processed: hangup
%%			   The action that are processed: hangup
%% Params:     Event to handle, ConnectedDevices holds pid of phone 
%%             connected to fsm and the currently connected phone fsm
%% Returns:    {ok, Reply, LoopData}
%%--------------------------------------------------------------------
connected({action, hangup, _FromPid}, ConnectedDevices) ->
    hangup(ConnectedDevices#data.fsmPid),
    {next_state, idle, ConnectedDevices#data{fsmPid=null}};
connected({event, hangup, FromPid}, ConnectedDevices) when FromPid == ConnectedDevices#data.fsmPid ->
    phone:reply(ConnectedDevices#data.phonePid, hangup),
    {next_state, idle, ConnectedDevices#data{fsmPid=null}};
connected({_,_,FromPid}, ConnectedDevices) when FromPid == ConnectedDevices#data.fsmPid ->
    {next_state, connected, ConnectedDevices};
connected({_,_,FromPid}, ConnectedDevices) ->
    busy(FromPid),
    {next_state, connected, ConnectedDevices}.

%%--------------------------------------------------------------------
%% Function:   terminate/3
%% Purpose:    detaches phone_fsm from hlr
%% Params:     ignore
%% Returns:    ok
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _LoopData) ->
	hlr:detach().

%%%===================================================================
%%% Stubbed functions. Do nothing except preventing error being thrown
%%%===================================================================
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.
