%%%---------------------------------------------------------------------
%%% @author Johan Stråle <jstrale@kth.se>, Helena Lindén <pkhli@kth.se>
%%%--------------------------------------------------------------------- 
%%% Description module hlr.erl
%%%--------------------------------------------------------------------- 
%%% This module represents a home location register where phone
%%% phone controllers can attach and detach phonenumbers to their
%%% id (Pid). Phonenumbers and id´s can also be looked up.
%%%--------------------------------------------------------------------- 
-module(hlr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([attach/1, detach/0, lookup_id/1, lookup_phone/1]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(SERVER, ?MODULE).
-define(ETSTABLENAME, hlrDataStore).

-record(data, {phoneNumber, pid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% Function:   start_link/0
%% Purpose:    start server
%% Params:     ---
%% Returns:    {ok, Pid}
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% Client functions
%%%===================================================================

attach(PhoneNumber) ->
    gen_server:call(?SERVER, {attach, PhoneNumber, self()}).

detach() ->
    gen_server:call(?SERVER, {detach, self()}).

lookup_id(PhoneNumber) ->
    gen_server:call(?SERVER, {lookup_id, PhoneNumber}).

lookup_phone(Pid) ->
    gen_server:call(?SERVER, {lookup_phone, Pid}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Function:   init/1
%% Purpose:    init ets table
%% Params:     ignore
%% Returns:    {reply, Reply, LoopData}
%%--------------------------------------------------------------------
init([]) ->
    ets:new(?ETSTABLENAME, [named_table, {keypos, #data.phoneNumber}]),
    {ok, null}.

%%--------------------------------------------------------------------
%% Function:   handle_call/3
%% Purpose:    handle all calls made by the client
%% Params:     {message}, From, LoopData
%% Returns:    {reply, Reply, null}
%%--------------------------------------------------------------------
handle_call({attach, PhoneNumber, Pid}, _From, null) ->
    ets:match_delete(?ETSTABLENAME, #data{phoneNumber='_', pid=Pid}), % delete existing pid, if exists
    ets:insert(?ETSTABLENAME, #data{phoneNumber = PhoneNumber, pid = Pid}), % insert new pid - phone
    Reply = ok,
    {reply, Reply, null};
handle_call({detach, Pid}, _From, null) ->
    Reply = ok,
    ets:match_delete(?ETSTABLENAME, #data{phoneNumber='_', pid=Pid}),
    {reply, Reply, null};
handle_call({lookup_id, PhoneNumber}, _From, null) ->
   case ets:lookup(?ETSTABLENAME, PhoneNumber) of
       [] ->
	   Reply = {error, invalid};
       [Rec] ->
	   Reply = {ok, Rec#data.pid}
   end,
    {reply, Reply, null};
handle_call({lookup_phone, Pid}, _From, null) ->
    MS = ets:fun2ms(fun(#data{phoneNumber=PhoneNumber, pid=P}) when Pid==P -> PhoneNumber end),
    case ets:select(?ETSTABLENAME, MS) of
	[] ->
	    Reply = {error, invalid};
	[PhoneNumber | []] ->
	    Reply = {ok, PhoneNumber}
    end,
    {reply, Reply, null}.

%%%===================================================================
%%% Stubbed functions. Do nothing except preventing error being thrown
%%%===================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
terminate(_Reason, _State) ->
    ok.
