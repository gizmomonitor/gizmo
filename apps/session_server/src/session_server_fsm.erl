-module(session_server_fsm).
-author('mkorszun@gmail.com').

-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, 'STARTED'/2, 'ACTIVE'/2, handle_event/3, handle_info/3, handle_sync_event/4, terminate/3, code_change/4]).

%% ###############################################################
%% MACROS
%% ###############################################################

-include_lib("utils/include/logger.hrl").

%% ###############################################################
%% STATE
%% ###############################################################

-record(state, {id, key, timeout, start_time, reason}).

%% ###############################################################
%% API
%% ###############################################################

start_link(ApplicationKey, DeviceId) ->
    gen_fsm:start_link({local, DeviceId}, ?MODULE, [ApplicationKey, DeviceId], []).

%% ###############################################################
%% GEN_FSM CALLBACKS
%% ###############################################################

init([ApplicationKey, DeviceId]) ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Timeout} = application:get_env(App, timeout),
    session_counter_api:add(ApplicationKey, self()),
    {ok, 'STARTED', #state{id = DeviceId, key = ApplicationKey,
        start_time = erlang:now(), timeout = Timeout}, Timeout}.

'STARTED'(heartbeat, #state{timeout=Timeout} = State) ->
    {next_state, 'ACTIVE', State, Timeout};
'STARTED'(timeout, State) ->
    {stop, normal, State#state{reason = timeout}};
'STARTED'(stop, State) ->
    {stop, normal, State#state{reason = normal}}.

'ACTIVE'(heartbeat, #state{timeout=Timeout} = State) ->
    {next_state, 'ACTIVE', State, Timeout};
'ACTIVE'(timeout, State) ->
    {stop, normal, State#state{reason = timeout}};
'ACTIVE'(stop, State) ->
    {stop, normal, State#state{reason = normal}}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_, StateName, #state{id = Id, key = Key, start_time = Start, reason = Reason}) ->
    SessionTime = timer:now_diff(erlang:now(), Start) div 1000000,
    ?INF("Session ~s/~s terminated. Duration: ~p State: ~p Reason: ~p",
        [Key, Id, SessionTime, StateName, Reason]).

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.

%% ###############################################################
%% ###############################################################
%% ###############################################################