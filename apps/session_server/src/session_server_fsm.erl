-module(session_server_fsm).
-author('mkorszun@gmail.com').

-behaviour(gen_fsm).

-export([start_link/3]).
-export([init/1, 'ACTIVE'/2, handle_event/3, handle_info/3,
    handle_sync_event/4, terminate/3, code_change/4]).

%% ###############################################################
%% MACROS
%% ###############################################################

-include_lib("gizmo_backend_utils/include/logger.hrl").

%% ###############################################################
%% STATE
%% ###############################################################

-record(state, {id, key, timeout, start_time, reason}).

%% ###############################################################
%% API
%% ###############################################################

start_link(ApplicationKey, DeviceId, Timeout) ->
    gen_fsm:start_link({local, DeviceId}, ?MODULE,
        [ApplicationKey, DeviceId, Timeout], []).

%% ###############################################################
%% GEN_FSM CALLBACKS
%% ###############################################################

init([ApplicationKey, DeviceId, undefined]) ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Timeout} = application:get_env(App, timeout),
    init([ApplicationKey, DeviceId, Timeout]);

init([ApplicationKey, DeviceId, Timeout]) when is_integer(Timeout) ->
    session_counter_api:add(ApplicationKey, self()),
    {ok, 'ACTIVE', #state{id = DeviceId, key = ApplicationKey,
        start_time = erlang:now(), timeout = Timeout}, Timeout}.

'ACTIVE'({heartbeat, Timeout}, State) when is_integer(Timeout) ->
    {next_state, 'ACTIVE', State#state{timeout = Timeout}, Timeout};
'ACTIVE'({heartbeat, _}, #state{timeout = Timeout} = State) ->
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