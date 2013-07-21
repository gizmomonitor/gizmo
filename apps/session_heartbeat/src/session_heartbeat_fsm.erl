%%% @doc Provides basic heartbeat functionality with state transitions

-module(session_heartbeat_fsm).
-author('mkorszun@gmail.com').

-behaviour(gen_fsm).

-export([start_link/4, heartbeat/4, device_info/1]).
-export([init/1, 'ACTIVE'/2, handle_sync_event/4, terminate/3, code_change/4]).

%% ###############################################################
%% MACROS
%% ###############################################################

-include_lib("gizmo_backend_utils/include/logger.hrl").

%% ###############################################################
%% STATE / TYPES
%% ###############################################################

-record(state, {id, key, timeout, start_time, device_state, reason}).

-type dtimeout() :: integer() | undefined.
-type dstate() :: binary() | undefined.

%% ###############################################################
%% API
%% ###############################################################

%% @doc Starts given device process
-spec start_link(binary(), atom(), dtimeout(), dstate()) -> {ok, pid()} | {error, term()}.
start_link(Key, Device, Timeout, DeviceState) ->
    gen_fsm:start_link({local, Device}, ?MODULE, [Key, Device, Timeout, DeviceState], []).

%% @doc Sends heartbeat to given device process
-spec heartbeat(binary(), atom(), dtimeout(), dstate()) -> ok.
heartbeat(Key, Device, Timeout, DeviceState) ->
    {ok, running} = session_heartbeat_sup:start_child(Key, Device, Timeout, DeviceState),
    gen_fsm:send_event(Device, {heartbeat, Timeout, DeviceState}).

%% @doc Returns device info
-spec device_info(atom()) -> list({atom(), term()}).
device_info(DeviceId) ->
    gen_fsm:sync_send_all_state_event(DeviceId, device_info).

%% ###############################################################
%% GEN_FSM CALLBACKS
%% ###############################################################

init([ApplicationKey, DeviceId, Timeout, DeviceState]) ->
    Timeout2 = if is_integer(Timeout) -> Timeout; true -> 5000 end,
    session_counter_gen:add(ApplicationKey, self(), DeviceState),
    State = #state{id = DeviceId,
                   key = ApplicationKey,
                   start_time = erlang:now(),
                   timeout = Timeout2,
                   device_state = DeviceState},
    {ok, 'ACTIVE', State, Timeout2}.

%% Device info - mostly for tests
handle_sync_event(device_info, _, StateName, StateData) ->
    {reply, lists:zip(record_info(fields, state), tl(tuple_to_list(StateData))), StateName, StateData}.

%% Device state not changed
'ACTIVE'({heartbeat, T, S}, #state{device_state = S} = State) when is_integer(T) ->
    {next_state, 'ACTIVE', State#state{timeout = T}, T};
'ACTIVE'({heartbeat, _, S}, #state{device_state = S, timeout = T} = State) ->
    {next_state, 'ACTIVE', State, T};

%% Device state changed / update state count
'ACTIVE'({heartbeat, T, S1}, #state{key = Key, device_state = S2} = State) when is_integer(T) ->
    session_counter_gen:update(Key, S2, S1),
    {next_state, 'ACTIVE', State#state{device_state = S1, timeout = T}, T};
'ACTIVE'({heartbeat, _, S1}, #state{key = Key, device_state = S2, timeout = T} = State) ->
    session_counter_gen:update(Key, S2, S1),
    {next_state, 'ACTIVE', State#state{device_state = S1, timeout = T}, T};

%% Device session terminating - include state in shutdown reason
'ACTIVE'(timeout, #state{device_state = S} = State) ->
    {stop, {shutdown, {state, S}}, State#state{reason = timeout}}.

%% Update state count
terminate(_, StateName, #state{id = Id, key = Key, start_time = Start, reason = Reason}) ->
    SessionTime = timer:now_diff(erlang:now(), Start) div 1000000,
    ?INF("Session ~s/~s terminated. Duration: ~p State: ~p Reason: ~p", [Key, Id, SessionTime, StateName, Reason]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ###############################################################
%% ###############################################################
%% ###############################################################