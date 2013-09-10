%%% @doc Session counter process

-module(session_counter_gen).
-author('mkorszun@gmail.com').

-behaviour(gen_server).

-export([start_link/2, add/3, update/3, active_sessions/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ###############################################################
%% INCLUDES
%% ###############################################################

-include_lib("gizmo_backend_utils/include/logger.hrl").
-include_lib("gizmo_backend_utils/include/types.hrl").

%% ###############################################################
%% STATE
%% ###############################################################

-record(state, {key, freq, device_state_counter = dict:new()}).

%% ###############################################################
%% API
%% ###############################################################

-spec start_link(atom(), binary()) -> {ok, pid()} | {error, term()}.
start_link(Name, Key) ->
    gen_server:start_link({local, Name}, ?MODULE, [Key], []).

%% @doc Updates session counter for given application
-spec add(binary(), pid(), binary()) -> ok.
add(Key, SessionProcess, SessionState) ->
    {ok, running} = session_counter_sup:start_child(?B2A(Key), Key),
    gen_server:cast(?B2A(Key), {add, SessionProcess, SessionState}).

%% @doc Updates session counter in case of state transition
-spec update(binary(), binary(), binary()) -> ok.
update(Key, OldSessionState, NewSessionState) ->
    gen_server:cast(?B2A(Key), {state_change, OldSessionState, NewSessionState}).

%% @doc Returns list of current active sessions
-spec active_sessions(binary(), term()) -> ok.
active_sessions(Key, SessionState) ->
    try gen_server:call(?B2A(Key), {get_counter, SessionState}) of
        Res -> {ok, Res}
    catch
        exit:{noproc, _} -> {ok, 0};
        exit:{normal, _} -> {ok, 0}
    end.

%% ###############################################################
%% GEN_SERVER CALLBACKS
%% ###############################################################

init([Key]) ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Freq} = application:get_env(App, update_frequency),
    timer:send_after(Freq, self(), update_stats),
    process_flag(trap_exit, true),
    {ok, #state{key = Key, freq = Freq}}.

%% Get device session state counters
handle_call({get_counter, DeviceState}, _From, #state{device_state_counter = DeviceStates} = State) ->
    {reply, get_device_state_counter(DeviceState, DeviceStates), State}.

%% Device session process started - update state counters
handle_cast({add, DeviceProcess, DeviceState}, #state{device_state_counter = DeviceStateCounter} = State) ->
    erlang:monitor(process, DeviceProcess),
    {noreply, State#state{device_state_counter = add_state(DeviceState, DeviceStateCounter)}};

%% Device session process changed state - update state counters
handle_cast({state_change, S1, S2}, #state{device_state_counter = DeviceStateCounter} = State) ->
    {noreply, State#state{device_state_counter = update_state(S1, S2, DeviceStateCounter)}}.

%% Device session process is down - update state counters
handle_info({'DOWN', Ref, _, _,{shutdown, {state, S}}}, #state{device_state_counter = DeviceStateCounter} = State) ->
    erlang:demonitor(Ref),
    case get_device_state_counter(all, DeviceStateCounter) of
        1 ->
            {stop, normal, State};

        _ ->
            {noreply, State#state{device_state_counter = rem_state(S, DeviceStateCounter)}}

    end;

%% Stats update result
handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _, Reason}, #state{key = Key} = State) ->
    ?ERR("Failed to save application ~p session count: ~p", [Key, Reason]),
    {noreply, State};

%% Update states periodically
handle_info(update_stats, #state{key = Key, freq = Freq, device_state_counter = DeviceStateCounter} = State) ->
    timer:send_after(Freq, self(), update_stats),
    Timestamp = ?I2B(timestamp(erlang:now())),
    spawn_link(fun() -> session_counter_stats:save(Key, Timestamp, dict:to_list(DeviceStateCounter)) end),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

update_state(OldState, NewState, States) ->
    update_counter(NewState, 1, update_counter(OldState, -1, States)).

add_state(State, States) ->
    update_counter(State, 1, update_counter(all, 1, States)).

rem_state(State, States) ->
    update_counter(State, -1, update_counter(all, -1, States)).

update_counter(NewState, Value, States) ->
    dict:update_counter(NewState, Value, States).

timestamp({Mega, Sec, Micro}) ->
    Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.

get_device_state_counter(all_states, DeviceStateCounter) ->
    dict:to_list(DeviceStateCounter);
get_device_state_counter(DeviceState, DeviceStateCounter) ->
    try dict:fetch(DeviceState, DeviceStateCounter) of
        C -> C
    catch
        _:_ -> 0
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################