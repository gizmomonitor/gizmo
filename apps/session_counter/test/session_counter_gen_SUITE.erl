-module(session_counter_gen_SUITE).
-author('mkorszun@gmail.com').

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%% ###############################################################
%% CT CALLBACKS
%% ###############################################################

all() -> [
    test_add,
    test_add_custom_state,
    test_active_sessions_one_session_one_dies,
    test_active_sessions_two_sessions_one_dies,
    test_active_sessions_no_sessions,
    test_active_sessions_all_states,
    test_update
].

init_per_suite(Config) ->
    ok = application:start(session_counter),
    ok = application:start(meck),
    Config.

end_per_suite(_) ->
    ok = application:stop(meck),
    ok = application:stop(session_counter).

init_per_testcase(_, _) ->
    [{key, base64:encode(crypto:strong_rand_bytes(10))}].

%% ###############################################################
%% TESTS
%% ###############################################################

test_add([{key, Key}]) ->
    meck:new(session_counter_stats),
    ok = session_counter_gen:add(Key, self(), undefined),
    {ok, 1} = session_counter_gen:active_sessions(Key, undefined),
    {ok, 1} = session_counter_gen:active_sessions(Key, all),
    ok = meck:unload(session_counter_stats).

test_add_custom_state([{key, Key}]) ->
    meck:new(session_counter_stats),
    ok = session_counter_gen:add(Key, self(), some_state),
    {ok, 1} = session_counter_gen:active_sessions(Key, some_state),
    {ok, 1} = session_counter_gen:active_sessions(Key, all),
    ok = meck:unload(session_counter_stats).

test_active_sessions_one_session_one_dies([{key, Key}]) ->
    meck:new(session_counter_stats),
    Pid = start_session(some_state1),
    ok = session_counter_gen:add(Key, Pid, some_state1),
    {ok, 1} = session_counter_gen:active_sessions(Key, some_state1),
    {ok, 1} = session_counter_gen:active_sessions(Key, all),
    ok = stop_session(Pid),
    {ok, 0} = session_counter_gen:active_sessions(Key, some_state1),
    {ok, 0} = session_counter_gen:active_sessions(Key, all),
    ok = meck:unload(session_counter_stats).

test_active_sessions_two_sessions_one_dies([{key, Key}]) ->
    meck:new(session_counter_stats),
    Pid1 = start_session(some_state1),
    Pid2 = start_session(some_state2),
    ok = session_counter_gen:add(Key, Pid1, some_state1),
    ok = session_counter_gen:add(Key, Pid2, some_state2),
    {ok, 1} = session_counter_gen:active_sessions(Key, some_state1),
    {ok, 1} = session_counter_gen:active_sessions(Key, some_state2),
    {ok, 2} = session_counter_gen:active_sessions(Key, all),
    ok = stop_session(Pid1),
    {ok, 0} = session_counter_gen:active_sessions(Key, some_state1),
    {ok, 1} = session_counter_gen:active_sessions(Key, some_state2),
    {ok, 1} = session_counter_gen:active_sessions(Key, all),
    ok = meck:unload(session_counter_stats).

test_active_sessions_all_states([{key, Key}]) ->
    meck:new(session_counter_stats),
    Pid = start_session(some_state1),
    ok = session_counter_gen:add(Key, Pid, some_state1),
    {ok, States} = session_counter_gen:active_sessions(Key, all_states),
    1 = proplists:get_value(some_state1, States),
    1 = proplists:get_value(all, States),
    ok = meck:unload(session_counter_stats).

test_active_sessions_no_sessions([{key, Key}]) ->
    {ok, 0} = session_counter_gen:active_sessions(Key, undefined),
    {ok, 0} = session_counter_gen:active_sessions(Key, some_state1),
    {ok, 0} = session_counter_gen:active_sessions(Key, all).

test_update([{key, Key}]) ->
    meck:new(session_counter_stats),
    Pid = start_session(some_state1),
    ok = session_counter_gen:add(Key, Pid, some_state1),
    {ok, 1} = session_counter_gen:active_sessions(Key, some_state1),
    {ok, 0} = session_counter_gen:active_sessions(Key, some_state2),
    {ok, 1} = session_counter_gen:active_sessions(Key, all),
    ok = session_counter_gen:update(Key, some_state1, some_state2),
    {ok, 0} = session_counter_gen:active_sessions(Key, some_state1),
    {ok, 1} = session_counter_gen:active_sessions(Key, some_state2),
    {ok, 1} = session_counter_gen:active_sessions(Key, all),
    ok = meck:unload(session_counter_stats).

%% ###############################################################
%% TEST HELPERS
%% ###############################################################

start_session(State) ->
    process_flag(trap_exit, true),
    spawn_link(fun() -> receive stop -> erlang:exit({shutdown, {state, State}}) end end).

stop_session(Counter) ->
    Counter ! stop,
    receive
        {'EXIT', _, _} ->
             ok;
        _ ->
             error
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################