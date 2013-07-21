-module(session_heartbeat_fsm_SUITE).
-author('mkorszun@gmail.com').

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%% ###############################################################
%% CT CALLBACKS
%% ###############################################################

all() -> [
            test_heartbeat_ok,
            test_heartbeat_new_state,
            test_heartbeat_new_timeout,
            test_heartbeat_new_state_timeout,
            test_heartbeat_timeout
         ].

init_per_suite(Config) ->
    ok = application:start(session_heartbeat),
    ok = application:start(meck),
    Config.

end_per_suite(_) ->
    ok = application:stop(meck),
    ok = application:stop(session_heartbeat).

init_per_testcase(_, _) ->
    [{device, list_to_atom(binary_to_list(base64:encode(crypto:strong_rand_bytes(10))))}].

%% ###############################################################
%% TESTS
%% ###############################################################

test_heartbeat_ok([{device, Device}]) ->
    meck:new(session_counter_gen),
    meck:expect(session_counter_gen, add, fun(_,_,_) -> ok end),
    ok = session_heartbeat_fsm:heartbeat(<<"key">>, Device, undefined, undefined),
    [{id, Device},
     {key, <<"key">>},
     {timeout, 5000},
     {start_time, _},
     {device_state, undefined},
     {reason, undefined}
    ] = session_heartbeat_fsm:device_info(Device),
    [{_, {session_counter_gen, add, [<<"key">>, _, undefined]}, ok}] = meck:history(session_counter_gen),
    true = meck:validate(session_counter_gen),
    meck:unload(session_counter_gen).

test_heartbeat_new_state([{device, Device}]) ->
    meck:new(session_counter_gen),
    meck:expect(session_counter_gen, add, fun(_,_,_) -> ok end),
    meck:expect(session_counter_gen, update, fun(_,_,_) -> ok end),
    ok = session_heartbeat_fsm:heartbeat(<<"key">>, Device, undefined, <<"one">>),
    [{id, Device},
     {key, <<"key">>},
     {timeout, 5000},
     {start_time, _},
     {device_state, <<"one">>},
     {reason, undefined}
    ] = session_heartbeat_fsm:device_info(Device),
    ok = session_heartbeat_fsm:heartbeat(<<"key">>, Device, undefined, <<"two">>),
    [{id, Device},
     {key, <<"key">>},
     {timeout, 5000},
     {start_time, _},
     {device_state, <<"two">>},
     {reason, undefined}
    ] = session_heartbeat_fsm:device_info(Device),
    [{_, {session_counter_gen, add, [<<"key">>, _, <<"one">>]}, ok},
     {_, {session_counter_gen, update, [<<"key">>, <<"one">>, <<"two">>]}, ok}] = meck:history(session_counter_gen),
    true = meck:validate(session_counter_gen),
    meck:unload(session_counter_gen).

test_heartbeat_new_timeout([{device, Device}]) ->
    meck:new(session_counter_gen),
    meck:expect(session_counter_gen, add, fun(_,_,_) -> ok end),
    ok = session_heartbeat_fsm:heartbeat(<<"key">>, Device, 10000, <<"one">>),
    [{id, Device},
     {key, <<"key">>},
     {timeout, 10000},
     {start_time, _},
     {device_state, <<"one">>},
     {reason, undefined}
    ] = session_heartbeat_fsm:device_info(Device),
    ok = session_heartbeat_fsm:heartbeat(<<"key">>, Device, 1000, <<"one">>),
    [{id, Device},
     {key, <<"key">>},
     {timeout, 1000},
     {start_time, _},
     {device_state, <<"one">>},
     {reason, undefined}
    ] = session_heartbeat_fsm:device_info(Device),
    [{_, {session_counter_gen, add, [<<"key">>, _, <<"one">>]}, ok}] = meck:history(session_counter_gen),
    true = meck:validate(session_counter_gen),
    meck:unload(session_counter_gen).

test_heartbeat_new_state_timeout([{device, Device}]) ->
    meck:new(session_counter_gen),
    meck:expect(session_counter_gen, add, fun(_,_,_) -> ok end),
    meck:expect(session_counter_gen, update, fun(_,_,_) -> ok end),
    ok = session_heartbeat_fsm:heartbeat(<<"key">>, Device, 10000, <<"one">>),
    [{id, Device},
     {key, <<"key">>},
     {timeout, 10000},
     {start_time, _},
     {device_state, <<"one">>},
     {reason, undefined}
    ] = session_heartbeat_fsm:device_info(Device),
    ok = session_heartbeat_fsm:heartbeat(<<"key">>, Device, 1000, <<"two">>),
    [{id, Device},
     {key, <<"key">>},
     {timeout, 1000},
     {start_time, _},
     {device_state, <<"two">>},
     {reason, undefined}
    ] = session_heartbeat_fsm:device_info(Device),
    [{_, {session_counter_gen, add, [<<"key">>, _, <<"one">>]}, ok},
     {_, {session_counter_gen, update, [<<"key">>, <<"one">>, <<"two">>]}, ok}] = meck:history(session_counter_gen),
    true = meck:validate(session_counter_gen),
    meck:unload(session_counter_gen).

test_heartbeat_timeout([{device, Device}]) ->
    meck:new(session_counter_gen),
    meck:expect(session_counter_gen, add, fun(_,_,_) -> ok end),
    meck:expect(session_counter_gen, update, fun(_,_,_) -> ok end),
    ok = session_heartbeat_fsm:heartbeat(<<"key">>, Device, 100, <<"one">>),
    ok = timer:sleep(200), undefined = whereis(Device),
    [{_, {session_counter_gen, add, [<<"key">>, _, <<"one">>]}, ok}] = meck:history(session_counter_gen),
    true = meck:validate(session_counter_gen),
    meck:unload(session_counter_gen).

%% ###############################################################
%% ###############################################################
%% ###############################################################