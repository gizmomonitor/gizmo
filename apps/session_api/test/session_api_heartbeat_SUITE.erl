-module(session_api_heartbeat_SUITE).
-author('mkorszun@gmail.com').

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%% ###############################################################
%% CT CALLBACKS
%% ###############################################################

all() -> [test_heartbeat_ok, test_heartbeat_app_not_found].

init_per_suite(Config) ->
    ok = application:start(ranch),
    ok = application:start(crypto),
    ok = application:start(cowboy),
    ok = application:start(inets),
    application:set_env(session_api, http_server_addr, {127,0,0,1}),
    application:set_env(session_api, http_server_port, 8888),
    ok = application:start(session_api),
    ok = application:start(meck),
    Config.

end_per_suite(_) ->
    ok = application:stop(meck),
    ok = application:stop(session_api),
    ok = application:stop(inets),
    ok = application:stop(cowboy),
    ok = application:stop(crypto),
    ok = application:stop(ranch).

%% ###############################################################
%% TESTS
%% ###############################################################

test_heartbeat_ok(_) ->
    meck:new(application_manager),
    meck:expect(application_manager, exists, fun(_) -> true end),
    meck:new(session_heartbeat_fsm),
    meck:expect(session_heartbeat_fsm, heartbeat, fun(_,_,_,_) -> ok end),
    {ok, {{"HTTP/1.1", 204, "No Content"}, _Header, _Body}} = httpc:request("http://127.0.0.1:8888/application/key/device/1234/heartbeat?timeout=1000"),
    [{_, {application_manager, exists, [<<"key">>]}, true}] = meck:history(application_manager),
    [{_, {session_heartbeat_fsm, heartbeat, [<<"key">>, device1234, 1000, undefined]}, ok}] = meck:history(session_heartbeat_fsm),
    true = meck:validate(application_manager),
    true = meck:validate(session_heartbeat_fsm),
    meck:unload(application_manager),
    meck:unload(session_heartbeat_fsm).

test_heartbeat_app_not_found(_) ->
    meck:new(application_manager),
    meck:expect(application_manager, exists, fun(_) -> false end),
    {ok, {{"HTTP/1.1", 404, "Not Found"}, Header, Body}} = httpc:request("http://127.0.0.1:8888/application/key/device/1234/heartbeat?timeout=1000"),
    "application/json" = proplists:get_value("content-type", Header),
    {struct, [{<<"error">>, <<"application_not_found">>}]} = mochijson2:decode(Body),
    [{_, {application_manager, exists, [<<"key">>]}, false}] = meck:history(application_manager),
    true = meck:validate(application_manager),
    meck:unload(application_manager).

%% ###############################################################
%% ###############################################################
%% ###############################################################