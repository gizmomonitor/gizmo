-module(session_api_active_sessions_SUITE).
-author('mkorszun@gmail.com').

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%% ###############################################################
%% CT CALLBACKS
%% ###############################################################

all() -> [test_active_sessions, test_active_sessions_history, test_active_sessions_application_not_found, test_active_sessions_error].

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

test_active_sessions(_) ->
    meck:new(application_manager),
    meck:expect(application_manager, exists, fun(_) -> true end),
    meck:new(session_counter_api),
    meck:expect(session_counter_api, active_sessions, fun(_,_,_,_) -> {ok, 5} end),
    {ok, {{"HTTP/1.1", 200, "OK"}, Header, Body}} = httpc:request("http://127.0.0.1:8888/application/key/active_sessions"),
    "application/json" = proplists:get_value("content-type", Header),
    {struct, [{<<"active_sessions">>, 5}]} = mochijson2:decode(Body),
    [{_, {application_manager, exists, [<<"key">>]}, true}] = meck:history(application_manager),
    [{_, {session_counter_api, active_sessions, [<<"key">>, undefined, undefined, undefined]}, {ok, 5}}] = meck:history(session_counter_api),
    true = meck:validate(application_manager),
    true = meck:validate(session_counter_api),
    meck:unload(application_manager),
    meck:unload(session_counter_api).

test_active_sessions_history(_) ->
    meck:new(application_manager),
    meck:expect(application_manager, exists, fun(_) -> true end),
    meck:new(session_counter_api),
    meck:expect(session_counter_api, active_sessions, fun(_,_,_,_) -> {ok, [{struct, [{<<"123">>, 1}]}, {struct, [{<<"456">>, 2}]}]} end),
    {ok, {{"HTTP/1.1", 200, "OK"}, Header, Body}} = httpc:request("http://127.0.0.1:8888/application/key/active_sessions"),
    "application/json" = proplists:get_value("content-type", Header),
    {struct, [{<<"active_sessions">>, [{struct, [{<<"123">>, 1}]}, {struct, [{<<"456">>, 2}]}]}]} = mochijson2:decode(Body),
    [{_, {application_manager, exists, [<<"key">>]}, true}] = meck:history(application_manager),
    [{_, {session_counter_api, active_sessions, [<<"key">>, undefined, undefined, undefined]}, {ok, [{struct, [{<<"123">>, 1}]}, {struct, [{<<"456">>, 2}]}]}}] = meck:history(session_counter_api),
    true = meck:validate(application_manager),
    true = meck:validate(session_counter_api),
    meck:unload(application_manager),
    meck:unload(session_counter_api).

test_active_sessions_application_not_found(_) ->
    meck:new(application_manager),
    meck:expect(application_manager, exists, fun(_) -> false end),
    {ok, {{"HTTP/1.1", 404, "Not Found"}, Header, Body}} = httpc:request("http://127.0.0.1:8888/application/key/active_sessions"),
    "application/json" = proplists:get_value("content-type", Header),
    {struct, [{<<"error">>, <<"application_not_found">>}]} = mochijson2:decode(Body),
    [{_, {application_manager, exists, [<<"key">>]}, false}] = meck:history(application_manager),
    true = meck:validate(application_manager),
    meck:unload(application_manager).

test_active_sessions_error(_) ->
    meck:new(application_manager),
    meck:expect(application_manager, exists, fun(_) -> true end),
    meck:new(session_counter_api),
    meck:expect(session_counter_api, active_sessions, fun(_,_,_,_) -> {error, reason} end),
    {ok, {{"HTTP/1.1", 500, "Internal Server Error"}, Header, Body}} = httpc:request("http://127.0.0.1:8888/application/key/active_sessions"),
    "application/json" = proplists:get_value("content-type", Header),
    {struct, [{<<"error">>, <<"internal_error">>}]} = mochijson2:decode(Body),
    [{_, {application_manager, exists, [<<"key">>]}, true}] = meck:history(application_manager),
    [{_, {session_counter_api, active_sessions, [<<"key">>, undefined, undefined, undefined]}, {error, reason}}] = meck:history(session_counter_api),
    true = meck:validate(application_manager),
    true = meck:validate(session_counter_api),
    meck:unload(application_manager),
    meck:unload(session_counter_api).

%% ###############################################################
%% ###############################################################
%% ###############################################################