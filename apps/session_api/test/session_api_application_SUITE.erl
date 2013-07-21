-module(session_api_application_SUITE).
-author('mkorszun@gmail.com').

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%% ###############################################################
%% CT CALLBACKS
%% ###############################################################

all() -> [test_create_ok, test_create_error].

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

test_create_ok(_) ->
    meck:new(application_manager),
    meck:expect(application_manager, create, fun(_) -> {ok, <<"key">>} end),
    {ok, {{"HTTP/1.1", 200, "OK"}, Header, Body}} = httpc:request(post, {"http://127.0.0.1:8888/application", [], "application/json", []}, [], []),
    "application/json" = proplists:get_value("content-type", Header),
    {struct, [{<<"key">>, <<"key">>}]} = mochijson2:decode(Body),
    [{_, {application_manager, create,[[]]}, {ok, <<"key">>}}] = meck:history(application_manager),
    true = meck:validate(application_manager),
    meck:unload(application_manager).

test_create_error(_) ->
    meck:new(application_manager),
    meck:expect(application_manager, create, fun(_) -> {error, reason} end),
    {ok, {{"HTTP/1.1", 500, "Internal Server Error"}, Header, Body}} = httpc:request(post, {"http://127.0.0.1:8888/application", [], "application/json", []}, [], []),
    "application/json" = proplists:get_value("content-type", Header),
    {struct, [{<<"error">>, <<"internal_error">>}]} = mochijson2:decode(Body),
    [{_, {application_manager, create,[[]]}, {error, reason}}] = meck:history(application_manager),
    true = meck:validate(application_manager),
    meck:unload(application_manager).

%% ###############################################################
%% ###############################################################
%% ###############################################################