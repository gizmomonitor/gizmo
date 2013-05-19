-module(http_api).
-author('mkorszun@gmail.com').

-export([start/0, start_link/0, stop/0]).

%% ###############################################################
%% API
%% ###############################################################

start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
    ensure_started(webmachine),
    http_api_sup:start_link().

start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
    ensure_started(webmachine),
    application:start(http_api).

stop() ->
    Res = application:stop(http_api),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################