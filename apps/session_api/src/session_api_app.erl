%%% @doc HTTP API application

-module(session_api_app).
-author('mkorszun@gmail.com').

-behaviour(application).

-export([start/2, stop/1]).

%% ###############################################################
%% APPLICATION CALLBACKS
%% ###############################################################

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
                    session_api_heartbeat:resource_info(),
                    session_api_active_sessions:resource_info(),
                    session_api_application:resource_info()
              ]
        }
    ]),
    {ok, App} = application:get_application(?MODULE),
    {ok, Addr} = application:get_env(App, http_server_addr),
    {ok, Port} = application:get_env(App, http_server_port),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}, {ip, Addr}], [{env, [{dispatch, Dispatch}]}]),
    session_api_sup:start_link().

stop(_State) ->
    ok.

%% ###############################################################
%% ###############################################################
%% ###############################################################