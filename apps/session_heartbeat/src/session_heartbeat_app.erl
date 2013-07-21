%%% @doc Session heartbeat application

-module(session_heartbeat_app).
-author('mkorszun@gmail.com').

-behaviour(application).

-export([start/2, stop/1]).

%% ###############################################################
%% APPLICATION CALLBACKS
%% ###############################################################

start(_StartType, _StartArgs) ->
    session_heartbeat_sup:start_link().

stop(_State) ->
    ok.

%% ###############################################################
%% ###############################################################
%% ###############################################################