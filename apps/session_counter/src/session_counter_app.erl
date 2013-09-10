%%% @doc Session counter application

-module(session_counter_app).
-author('mkorszun@gmail.com').

-behaviour(application).

-export([start/2, stop/1]).

%% ###############################################################
%% APPLICATION CALLBACKS
%% ###############################################################

start(_StartType, _StartArgs) ->
    session_counter_sup:start_link().

stop(_State) ->
    ok.

%% ###############################################################
%% ###############################################################
%% ###############################################################