%%% @doc Heartbeat process supervisor

-module(session_heartbeat_sup).
-author('mkorszun@gmail.com').

-behaviour(supervisor).

-export([init/1, start_link/0, start_child/4]).

%% ###############################################################
%% MACROS
%% ###############################################################

-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ###############################################################
%% API
%% ###############################################################

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Key, Device, Timeout, DeviceState) ->
    case supervisor:start_child(?MODULE, [Key, Device, Timeout, DeviceState]) of
        {ok, _Pid} -> {ok, running};
        {error, {already_started, _}} -> {ok, running};
        {error, Error} -> {error, Error}
    end.

%% ###############################################################
%% SUPERVISOR CALLBACKS
%% ###############################################################

init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(session_heartbeat_fsm, worker)]}}.

%% ###############################################################
%% ###############################################################
%% ###############################################################