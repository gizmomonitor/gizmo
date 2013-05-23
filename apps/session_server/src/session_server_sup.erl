-module(session_server_sup).
-author('mkorszun@gmail.com').

-behaviour(supervisor).

-export([init/1, start_link/0, start_child/3]).

%% ###############################################################
%% MACROS
%% ###############################################################

-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ###############################################################
%% API
%% ###############################################################

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ApplicationKey, DeviceId, Timeout) ->
    supervisor:start_child(?MODULE, [ApplicationKey, DeviceId, Timeout]).

%% ###############################################################
%% SUPERVISOR CALLBACKS
%% ###############################################################

init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(session_server_fsm, worker)]}}.

%% ###############################################################
%% ###############################################################
%% ###############################################################