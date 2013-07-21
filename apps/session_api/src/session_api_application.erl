%%% @doc Application resource

-module(session_api_application).
-author('mkorszun@gmail.com').

-export([init/3, allowed_methods/2, content_types_accepted/2]).
-export([resource_info/0]).
-export([from_json/2 ]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include_lib("gizmo_backend_utils/include/logger.hrl").

%% ###############################################################
%% CONTROL
%% ###############################################################

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

resource_info() ->
    {"/application", [], ?MODULE, []}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

from_json(ReqData, State) ->
    case application_manager:create([]) of
        {ok, Key} ->
            Body = mochijson2:encode({struct, [{key, Key}]}),
            {true, session_api_utils:to_json(Body, ReqData), State};
        {error, Reason} ->
            ?ERR("Application key generation failed: ~p", [Reason]),
            {halt, session_api_utils:to_error(internal_error, 500, ReqData), State}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################