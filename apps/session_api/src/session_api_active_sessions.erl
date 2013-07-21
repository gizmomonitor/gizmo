%%% @doc Active sessions resource

-module(session_api_active_sessions).
-author('mkorszun@gmail.com').

-export([init/3, content_types_provided/2]).
-export([resource_info/0]).
-export([resource_exists/2, active_sessions_to_json/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include_lib("gizmo_backend_utils/include/logger.hrl").
-include_lib("gizmo_backend_utils/include/types.hrl").

%% ###############################################################
%% CONTROL
%% ###############################################################

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, active_sessions_to_json}], Req, State}.

resource_info() ->
    {"/application/:application/active_sessions", [], ?MODULE, []}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

resource_exists(ReqData, State) ->
    session_api_utils:application_exists(ReqData, State).

active_sessions_to_json(ReqData, State) ->
    {Key, _} = cowboy_req:binding(application, ReqData),
    {SessionState, _} = cowboy_req:qs_val(<<"state">>, ReqData, undefined),
    {Start, _} = cowboy_req:qs_val(<<"start">>, ReqData, undefined),
    {End, _} = cowboy_req:qs_val(<<"end">>, ReqData, undefined),
    case session_counter_api:active_sessions(Key, SessionState, ?B2I(Start), ?B2I(End)) of
        {ok, Res} ->
            {mochijson2:encode({struct, [{active_sessions, Res}]}), ReqData, State};
        {error, Reason} ->
            ?ERR("Read active sessions for ~s: ~p", [Key, Reason]),
            {halt, session_api_utils:to_error(internal_error, 500, ReqData), State}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################