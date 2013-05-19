-module(http_api_application).
-author('mkorszun@gmail.com').

-export([init/1, allowed_methods/2, content_types_provided/2]).
-export([process_post/2]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include_lib("utils/include/logger.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%% ###############################################################
%% CONTROL
%% ###############################################################

init([]) ->
    {ok, []}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

%% ###############################################################
%% RESOURCE
%% ###############################################################

%% ###############################################################
%% READ
%% ###############################################################

process_post(ReqData, State) ->
    case application_obj:create([]) of
        {ok, Key} ->
            Body = mochijson2:encode({struct, [{key, Key}]}),
            {true, wrq:set_resp_body(Body, ReqData), State};
        {error, Reason} ->
            ?ERR("Application key generation failed: ~p", [Reason]),
            http_api_error:error(internal_error, 500, ReqData, State)
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################