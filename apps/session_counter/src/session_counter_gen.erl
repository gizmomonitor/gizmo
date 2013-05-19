-module(session_counter_gen).
-author('mkorszun@gmail.com').

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ###############################################################
%% MACROS
%% ###############################################################

-include_lib("utils/include/logger.hrl").
-include_lib("utils/include/types.hrl").

%% ###############################################################
%% STATE
%% ###############################################################

-record(state, {counter = 0, key, freq}).

%% ###############################################################
%% API
%% ###############################################################

start_link(Key) ->
    gen_server:start_link({local, ?L2A(Key)}, ?MODULE, [Key], []).

%% ###############################################################
%% GEN_SERVER CALLBACKS
%% ###############################################################

init([Key]) ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Freq} = application:get_env(App, update_frequency),
    timer:send_after(Freq, self(), update_stats),
    process_flag(trap_exit, true),
    {ok, #state{key = Key, freq = Freq}}.

handle_call(get_counter, _From, #state{counter = Cnt} = State) ->
    {reply, Cnt, State}.

handle_cast({add, SessionProcess}, #state{counter = Cnt} = State) ->
    erlang:monitor(process, SessionProcess),
    {noreply, State#state{counter = Cnt + 1}};
handle_cast(remove, #state{counter = Cnt} = State) ->
    {noreply, State#state{counter = Cnt - 1}}.

%% Session monitoring
handle_info({'DOWN', _, _, _, Reason}, #state{counter = 1} = State) ->
    {stop, normal, State};
handle_info({'DOWN', _, _, _, Reason}, #state{counter = Cnt} = State) ->
    {noreply, State#state{counter = Cnt - 1}};

%% Stats update result
handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _, Reason}, #state{key = Key} = State) ->
    ?ERR("Failed to save application ~p session count: ~p", [Key, Reason]),
    {noreply, State};

handle_info(update_stats, #state{counter = Cnt, key = Key, freq = Freq} = State) ->
    timer:send_after(Freq, self(), update_stats),
    Timestamp = ?I2B(timestamp(erlang:now())),
    spawn_link(fun() -> session_counter_stats:save(?L2B(Key), Timestamp, Cnt) end),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _State, _Extra) ->
    ok.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

timestamp({Mega, Sec, Micro}) ->
    Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.

%% ###############################################################
%% ###############################################################
%% ###############################################################