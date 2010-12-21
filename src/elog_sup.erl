%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2010 Fernando Benavides <greenmellon@gmail.com>
%%% @doc elog main supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(elog_sup).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(supervisor).

%% API
-export([start_link/0, reload/0]).

%% Supervisor callbacks
-export([init/1]).

%% @headerfile "elog.hrl"
-include("elog.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec reload() -> ok.
reload() ->
  ok = lists:foreach(fun({ChildId, _, _, _}) ->
                             ok = supervisor:terminate_child(?MODULE, ChildId)
                     end, supervisor:which_children(?MODULE)),
  lists:foreach(
    fun(LogLevel) ->
            io:format("elog Restarting ~p: ~p~n",
                      [LogLevel, supervisor:restart_child(?MODULE, LogLevel)])
    end, ?LOG_LEVELS).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  Children =
    [{LogLevel, {elogger, start_link, [LogLevel]},
      transient, brutal_kill, worker, [italkdb_db]} || LogLevel <- ?LOG_LEVELS],
  error_logger:info_msg("Inititalizing elog supervisor...~n"),
  {ok, {{one_for_one, 5, 10}, Children}}.