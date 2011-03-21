%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc Yet another logging library for Erlang
%%% @end
%%%-------------------------------------------------------------------
-module(elog).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(application).

%% @headerfile "elog.hrl"
-include("elog.hrl").

%% API
-export([start/0, level/1, level/2, get_env/1, get_env/2]).
%% APPLICATION
-export([start/2, stop/1]).

-type loglevel() :: ?LOG_LEVEL_DEBUG | ?LOG_LEVEL_INFO | ?LOG_LEVEL_STAT | ?LOG_LEVEL_WARN | ?LOG_LEVEL_ERROR | ?LOG_LEVEL_FATAL.
-export_type([loglevel/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% @doc  Returns global config values
-spec get_env(level | logger) -> term().
get_env(Key) ->
  case application:get_env(elog, Key) of
    undefined -> get_env_default(Key);
    {ok, Value} -> Value
  end.

%%% @doc  Returns a configuration parameter
-spec get_env(loglevel(), logger) -> term().
get_env(Level, Key) ->
  case application:get_env(elog, Level) of
    undefined -> get_env(Key);
    {ok, Values} -> proplists:get_value(Key, Values, get_env(Key))
  end.

%%% @doc  Starts the application
-spec start() -> ok.
start() ->
  application:start(elog).

%%% @doc  Sets the global log level
-spec level(loglevel()) -> ok.
level(Level) ->
  application:set_env(elog, log_level, Level),
  elog_sup:reload().

%%% @doc  Sets the log level for a particular module or regular expression
-spec level(loglevel(), atom() | string()) -> ok.
level(Level, ModuleOrRegExp) ->
  _ = lists:foldl(
        fun(L, reached) ->
                try elogger:add(L, ModuleOrRegExp)
                catch
                  _:{noproc, _} ->
                    {ok, _Pid} = elog_sup:add_exception(L),
                    elogger:add(L, ModuleOrRegExp)
                end,
                reached;
           (L, not_reached) ->
                case L of
                  Level ->
                    try elogger:add(L, ModuleOrRegExp)
                    catch
                      _:{noproc, _} ->
                        {ok, _Pid} = elog_sup:add_exception(L),
                        elogger:add(L, ModuleOrRegExp)
                    end,
                    reached;
                  _ ->
                    catch elogger:remove(L, ModuleOrRegExp),
                    not_reached
                end
        end, not_reached, ?LOG_LEVELS),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% APPLICATION FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
  elog_sup:start_link().

%% @hidden
-spec stop([]) -> ok.
stop([]) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_env_default(level) -> ?LOG_LEVEL_INFO;
get_env_default(logger) -> {elogger_console, []}.