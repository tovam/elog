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
-export([start/0, level/1, level/3, mod_level/2, cat_level/2, re_level/2, get_env/1, get_env/2]).
-export([debug/0, info/0, stat/0, warn/0, error/0, fatal/0]).
-export([mod_debug/1, mod_info/1, mod_stat/1, mod_warn/1, mod_error/1, mod_fatal/1]).
-export([cat_debug/1, cat_info/1, cat_stat/1, cat_warn/1, cat_error/1, cat_fatal/1]).
-export([re_debug/1, re_info/1, re_stat/1, re_warn/1, re_error/1, re_fatal/1]).
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

%%% @equiv level(mod, Level, Value)
-spec mod_level(loglevel(), atom()) -> ok.
mod_level(Level, Value) -> level(mod, Level, Value).

%%% @equiv level(cat, Level, Value)
-spec cat_level(loglevel(), atom()) -> ok.
cat_level(Level, Value) -> level(cat, Level, Value).

%%% @equiv level(re, Level, Value)
-spec re_level(loglevel(), string()) -> ok.
re_level(Level, Value) -> level(re, Level, Value).

%% @equiv level(debug)
-spec debug() -> ok.
debug() -> level(?LOG_LEVEL_DEBUG).

%%% @equiv level(mod, debug, Value)
-spec mod_debug(atom()) -> ok.
mod_debug(Value) -> level(mod, ?LOG_LEVEL_DEBUG, Value).
%%% @equiv level(cat, debug, Value)
-spec cat_debug(atom()) -> ok.
cat_debug(Value) -> level(cat, ?LOG_LEVEL_DEBUG, Value).
%%% @equiv level(re, debug, Value)
-spec re_debug(string()) -> ok.
re_debug(Value) -> level(re, ?LOG_LEVEL_DEBUG, Value).

%% @equiv level(info)
-spec info() -> ok.
info() -> level(?LOG_LEVEL_INFO).

%%% @equiv level(mod, info, Value)
-spec mod_info(atom()) -> ok.
mod_info(Value) -> level(mod, ?LOG_LEVEL_INFO, Value).
%%% @equiv level(cat, info, Value)
-spec cat_info(atom()) -> ok.
cat_info(Value) -> level(cat, ?LOG_LEVEL_INFO, Value).
%%% @equiv level(re, info, Value)
-spec re_info(string()) -> ok.
re_info(Value) -> level(re, ?LOG_LEVEL_INFO, Value).

%% @equiv level(stat)
-spec stat() -> ok.
stat() -> level(?LOG_LEVEL_STAT).

%%% @equiv level(mod, stat, Value)
-spec mod_stat(atom()) -> ok.
mod_stat(Value) -> level(mod, ?LOG_LEVEL_STAT, Value).
%%% @equiv level(cat, stat, Value)
-spec cat_stat(atom()) -> ok.
cat_stat(Value) -> level(cat, ?LOG_LEVEL_STAT, Value).
%%% @equiv level(re, stat, Value)
-spec re_stat(string()) -> ok.
re_stat(Value) -> level(re, ?LOG_LEVEL_STAT, Value).

%% @equiv level(warn)
-spec warn() -> ok.
warn() -> level(?LOG_LEVEL_WARN).

%%% @equiv level(mod, warn, Value)
-spec mod_warn(atom()) -> ok.
mod_warn(Value) -> level(mod, ?LOG_LEVEL_WARN, Value).
%%% @equiv level(cat, warn, Value)
-spec cat_warn(atom()) -> ok.
cat_warn(Value) -> level(cat, ?LOG_LEVEL_WARN, Value).
%%% @equiv level(re, warn, Value)
-spec re_warn(string()) -> ok.
re_warn(Value) -> level(re, ?LOG_LEVEL_WARN, Value).

%% @equiv level(error)
-spec error() -> ok.
error() -> level(?LOG_LEVEL_ERROR).

%%% @equiv level(mod, error, Value)
-spec mod_error(atom()) -> ok.
mod_error(Value) -> level(mod, ?LOG_LEVEL_ERROR, Value).
%%% @equiv level(cat, error, Value)
-spec cat_error(atom()) -> ok.
cat_error(Value) -> level(cat, ?LOG_LEVEL_ERROR, Value).
%%% @equiv level(re, error, Value)
-spec re_error(string()) -> ok.
re_error(Value) -> level(re, ?LOG_LEVEL_ERROR, Value).

%% @equiv level(fatal)
-spec fatal() -> ok.
fatal() -> level(?LOG_LEVEL_FATAL).

%%% @equiv level(mod, fatal, Value)
-spec mod_fatal(atom()) -> ok.
mod_fatal(Value) -> level(mod, ?LOG_LEVEL_FATAL, Value).
%%% @equiv level(cat, fatal, Value)
-spec cat_fatal(atom()) -> ok.
cat_fatal(Value) -> level(cat, ?LOG_LEVEL_FATAL, Value).
%%% @equiv level(re, fatal, Value)
-spec re_fatal(string()) -> ok.
re_fatal(Value) -> level(re, ?LOG_LEVEL_FATAL, Value).

%%% @doc  Sets the log level for a particular module, category or regular expression
-spec level(elogger:exception_class(), loglevel(), atom() | string()) -> ok.
level(Class, Level, Value) ->
  _ = lists:foldl(
        fun(L, reached) ->
                try elogger:add(L, Class, Value)
                catch
                  _:{noproc, _} ->
                    {ok, _Pid} = elog_sup:add_exception(L),
                    elogger:add(L, Class, Value)
                end,
                reached;
           (L, not_reached) ->
                case L of
                  Level ->
                    try elogger:add(L, Class, Value)
                    catch
                      _:{noproc, _} ->
                        {ok, _Pid} = elog_sup:add_exception(L),
                        elogger:add(L, Class, Value)
                    end,
                    reached;
                  _ ->
                    catch elogger:remove(L, Class, Value),
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