%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc Tester module for elog
%%% @end
%%%-------------------------------------------------------------------
-module(elog_tester).
-author('Fernando Benavides <greenmellon@gmail.com>').

-include("elog.hrl").

-export([log/3, start/4, stop/1, start/0, main/0]).

-define(SLEEP, 1000).

-spec main() -> no_return().
main() ->
  start(),
  timer:sleep(?SLEEP),
  halt(0).

-spec start() -> ok.
start() ->
  ok = elog:debug(),
  io:format("~n------------------------------------------------~n"
              "You should see 2 comments per each level..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:error(),
  io:format("~n------------------------------------------------~n"
              "You should see 2 comments per each level >= error..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:fatal(),
  io:format("~n------------------------------------------------~n"
              "You should only see fatal comments..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:mod_debug(?MODULE),
  io:format("~n------------------------------------------------~n"
              "You should see 1 comments per each level < fatal..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:cat_debug(fake),
  io:format("~n------------------------------------------------~n"
              "You should see 1 comments per each level < fatal..."
                "~n------------------------------------------------~n"),
  ok = log_all(),

  timer:sleep(?SLEEP),
  ok = elog:mod_fatal(?MODULE),
  ok = elog:cat_fatal(fake),
  io:format("~n------------------------------------------------~n"
              "You should only see fatal comments..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:mod_info(?MODULE),
  ok = elog:mod_info(elog_tester_helper),
  io:format("~n------------------------------------------------~n"
              "You should see 2 comments per each level >= info..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:mod_fatal(?MODULE),
  ok = elog:mod_fatal(elog_tester_helper),
  ok = elog:cat_info(fake),
  ok = elog:cat_info(default),
  io:format("~n------------------------------------------------~n"
              "You should see 2 comments per each level >= info..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:warn(),
  io:format("~n------------------------------------------------~n"
              "You should see 2 comments per each level >= warn..."
                "~n------------------------------------------------~n"),
  ok = log_all(),
  
  timer:sleep(?SLEEP),
  ok = elog:fatal(),
  ok = elog:re_debug("debug"),
  io:format("~n------------------------------------------------~n"
              "You should see 2 fatal and 2 debug comments..."
                "~n------------------------------------------------~n"),
  ok = log_all().

-spec start(pos_integer(), elog:loglevel(), string(), [term()]) -> {ok, timer:tref()}.
start(Time, Level, Format, Args) ->
  timer:apply_interval(Time, ?MODULE, log, [Level, Format, Args]).

-spec stop(timer:tref()) -> {ok, cancel}.
stop(Timer) ->
  timer:cancel(Timer).

-spec log(elog:loglevel(), string(), [term()]) -> ok.
log(?LOG_LEVEL_DEBUG, Format, Args) -> ?CDEBUG(fake, Format, Args);
log(?LOG_LEVEL_ERROR, Format, Args) -> ?CERROR(fake, Format, Args);
log(?LOG_LEVEL_FATAL, Format, Args) -> ?CFATAL(fake, Format, Args);
log(?LOG_LEVEL_INFO, Format, Args) -> ?CINFO(fake, Format, Args);
log(?LOG_LEVEL_STAT, Format, Args) -> ?CSTAT(fake, Format, Args);
log(?LOG_LEVEL_WARN, Format, Args) -> ?CWARN(fake, Format, Args).

log_all() ->
  lists:foreach(
    fun(L) ->
            timer:sleep(100),
            log(L, "~p at ~p~n", [L, element(2, calendar:local_time())])
    end, ?LOG_LEVELS),
  timer:sleep(erlang:trunc(?SLEEP / 10)),
  ?THROW("~p at ~p~n", [?LOG_LEVEL_ERROR, element(2, calendar:local_time())]),
  elog_tester_helper:log_all().