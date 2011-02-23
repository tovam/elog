%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc Tester Helper module for elog
%%% @end
%%%-------------------------------------------------------------------
-module(elog_tester_helper).
-author('Fernando Benavides <greenmellon@gmail.com>').

-include("elog.hrl").

-export([log_all/0]).

-spec log(loglevel(), string(), [term()]) -> ok.
log(?LOG_LEVEL_DEBUG, Format, Args) -> ?DEBUG(Format, Args);
log(?LOG_LEVEL_ERROR, Format, Args) -> ?ERROR(Format, Args);
log(?LOG_LEVEL_FATAL, Format, Args) -> ?FATAL(Format, Args);
log(?LOG_LEVEL_INFO, Format, Args) -> ?INFO(Format, Args);
log(?LOG_LEVEL_STAT, Format, Args) -> ?STAT(Format, Args);
log(?LOG_LEVEL_WARN, Format, Args) -> ?WARN(Format, Args).

-spec log_all() -> ok.
log_all() ->
  lists:foreach(
    fun(L) ->
            timer:sleep(100),
            log(L, "~p at ~p~n", [L, element(2, calendar:local_time())])
    end, ?LOG_LEVELS),
  timer:sleep(100),
  ?THROW("~p at ~p~n", [?LOG_LEVEL_ERROR, element(2, calendar:local_time())]).