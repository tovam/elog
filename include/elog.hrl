%%NOTE: We do this because we want to step over other definitions of these macros
-undef(LOG_LEVEL_DEBUG).
-undef(LOG_LEVEL_INFO).
-undef(LOG_LEVEL_STAT).
-undef(LOG_LEVEL_WARN).
-undef(LOG_LEVEL_ERROR).
-undef(LOG_LEVEL_FATAL).
-undef(LOG_LEVELS).
-undef(LOG).
-undef(DEBUG).
-undef(INFO).
-undef(WARN).
-undef(STAT).
-undef(ERROR).
-undef(FATAL).

-define(LOG_LEVEL_DEBUG, debug).
-define(LOG_LEVEL_INFO,  info).
-define(LOG_LEVEL_STAT,  stat).
-define(LOG_LEVEL_WARN,  warn).
-define(LOG_LEVEL_ERROR, error).
-define(LOG_LEVEL_FATAL, fatal).
-define(LOG_LEVELS, [?LOG_LEVEL_DEBUG, ?LOG_LEVEL_INFO, ?LOG_LEVEL_STAT,
                     ?LOG_LEVEL_WARN, ?LOG_LEVEL_ERROR, ?LOG_LEVEL_FATAL]).

-record(log, {time = erlang:localtime() :: {{2010..9999,1..12,1..31},{0..23,0..59,0..59}},
              level                     :: elog:loglevel(),
              module                    :: atom(),
              line                      :: integer(),
              pid                       :: pid(),
              node                      :: node(),
              stacktrace = []           :: [term()],
              text = ""                 :: string(),
              args = []                 :: [term()]}).

-ifdef(no_log).
-define(LOG(LOGProcess, LOGLevel, LOGStr, LOGArgs, LOGStack), ok = hd([ok, LOGProcess, LOGLevel, LOGStr, LOGStack | LOGArgs]).
-else.
-define(LOG(LOGProcess, LOGLevel, LOGStr, LOGArgs, LOGStack),
        try
          gen_server:cast(LOGProcess,
                          #log{module      = ?MODULE,
                               level       = LOGLevel,
                               line        = ?LINE,
                               pid         = self(),
                               node        = node(),
                               stacktrace  = LOGStack,
                               text        = LOGStr,
                               args        = LOGArgs})
        catch
            _ ->
                error_logger:error_msg("Exception trying to log a message:~p~n",
                                       [{LOGStr, LOGArgs}])
        end).
-endif.

-ifdef(no_debug).
-define(DEBUG(Str, Args), ok = hd([ok, Str | Args])).
-else.
-define(DEBUG(Str, Args), ?LOG('elogger-debug', ?LOG_LEVEL_DEBUG,  Str, Args, [])).
-endif.
-ifdef(no_info).
-define(INFO(Str, Args),  ok = hd([ok, Str | Args])).
-else.
-define(INFO(Str, Args),  ?LOG('elogger-info', ?LOG_LEVEL_INFO,  Str, Args, [])).
-endif.
-ifdef(no_stat).
-define(STAT(Str, Args),  ok = hd([ok, Str | Args])).
-else.
-define(STAT(Str, Args),  ?LOG('elogger-stat', ?LOG_LEVEL_STAT, Str, Args, [])).
-endif.
-ifdef(no_warn).
-define(WARN(Str, Args),  ok = hd([ok, Str | Args])).
-else.
-define(WARN(Str, Args),  ?LOG('elogger-warn', ?LOG_LEVEL_WARN, Str, Args, [])).
-endif.
-ifdef(no_error).
-define(ERROR(Str, Args), ok = hd([ok, Str | Args])).
-define(THROW(Str, Args), ok = hd([ok, Str | Args])).
-else.
-define(ERROR(Str, Args), ?LOG('elogger-error', ?LOG_LEVEL_ERROR, Str, Args, erlang:get_stacktrace())).
-define(THROW(Str, Args), try throw({}) catch _:_ -> ?ERROR(Str, Args) end).
-endif.
-ifdef(no_fatal).
-define(FATAL(Str, Args), ok = hd([ok, Str | Args])).
-else.
-define(FATAL(Str, Args), ?LOG('elogger-fatal', ?LOG_LEVEL_FATAL, Str, Args, erlang:get_stacktrace())).
-endif.