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
              category = default        :: atom(),
              module                    :: atom(),
              line                      :: integer(),
              pid                       :: pid(),
              node                      :: node(),
              stacktrace = []           :: [term()],
              text = ""                 :: string(),
              args = []                 :: [term()]}).

-ifdef(no_log).
-define(LOG(LOGProcess, LOGLevel, LOGCat, LOGStr, LOGArgs, LOGStack), ok = hd([ok, LOGProcess, LOGLevel, LOGStr, LOGStack | LOGArgs]).
-else.
-define(LOG(LOGProcess, LOGLevel, LOGCat, LOGStr, LOGArgs, LOGStack),
        try
          gen_server:cast(LOGProcess,
                          #log{module      = ?MODULE,
                               category    = LOGCat,
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

-define(NO_LOG(Cat, Str, Args), ok = hd([ok, Cat, Str | Args])).

-ifdef(no_debug).
-define(CDEBUG(Cat, Str, Args), ?NO_LOG(Cat, Str, Args)).
-else.
-define(CDEBUG(Cat, Str, Args), ?LOG('elogger-debug', ?LOG_LEVEL_DEBUG, Cat, Str, Args, [])).
-endif.
-define(DEBUG(Str, Args), ?CDEBUG(default, Str, Args)).

-ifdef(no_info).
-define(CINFO(Cat, Str, Args), ?NO_LOG(Cat, Str, Args)).
-else.
-define(CINFO(Cat, Str, Args), ?LOG('elogger-info', ?LOG_LEVEL_INFO, Cat, Str, Args, [])).
-endif.
-define(INFO(Str, Args), ?CINFO(default, Str, Args)).

-ifdef(no_stat).
-define(CSTAT(Cat, Str, Args), ?NO_LOG(Cat, Str, Args)).
-else.
-define(CSTAT(Cat, Str, Args), ?LOG('elogger-stat', ?LOG_LEVEL_STAT, Cat, Str, Args, [])).
-endif.
-define(STAT(Str, Args), ?CSTAT(default, Str, Args)).

-ifdef(no_warn).
-define(CWARN(Cat, Str, Args), ?NO_LOG(Cat, Str, Args)).
-else.
-define(CWARN(Cat, Str, Args), ?LOG('elogger-warn', ?LOG_LEVEL_WARN, Cat, Str, Args, [])).
-endif.
-define(WARN(Str, Args), ?CWARN(default, Str, Args)).

-ifdef(no_error).
-define(CERROR(Cat, Str, Args), ?NO_LOG(Cat, Str, Args)).
-define(CTHROW(Cat, Str, Args), ?NO_LOG(Cat, Str, Args)).
-else.
-define(CERROR(Cat, Str, Args), ?LOG('elogger-error', ?LOG_LEVEL_ERROR, Cat, Str, Args, erlang:get_stacktrace())).
-define(CTHROW(Cat, Str, Args), try throw({}) catch _:_ -> ?CERROR(Cat, Str, Args) end).
-endif.
-define(ERROR(Str, Args), ?CERROR(default, Str, Args)).
-define(THROW(Str, Args), ?CTHROW(default, Str, Args)).

-ifdef(no_fatal).
-define(CFATAL(Cat, Str, Args), ?NO_LOG(Cat, Str, Args)).
-else.
-define(CFATAL(Cat, Str, Args), ?LOG('elogger-fatal', ?LOG_LEVEL_FATAL, Cat, Str, Args, erlang:get_stacktrace())).
-endif.
-define(FATAL(Str, Args), ?CFATAL(default, Str, Args)).