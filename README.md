# elog
Yet another logging library for Erlang

## Overview
**elog** is very basic logging library for Erlang. Its main advantage is its low foot-print.
It was tested on a very log-intensive application running on a server with 128K concurrent clients and it worked smoothly.

## Usage
1. First you should include `"elog.hrl"` in each module you wish to log from
2. To log something, just use `?LEVEL(Str, Args)` macros on your code. The available levels are:
    - `DEBUG`
    - `INFO`
    - `STAT`
    - `WARN`
    - `ERROR`
    - `THROW` (Which behaves like `ERROR` but throws and catches an ad-hoc exception to let you see the real stack trace when you're out of an exception catch block)
    - `FATAL`
3. You can use `no_LEVEL` macros to avoid the log calls of level _LEVEL_ when compiling
4. You can use `no_log` to compile your code with no log at all
5. To start logging, start the app (i.e. `elog:start().` or `application:start(elog).`)
6. For further information, check the  [`elog` API Documentation](http://elbrujohalcon.github.com/elog/) 