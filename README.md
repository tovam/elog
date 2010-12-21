# elog
Yet another logging library for Erlang

## Overview
**elog** is very basic logging library for Erlang.  Its main advantage is its low foot-print.
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
6. To decide what to log by default you can set the `log_level` environment variable for the app
7. If you want to log to a file instead of the console, just set the `log_file` environment variable with the file name

## Good News
We've added a couple of convenient functions to let our users log better. We list them here, check the rest of the documentation for them
- You can set the log level for a particular module
- You can set the log level for a particular regular expression
- You can trap error_logger messages
- You can even replace error_logger default handlers

## Bad News
There're many many things missing for this project to became a really logging subsystem. For instance...
- Logging to a file is almost a HACK, not really well designed
- When you decide to set the log level for a module, every other module compiled with that level on will start sending messages to the logger. The logger will ignore them anyway, but if you're concern about the amount of messages sent through the system you should use this feature with extra care
- The logging format is hardcoded