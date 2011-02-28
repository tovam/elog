%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc Abridged Mail {@link elogger}.
%%% ==Usage==
%%%       To use it in elog <a href="overview-summary.html#configuration">config files</a> you need to set 
%%%       the corresponding <i>logger</i> env variable to: <code>{elogger_abridged_mail, [Option]}</code><br/>
%%%       (where <code>Option :: {Name::atom(), Value::term()}</code>). Availabe options include:
%%% <dl>
%%%   <dt><b>minutes</b> :: pos_integer()</dt>
%%%     <dd>Minimal time to wait between mails (in minutes)</dd>
%%%   <dt><b>count</b> :: pos_integer()</dt>
%%%     <dd>If this parameter is set, and the logger collects <i>count</i> messages, the mail will
%%%         be sent disregarding the <b>minutes</b> parameter</dd>
%%%   <dt><b>source</b> :: {SrcName::string(), SrcAddr::string(), SrcPwd::string()}</dt>
%%%     <dd>Source account, with name (SrcName), mail address (SrcAddr) and password (SrcPwd)</dd>
%%%   <dt><b>server</b> :: Host::string() | {Host::string(), Port::pos_integer()}</dt>
%%%     <dd>SMTP server host (and port) to use</dd>
%%%   <dt><b>recipients</b> :: [string()]</dt>
%%%     <dd>People who should receive the mails</dd>
%%%   <dt><b>subject</b> :: string()</dt>
%%%     <dd>A string to be used as the subject</dd>
%%% </dl>
%%% @end
%%%-------------------------------------------------------------------
-module(elogger_abridged_mail).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(elogger).

-include("elog.hrl").

-export([init/1, log/2, handle_info/2, terminate/2]).

-record(state, {server      :: {string(), pos_integer()},
                source      :: {string(), string(), string()},
                recipients  :: [string()],
                subject     :: string(),
                logs = []   :: [#log{}],
                count       :: infinity | pos_integer()}).
-opaque state() :: #state{}.

%%% @hidden
-spec init([proplists:property()]) -> {ok, state()} | {stop, {invalid_source, term()}}.
init(Props) ->
  case proplists:get_value(source, Props) of
    Source = {_, SrcAddr, _} ->
      Server =
        case proplists:get_value(server, Props, {"smtp.gmail.com", 465}) of
          {H, P} -> {H, P};
          H -> {H, 465}
        end,
      Minutes = proplists:get_value(minutes, Props),
      Count   = proplists:get_value(count,   Props, infinity),
      case {Minutes, Count} of
        {undefined, infinity} ->
          {stop, {invalid_parameters, Minutes, Count}};
        {Minutes, Count} ->
          case Minutes of
            undefined -> noop;
            Minutes -> {ok, _Timer} = timer:send_interval(erlang:trunc(Minutes * 60 * 1000),
                                                          send_mail)
          end,
          {ok, #state{server      = Server,
                      source      = Source,
                      recipients  = proplists:get_value(recipients, Props, [SrcAddr]),
                      subject     = proplists:get_value(subject, Props, "~p - ~p"),
                      count       = Count}}
      end;
    Other ->
      {stop, {invalid_source, Other}}
  end.

%%% @hidden
-spec log(elogger:log(), state()) -> {ok, state()}.
log(Log, State = #state{logs  = Logs,
                        count = Count}) when Count > (1 + length(Logs)) ->
  {ok, State#state{logs = [Log|Logs]}};
log(Log, State = #state{logs  = Logs}) ->
  handle_info(send_mail, State#state{logs = [Log|Logs]}).

-spec handle_info(send_mail | term(), state()) -> {ok, state()}.
handle_info(send_mail, State = #state{logs = []}) -> {ok, State};
handle_info(send_mail, State = #state{logs       = Logs,
                                      server     = Server,
                                      source     = Source,
                                      recipients = Recipients,
                                      subject    = Subject}) ->
  Message =
    lists:map(fun(#log{time        = {_,{HH,Mm,SS}},
                       level       = Level,
                       module      = Mod,
                       line        = Line,
                       pid         = Pid,
                       node        = Node,
                       text        = Text,
                       args        = Args,
                       stacktrace  = []}) ->
                      io_lib:format("~2..0b:~2..0b:~2..0b|~p|~s|~s:~p|~p: " ++ Text,
                                    [HH,Mm,SS, Pid, Node, Mod, Line, Level | Args]);
                 (#log{time        = {_,{HH,Mm,SS}},
                       level       = Level,
                       module      = Mod,
                       line        = Line,
                       pid         = Pid,
                       node        = Node,
                       text        = Text,
                       args        = Args,
                       stacktrace  = Stack}) ->
                      io_lib:format("~2..0b:~2..0b:~2..0b|~p|~s|~s:~p|~p: " ++ Text ++
                                      "~n\tStack Trace:~n\t\t~p~n",
                                    [HH,Mm,SS, Pid, Node, Mod, Line, Level | Args] ++ [Stack])
              end, lists:reverse(Logs)),
  mailer:send(Server, Source, Recipients, Subject, Message),
  {ok, State#state{logs = []}};
handle_info(_Info, State) -> {ok, State}.

%%% @hidden
-spec terminate(normal | shutdown | term(), state()) -> {ok, state()}.
terminate(_Reason, State) -> handle_info(send_mail, State).