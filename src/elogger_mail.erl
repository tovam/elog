%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc Mail {@link elogger}.
%%% ==Usage==
%%%       To use it in elog <a href="overview-summary.html#configuration">config files</a> you need to set 
%%%       the corresponding <i>logger</i> env variable to: <code>{elogger_mail, [Option]}</code><br/>
%%%       (where <code>Option :: {Name::atom(), Value::term()}</code>). Availabe options include:
%%% <dl>
%%%   <dt><b>source</b> :: {SrcName::string(), SrcAddr::string(), SrcPwd::string()}</dt>
%%%     <dd>Source account, with name (SrcName), mail address (SrcAddr) and password (SrcPwd)</dd>
%%%   <dt><b>server</b> :: Host::string() | {Host::string(), Port::pos_integer()}</dt>
%%%     <dd>SMTP server host (and port) to use</dd>
%%%   <dt><b>recipients</b> :: [string()]</dt>
%%%     <dd>People who should receive the mails</dd>
%%%   <dt><b>subject</b> :: string()</dt>
%%%     <dd>A string like the ones used in {@link io:format/2}. It will receive 2 atom parameters: Level and Node</dd>
%%% </dl>
%%% @end
%%%-------------------------------------------------------------------
-module(elogger_mail).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(elogger).

-include("elog.hrl").

-export([init/1, log/2, handle_info/2, terminate/2]).

-record(state, {server      :: {string(), pos_integer()},
                source      :: {string(), string(), string()},
                recipients  :: [string()],
                subject     :: string()}).
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
      {ok, #state{server      = Server,
                  source      = Source,
                  recipients  = proplists:get_value(recipients, Props, [SrcAddr]),
                  subject     = proplists:get_value(subject, Props, "~p - ~p")}};
    Other ->
      {stop, {invalid_source, Other}}
  end.

%%% @hidden
-spec log(elogger:log(), state()) -> {ok, state()}.
log(#log{time        = {_,{HH,Mm,SS}},
         level       = Level,
         module      = Mod,
         line        = Line,
         pid         = Pid,
         node        = Node,
         text        = Text,
         args        = Args,
         stacktrace  = []}, State = #state{server     = Server,
                                           source     = Source,
                                           recipients = Recipients,
                                           subject    = SubjectFormat}) ->
  Subject = io_lib:format(SubjectFormat, [Level, Node]),
  Message = io_lib:format("~2..0b:~2..0b:~2..0b|~s:~p|~p: " ++ Text,
                          [HH,Mm,SS, Pid, Mod, Line | Args]),
  mailer:send(Server, Source, Recipients, Subject, Message),
  {ok, State};
log(#log{time        = {_,{HH,Mm,SS}},
         level       = Level,
         module      = Mod,
         line        = Line,
         pid         = Pid,
         node        = Node,
         text        = Text,
         args        = Args,
         stacktrace  = Stack}, State = #state{server     = Server,
                                              source     = Source,
                                              recipients = Recipients,
                                              subject    = SubjectFormat}) ->
  Subject = io_lib:format(SubjectFormat, [Level, Node]),
  Message = io_lib:format("~2..0b:~2..0b:~2..0b|~s:~p|~p: " ++ Text ++
                            "~n\tStack Trace:~n\t\t~p~n",
                          [HH,Mm,SS, Pid, Mod, Line | Args] ++ [Stack]),
  mailer:send(Server, Source, Recipients, Subject, Message),
  {ok, State}.

%%% @hidden
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(_Info, State) -> {ok, State}.

%%% @hidden
-spec terminate(normal | shutdown | term(), state()) -> ok.
terminate(_Reason, _State) -> ok.