%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Fernando Benavides <greenmellon@gmail.com>
%%% @doc Mail {@link elogger}
%%% @end
%%%-------------------------------------------------------------------
-module(elogger_mail).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(elogger).

-include("elog.hrl").

-export([init/1, log/2, terminate/2]).

-record(state, {server      :: undefined | string() | {string(), pos_integer()},
                source      :: {string(), string()},
                recipients  :: [string()],
                subject     :: string()}).
-opaque state() :: #state{}.

%%% @hidden
-spec init([proplists:property()]) -> elogger:init_result().
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
                  subject     = proplists:get_value(subject, Props, "~p")}};
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
  Subject = io_lib:format(SubjectFormat, [Level]),
  Message = io_lib:format("~2..0b:~2..0b:~2..0b|~p|~s|~s:~p|~p: " ++ Text,
                          [HH,Mm,SS, Pid, fancy_node(Node), Mod, Line, Level | Args]),
  spawn(fun() -> send_message(Server, Source, Recipients, Subject, Message) end),
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
  Subject = io_lib:format(SubjectFormat, [Level]),
  Message = io_lib:format("~2..0b:~2..0b:~2..0b|~p|~s|~s:~p|~p: " ++ Text ++
                            "~n\tStack Trace:~n\t\t~p~n",
                          [HH,Mm,SS, Pid, fancy_node(Node), Mod, Line, Level | Args] ++ [Stack]),
  spawn(fun() -> send_message(Server, Source, Recipients, Subject, Message) end),
  {ok, State}.

%%% @hidden
-spec terminate(normal | shutdown | term(), {}) -> ok.
terminate(_Reason, _State) -> ok.

fancy_node(SourceNode) ->
  case {string:tokens(atom_to_list(SourceNode), "@"),
        string:tokens(atom_to_list(node()), "@")} of
    {X,X} ->
      "";
    {[X,Y], [_Z,Y]} ->
      X;
    _ ->
      SourceNode
  end.

send_message(_Server, _Source, [], _Subject, _Message) -> ok;
send_message({Host, Port}, {SrcName, SrcAddr, SrcPwd}, Recipients, Subject, Message) ->
  {ok, Socket} = ssl:connect(Host, Port, [{ssl_imp, old},{active, false}], 60000),
  ok = recv(Socket),
  ok = send(Socket, "HELO localhost"),
  ok = send(Socket, "AUTH LOGIN"),
  ok = send(Socket, binary_to_list(base64:encode(SrcAddr))),
  ok = send(Socket, binary_to_list(base64:encode(SrcPwd))),
  ok = send(Socket, ["MAIL FROM:<", SrcAddr, ">"]),
  lists:foreach(fun(Rcpt) -> ok = send(Socket, ["RCPT TO:", $<, Rcpt, $>]) end, Recipients),
  ok = send(Socket, "DATA"),
  ok = send_no_recv(Socket, ["From: ", SrcName, $<, SrcAddr, $>]),
  ok = send_no_recv(Socket, ["Date: ", httpd_util:rfc1123_date()]),
  ok = send_no_recv(Socket, ["Subject: " | Subject]),
  ok = send_no_recv(Socket, "Content-type: text/plain"),
  ok = send_no_recv(Socket, ""),
  ok = send_no_recv(Socket, Message),
  ok = send_no_recv(Socket, ""),
  ok = send(Socket, "."),
  ok = send(Socket, "QUIT"),
  ssl:close(Socket),
  io:format("Mail sent from ~s <~s> to ~p~n", [SrcName, SrcAddr, Recipients]).

send_no_recv(Socket, Data) ->
  %io:format([$> | Data] ++ [13,10]),
  ssl:send(Socket, Data ++ [13,10]).

send(Socket, Data) ->
  send_no_recv(Socket, Data),
  recv(Socket).

recv(Socket) ->
  case ssl:recv(Socket, 0, 30000) of
    {ok, _Return} ->
      %io:format([$< | _Return]),
      ok;
    {error, Reason} ->
      error_logger:error_msg("~p:~p > ~p~n", [?MODULE, ?LINE, Reason]),
      {error, Reason}
  end.