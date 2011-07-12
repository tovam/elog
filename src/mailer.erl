%%%-------------------------------------------------------------------
%%% @author Chad DePue <chad@inakanetworks.com>
%%% @copyright (C) 2011 Chad DePue <chad@inakanetworks.com>
%%% @doc Simple SMTP Helper. Send mails through SMTP
%%% @end
%%%-------------------------------------------------------------------
-module(mailer).
-author('Chad DePue <chad@inakanetworks.com>').

-export([send/5]).

%%% @doc  Sends a mail
-spec send({string(), integer()}, {string(), string(), string()}, [string()], iodata(), iodata()) -> ok.
send(Server, Source, Recipients, Subject, Body) ->
  _Pid = spawn(fun() -> send_message(Server, Source, Recipients, Subject, Body) end),
  ok.

send_message(_Server, _Source, [], _Subject, _Body) -> ok;
send_message({Host, Port}, {SrcName, SrcAddr, SrcPwd}, Recipients, Subject, Body) ->
  try
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
    ok = send_no_recv(Socket, Body),
    ok = send_no_recv(Socket, ""),
    ok = send(Socket, "."),
    ok = send(Socket, "QUIT"),
    ssl:close(Socket),
    io:format("Mail sent from ~s <~s> to ~p~n", [SrcName, SrcAddr, Recipients])
  catch
    _:Error ->
      io:format("Could not send from ~s <~s> to ~p~n\tError: ~p~n", [SrcName, SrcAddr, Recipients, Error]),
      ok
  end.

send_no_recv(Socket, Data) ->
  %io:format([$> | Data] ++ [13,10]),
  ssl:send(Socket, Data ++ [13,10]).

send(Socket, Data) ->
  case send_no_recv(Socket, Data) of
    ok -> recv(Socket);
    Error -> Error
  end.

recv(Socket) ->
  case ssl:recv(Socket, 0, 30000) of
    {ok, _Return} ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.