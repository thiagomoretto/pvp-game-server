-module(tcp).
-compile(export_all).

-define(SEP, ";").

start_server(Port) ->
  router:start(),
  Pid = spawn_link(fun() ->
      {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
      spawn(fun() -> acceptor(Listen) end),
      timer:sleep(infinity)
  end),
  {ok, Pid}.

acceptor(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> acceptor(ListenSocket) end),
  handle(Socket).

handle(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket);
    {tcp, Socket, <<"\r\n", _binary>>} ->
      handle(Socket);
    {tcp, Socket, Binary} ->
      String = erlang:binary_to_list(Binary),
      Tokens = string:tokens(String, ?SEP),
      case handle_command(Socket, Tokens) of
        ok ->
          gen_tcp:send(Socket, "-OK\n");
        error ->
          gen_tcp:send(Socket, "-ERROR\n")
      end,
      handle(Socket)
  end.

handle_command(Socket, [Command, PlayerName | _Args]) when Command =:= "register" ->
  player:register({PlayerName, Socket}),
  ok;

handle_command(_, [Command, OtherPlayerName | Args]) when Command =:= "send" ->
  player:send_command(OtherPlayerName, Args),
  ok;

handle_command(_, _) ->
  io:format("Unknown message~n"),
  error.

reply(Socket, Message) ->
  io:format("<< message ~p~n", [Message]),
  gen_tcp:send(Socket, Message),
  ok.
