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
  ReplyFun = fun(Message) -> reply(Socket, Message) end,
  PlayerPid = player:create(ReplyFun),
  handle(Socket, PlayerPid).

handle(Socket, PlayerPid) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket),
      player:stop(PlayerPid);
    {tcp, Socket, <<"\r\n", _binary>>} ->
      handle(Socket, PlayerPid);
    {tcp, Socket, Binary} ->
      String = erlang:binary_to_list(Binary),
      Tokens = string:tokens(String, ?SEP),
      case handle_command(Socket, PlayerPid, Tokens) of
        ok ->
          gen_tcp:send(Socket, "-OK\n");
        error ->
          gen_tcp:send(Socket, "-ERROR\n")
      end,
      handle(Socket, PlayerPid)
  end.

handle_command(_, PlayerPid, [Command, PlayerName | _Args]) when Command =:= "signin" ->
  player:signin(PlayerPid, PlayerName),
  ok;

handle_command(_, PlayerPid, [Command, MatchName | _Args]) when Command =:= "create" ->
  player:create_match(PlayerPid, MatchName),
  ok;

handle_command(_, PlayerPid, [Command, MatchName | _Args]) when Command =:= "join" ->
  player:join(PlayerPid, MatchName),
  ok;

handle_command(_, PlayerPid, [Command | Args]) when Command =:= "send" ->
  player:send_command(PlayerPid, string:join(Args, ?SEP)),
  ok;

handle_command(_, _, _) ->
  io:format("Unknown message~n"),
  error.

reply(Socket, Atom) when erlang:is_atom(Atom) ->
  gen_tcp:send(Socket, erlang:atom_to_list(Atom));

reply(Socket, Message) ->
  io:format("<< message ~p~n", [Message]),
  gen_tcp:send(Socket, Message),
  ok.
