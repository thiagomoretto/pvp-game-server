-module(player).
-compile(export_all).

register(Player) ->
  {PlayerName, _} = Player,
  Pid = spawn(player, handle_commands, [Player]),
  io:format("Player ~p registered with PID ~p~n", [PlayerName, Pid]),
  router:register_player(PlayerName, Pid).

send_command(OtherPlayerName, Command) ->
  router:send_command(OtherPlayerName, Command).

handle_commands(Player) ->
  {PlayerName, Socket} = Player,
  receive
    {handle_command, Command} ->
      io:format("~p received ~p~n", [PlayerName, Command]),
      tcp:reply(Socket, Command),
      handle_commands(Player);
    stop ->
      ok
  end.
