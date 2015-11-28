-module(player).
-compile(export_all).

register(PlayerName) ->
  Pid = spawn(player, handle_commands, [PlayerName]),
  io:format("Player ~p registered with PID ~p~n", [PlayerName, Pid]),
  router:register_player(PlayerName, Pid).

send_command(OtherPlayerName, Command) ->
  router:send_command(OtherPlayerName, Command).

handle_commands(PlayerName) ->
  receive
    {handle_command, Command} ->
      io:format("~p received ~p~n", [PlayerName, Command]),
      handle_commands(PlayerName);
    stop ->
      ok
  end.
