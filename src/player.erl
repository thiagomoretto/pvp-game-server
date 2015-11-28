-module(player).
-compile(export_all).

create(Socket) ->
  Player = {unidentified, Socket},
  Pid = spawn_link(player, handle_player_commands, [Player, none]),
  io:format("player created with pid ~p~n", [Pid]),
  Pid.

signin(PlayerPid, PlayerName) ->
  PlayerPid ! {signin, PlayerName}.

create_match(PlayerPid, MatchName) ->
  PlayerPid ! {create, MatchName}.

join(PlayerPid, MatchName) ->
  PlayerPid ! {join, MatchName}.

send_command(PlayerPid, Command) ->
  PlayerPid ! {send, Command}.

handle_player_commands(Player, MatchName) ->
  {PlayerName, Socket} = Player,
  receive
    {signin, NewPlayerName} ->
      handle_player_commands({NewPlayerName, Socket}, MatchName);
    {create, NewMatchName} ->
      % TODO:
      match:create(NewMatchName, PlayerName, self()),
      handle_player_commands(Player, NewMatchName);
    {join, NewMatchName} ->
      % TODO:
      match:join(NewMatchName, PlayerName, self()),
      handle_player_commands(Player, NewMatchName);
    {send, Command} ->
      % TODO:
      io:format("send message ~p to match ~p~n", [Command, MatchName]),
      match:send(MatchName, PlayerName, Command),
      handle_player_commands(Player, MatchName);
    {recv, From, Command} ->
      io:format("~p sent ~p~n", [From, Command]),
      tcp:reply(Socket, Command),
      handle_player_commands(Player, MatchName);
    % TODO: Handle EXIT signals.
    {joined, PlayerName} ->
      tcp:reply(Socket, string:join(["joined", PlayerName], ";")),
      handle_player_commands(Player, MatchName);
    stop ->
      ok;
    _ ->
      io:format("Oops"),
      handle_player_commands(Player, MatchName)
  end.
