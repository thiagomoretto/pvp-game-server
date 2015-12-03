-module(player).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

create(ReplyFun) ->
  Player = {unidentified, ReplyFun},
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
  {PlayerName, ReplyFun} = Player,
  receive
    {signin, NewPlayerName} ->
      handle_player_commands({NewPlayerName, ReplyFun}, MatchName);
    {create, NewMatchName} ->
      case match:exists(NewMatchName) of
        undefined ->
          match:create(NewMatchName, PlayerName, self()),
          ReplyFun(match_created);
        _ ->
          ReplyFun(match_already_exists)
      end,
      handle_player_commands(Player, NewMatchName);
    {join, NewMatchName} ->
      case match:exists(NewMatchName) of
        undefined ->
          ReplyFun(match_does_not_exists);
        _ ->
          match:join(NewMatchName, PlayerName, self())
      end,
      handle_player_commands(Player, NewMatchName);
    {send, Command} ->
      case match:exists(MatchName) of
        undefined ->
          ReplyFun(match_does_not_exists);
        _ ->
          match:send(MatchName, PlayerName, Command)
      end,
      handle_player_commands(Player, MatchName);
    {recv, _From, Command} ->
      ReplyFun(Command),
      handle_player_commands(Player, MatchName);
    % TODO: Handle EXIT signals.
    {joined, PlayerName} ->
      ReplyFun(string:join(["joined", PlayerName], ";")),
      handle_player_commands(Player, MatchName);
    stop ->
      ok;
    _ ->
      io:format("Oops"),
      handle_player_commands(Player, MatchName)
  end.
