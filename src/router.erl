-module(router).
-define(SERVER, router).
-compile(export_all).

start() ->
  util:start(?SERVER, {router, route_commands, [dict:new()]}).

stop() ->
  util:stop(?SERVER).

register_player(PlayerName, PlayerPid) ->
  global:send(?SERVER, {register_player, PlayerName, PlayerPid}).

unregister_player(PlayerName) ->
  global:send(?SERVER, {unregister_player, PlayerName}).

send_command(ToPlayerName, Command) ->
  global:send(?SERVER, {send_command, ToPlayerName, Command}).

route_commands(Players) ->
  receive
    {send_command, PlayerName, Command} ->
      case dict:find(PlayerName, Players) of
        {ok, PlayerPid} ->
          PlayerPid ! {handle_command, Command};
        error ->
          do_something
      end,
      route_commands(Players);
    {register_player, PlayerName, PlayerPid} ->
      route_commands(dict:store(PlayerName, PlayerPid, Players));
    {unregister_player, PlayerName} ->
      case dict:find(PlayerName, Players) of
        {ok, PlayerPid} ->
          PlayerPid ! stop,
          route_commands(dict:erase(PlayerName, Players));
        error ->
          route_commands(Players)
      end;
    shutdown ->
      io:format("Shutting down~n");
    Oops ->
      io:format("Unknown command~p~n", [Oops]),
      route_commands(Players)
  end.
