-module(match).
-compile(export_all).

create(MatchName, OwnerName, OwnerPid) ->
  util:start(MatchName, {match, route_messages, [MatchName, dict:new()]}),
  join(MatchName, OwnerName, OwnerPid).

exists(MatchName) ->
  global:whereis_name(MatchName).

join(MatchName, PlayerName, PlayerPid) ->
  global:send(MatchName, {join, PlayerName, PlayerPid}).

stop(MatchName) ->
  global:send(MatchName, stop).

quit(MatchName, PlayerName) ->
  global:send(MatchName, {quit, PlayerName}).

send(MatchName, PlayerName, Message) ->
  global:send(MatchName, {send, PlayerName, Message}).

broadcast(joined, PlayerName, Players) ->
  dict:map(fun(_, PlayerPid) ->
    PlayerPid ! {joined, PlayerName} end, Players).

broadcast(Key, From, Message, Players) ->
  dict:map(fun(_, PlayerPid) ->
    PlayerPid ! {Key, From, Message} end, Players).

route_messages(MatchName, Players) ->
  receive
    {send, From, Message} ->
      case dict:find(From, Players) of
        {ok, _} ->
          broadcast(recv, From, Message, Players);
        error ->
          player_does_not_belongs_to_this_match
      end,
      route_messages(MatchName, Players);
    {join, PlayerName, PlayerPid} ->
      broadcast(joined, PlayerName, Players),
      route_messages(MatchName, dict:store(PlayerName, PlayerPid, Players));
    {quit, PlayerName} ->
      case dict:find(PlayerName, Players) of
        {ok, PlayerPid} ->
          PlayerPid ! stop,
          broadcast(gone, PlayerName, Players),
          route_messages(MatchName, dict:erase(PlayerName, Players));
        error ->
          route_messages(MatchName, Players)
      end;
    shutdown ->
      io:format("Shutting down~n");
    Oops ->
      io:format("Unknown message~p~n", [Oops]),
      route_messages(MatchName, Players)
  end.
