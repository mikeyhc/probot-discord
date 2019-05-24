-module(dice_plugin).

-export([load/0]).

load() ->
    #{name => <<"dice">>,
      version => <<"v0.1">>,
      commands =>
      #{<<"roll">> => fun roll/1
       }}.

roll([Upper]) ->
    Int = binary_to_integer(Upper),
    V = rand:uniform(Int),
    {ok, binary:list_to_bin(lists:flatten(io_lib:format("~p", [V])))}.
