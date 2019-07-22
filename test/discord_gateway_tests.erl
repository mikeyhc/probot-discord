-module(discord_gateway_tests).

-include("discord_gateway.hrl").

-include_lib("eunit/include/eunit.hrl").

decode_payload_test_() ->
    Set = [{<<"{\"op\": 0, \"d\": \"body\"}">>,
            #payload{op = 0, d = <<"body">>}}
    ],
    lists:map(fun({In, Out}) ->
                      ?_assertEqual(Out, discord_gateway:decode_payload(In))
              end, Set).
