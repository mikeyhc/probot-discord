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

%handle_message_test_() ->
%    Set = [{{0,
%             #payload{op=0, d=#{<<"content">> => <<"">>}},
%             #{session_id => 0}},
%            #{session_id => 0}}
%          ],
%    F = fun({{A, B, C}, Out}) ->
%                ?_assertEqual(Out, discord_gateway:handle_message(A, B, C))
%        end,
%    lists:map(F, Set).

make_heartbeat_test_() ->
    Set = [{#payload{d = #{<<"heartbeat_interval">> => 100}, s = 0},
            #{interval => 100, seq => 0}}],
    lists:map(
      fun({I, O}) -> ?_assertEqual(discord_gateway:make_heartbeat(I), O) end,
      Set).
