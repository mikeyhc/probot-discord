-module(dice_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

rev_to_int_test_() ->
    [?_assertEqual(0, dice_plugin:rev_to_int("0")),
     ?_assertEqual(12, dice_plugin:rev_to_int("21")),
     ?_assertEqual(-12, dice_plugin:rev_to_int("21-"))
    ].

calculate_test_() ->
    Rand0 = dice_plugin:calculate([1, 10, d]),
    Rand1 = dice_plugin:calculate([0, 10, d]),
    Rand2 = dice_plugin:calculate([2, 10, d]),
    [?_assertEqual(4, dice_plugin:calculate([2,2,'+'])),
     ?_assertEqual(0, dice_plugin:calculate([2,2,'-'])),
     ?_assertEqual(4, dice_plugin:calculate([2,2,'*'])),
     ?_assertEqual(1.0, dice_plugin:calculate([2,2,'/'])),
     ?_assert(Rand0 >= 1 andalso Rand0 =< 10),
     ?_assertEqual(Rand1, 0),
     ?_assert(Rand2 >= 1 andalso Rand2 =< 20)
    ].

tokenize_test_() ->
    Tests = [{[1, '+', 1], "1+1"},
             {[1, '-', 1], "1-1"},
             {[1, '*', 1], "1*1"},
             {[1, '/', 1], "1/1"},
             {[1, 'd', 1], "1d1"},
             {[1, 'd', 1], "1 d 1"},
             {[12], "12"}],
    lists:map(fun({O, I}) ->
                      L = binary:list_to_bin(I),
                      ?_assertEqual(O, dice_plugin:tokenize(L))
              end,
              Tests).

postfixify_test_() ->
    Tests = [{[1, 1, '+'], [1, '+', 1]},
             {[1, 2, 3, '/', '-', 4, '+'], [1, '-', 2, '/', 3, '+', 4]},
             {[1, 2, 3, d, '*'], [1, '*', 2, d, 3]},
             {[12], [12]}
            ],
    lists:map(fun({O, I}) -> ?_assertEqual(O, dice_plugin:postfixify(I)) end,
              Tests).
