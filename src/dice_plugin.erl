-module(dice_plugin).

-export([load/0, tokenize/1, postfixify/1, calculate/1]).

load() ->
    #{name => <<"dice">>,
      version => <<"v0.1">>,
      commands =>
      #{<<"roll">> => fun roll/1
       }}.

roll([Body]) ->
    Tokens = tokenize(Body),
    Postfix = postfixify(Tokens),
    V = calculate(Postfix),
    {ok, binary:list_to_bin(lists:flatten(io_lib:format("~p", [V])))}.

rev_to_int(I) ->
    list_to_integer(lists:reverse(I)).

tokenize(Body) ->
    L0 = binary:bin_to_list(Body),
    F0 = fun($+, {B, T}) -> {"", ['+', rev_to_int(B)|T]};
            ($-, {B, T}) -> {"", ['-', rev_to_int(B)|T]};
            ($*, {B, T}) -> {"", ['*', rev_to_int(B)|T]};
            ($/, {B, T}) -> {"", ['/', rev_to_int(B)|T]};
            ($d, {B, T}) -> {"", [d, rev_to_int(B)|T]};
            ($ , Acc) -> Acc;
            (X, {B, T}) -> {[X|B], T}
         end,
    {Last, Tokens} = lists:foldl(F0, {"", []}, L0),
    lists:reverse([rev_to_int(Last)|Tokens]).

postfixify(Stack) ->
    postfixify(Stack, []).

postfixify([], S) -> lists:reverse(S);
% TODO check if this can be accessed by a list with more than 1 element
postfixify([N], S) -> lists:reverse([N|S]);
postfixify([Op, A|R], S) ->
    if is_atom(Op) -> postfixify(R, push_op(Op, A, S));
       true -> postfixify([A|R], [Op|S])
    end.

push_op(Op, A, [Last|S]) ->
    if is_atom(Last) ->
           case op_precedence(Op) < op_precedence(Last) of
               true -> [Last, Op, A|S];
               false -> [Op, A, Last|S]
           end;
       true -> [Op, A, Last|S]
    end.

op_precedence('+') -> 5;
op_precedence('-') -> 5;
op_precedence('*') -> 3;
op_precedence('/') -> 3;
op_precedence(d) -> 0.

calculate(Stack) ->
    calculate(Stack, []).

calculate([], [V]) -> V;
calculate([X|R], S) ->
    if is_atom(X) -> calculate(R, apply_op(X, S));
       true -> calculate(R, [X|S])
    end.

apply_op('+', [X, Y|R]) -> [Y + X|R];
apply_op('-', [X, Y|R]) -> [Y - X|R];
apply_op('*', [X, Y|R]) -> [Y * X|R];
apply_op('/', [X, Y|R]) -> [Y / X|R];
apply_op(d, [X, Y|R]) ->
    [lists:foldl(fun(_, V) -> rand:uniform(X) + V end,
                 0, lists:seq(1, Y))|R].
