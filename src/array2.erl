-module(array2).
-author("osense").

-export([new/0, set/3, get/2, to_list/1, to_orddict/1]).
-export_type([array2/0]).

-define(DEFAULT_VAL, undefined).

-type array2() :: {idx(), idx(), tuple()} | {}.
-type idx() :: integer().


-spec new() -> array2().
new() ->
    {}.


-spec set(idx(), term(), array2()) -> array2().
set(N, Value, {Start, End, Data}) ->
    if
        N < Start -> set(N, Value, {N, End, erlang:make_tuple(End - N + 1, ?DEFAULT_VAL, make_initlist(Start - N + 1, Data))});
        N > End -> set(N, Value, {Start, N, erlang:make_tuple(N - Start + 1, ?DEFAULT_VAL, make_initlist(1, Data))});
        true -> {Start, End, setelement(N - Start + 1, Data, Value)}
    end;
set(N, Value, {}) ->
    {N, N, {Value}}.

make_initlist(Start, Data) ->
    lists:zip(lists:seq(Start, Start + tuple_size(Data) - 1), tuple_to_list(Data)).


-spec get(idx(), array2()) -> term().
get(N, {Start, End, Data}) when N >= Start, N =< End ->
    element(N - Start + 1, Data);
get(_, _) ->
    ?DEFAULT_VAL.


-spec to_list(array2()) -> list().
to_list({_, _, Data}) ->
    lists:filter(fun(Elem) -> Elem /= ?DEFAULT_VAL end, tuple_to_list(Data));
to_list({}) ->
    [].


-spec to_orddict(array2()) -> [{pos_integer(), term()}].
to_orddict({Start, End, Data}) ->
    to_orddict1(Start, Start, End, Data, []);
to_orddict({}) ->
    [].

to_orddict1(_, Pos, End, _, Acc) when Pos > End ->
    lists:reverse(Acc);
to_orddict1(Start, Pos, End, Data, Acc) ->
    case element(Pos - Start + 1, Data) of
        ?DEFAULT_VAL -> to_orddict1(Start, Pos + 1, End, Data, Acc);
        Val -> to_orddict1(Start, Pos + 1, End, Data, [{Pos, Val} | Acc])
    end.
