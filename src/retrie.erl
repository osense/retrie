-module(retrie).

-export([new/0, insert/3, lookup/2]).


new() ->
    {<<>>, undefined}.


insert(<<H:8, T/binary>>, Value, Tree) ->
    insert1(H, T, Value, Tree).

insert1(H, <<>>, Value, {Lo, Hi, NodeVal, Array}) ->
    NewTuple = case array:get(H, Array) of
        {Lo1, Hi1, _, Array1} -> {Lo1, Hi1, Value, Array1};
        {Key1, Value1} ->
            NewNode = {H, H, Value, array:new()},
            insert(Key1, Value1, NewNode)
    end,
    {Lo, Hi, NodeVal, array:set(H, NewTuple, Array)};
insert1(H, T, Value, {Lo, Hi, NodeVal, Array}) ->
    NewTuple = insert(T, Value, array:get(H, Array)),
    {min(H, Lo), max(H, Hi), NodeVal, array:set(H, NewTuple, Array)};
insert1(H, T, Value, {<<NH:8, NT/binary>>, NodeVal}) ->
    NewNode = {NH, NH, none, array:set(NH, {NT, NodeVal}, array:new())},
    insert1(H, T, Value, NewNode);
insert1(H, T, Value, _) ->
    {<<H:8, T/binary>>, Value}.


lookup(<<H:8, T/binary>>, Tree) ->
    lookup1(H, T, Tree);
lookup(<<>>, {_, _, Value, _}) ->
    Value.

lookup1(H, T, {Lo, Hi, _, Array}) when H >= Lo, H =< Hi ->
    lookup(T, array:get(H, Array));
lookup1(H, T, {<<H:8, T/binary>>, Value}) ->
    Value.

