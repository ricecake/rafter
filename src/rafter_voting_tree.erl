-module(rafter_voting_tree).

-export([tree/1, tree/2]).

-include("rafter.hrl").

-spec tree([peer()]) -> #vstruct{}.
tree(Ids) ->
    tree(Ids, 2).

-spec tree([peer()], pos_integer()) -> #vstruct{}.
tree([RootId|Ids], D)
  when length(Ids) > D ->
    SublistLength = ceil(length(Ids) / D),
    IdLists = chunk(SublistLength, Ids),
    SubTrees = lists:map(fun(SubIds) -> tree(SubIds, D) end, IdLists),
    SubTree = rafter_voting:merge_vstructs(1, 1, SubTrees),
    Root = single_node_vstruct(RootId),
    rafter_voting:merge_vstructs(1, 2, [Root, SubTree]);
tree([RootId|Ids], _D) ->
    % length(Ids) <= D
    Leaves = lists:map(fun(Id) -> #vstruct_p{id = Id} end, Ids),
    {Indices, _} = lists:mapfoldl(fun(Id, K) -> {{Id, [[K]]}, K+1} end,
                                  0, Ids),
    SubTree = #vstruct{tree = #vstruct_v{thresh = 1, children = Leaves},
             indices = dict:from_list(Indices)},
    Root = single_node_vstruct(RootId),
    rafter_voting:merge_vstructs(1, 2, [Root, SubTree]).

-spec drop(non_neg_integer(), list()) -> list().
drop(_, []) ->
    [];
drop(0, L) ->
    L;
drop(N, [_|L]) ->
    drop(N-1, L).

-spec chunk(non_neg_integer(), list()) -> [ list() ].
chunk(_, []) ->
    [];
chunk(N, L) ->
    [lists:sublist(L, N) | chunk(N, drop(N, L))].

-spec single_node_vstruct(peer()) -> #vstruct{}.
single_node_vstruct(Id) ->
    #vstruct{tree = #vstruct_v{thresh = 1, children = [#vstruct_p{id = Id}]},
             indices = dict:from_list([{Id, [1]}])}.

ceil(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
