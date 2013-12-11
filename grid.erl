-module(grid).

-export([completeColumnCover/3, columnCover/3, makeGrid/2, grid/1]).

-include("voting.hrl").

-spec grid([vid()]) -> #vstruct{}.

grid(Ids) ->
    {Rows, Cols, D} = makeGrid(length(Ids), true),
    %% use columnCover, completeColumnCover, merge_vstructs to construct the
    %% tree-shaped voting structure
    ok.

-type grid_spec() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-spec completeColumnCover([vid()], non_neg_integer(), grid_spec()) ->
    #vstruct{}.

completeColumnCover(Ids, Col, {Rows, Cols, _}) ->
    RowsIds = chunk(Cols, Ids),
    ColIds = lists:map(fun(RowIds) -> lists:nth(Col, RowIds) end, RowsIds),
    Phys = lists:map(fun(Id) -> #vstruct_p{id = Id} end, ColIds),
    {Indices, _} = lists:mapfoldl(fun(Id, K) -> {{Id, [[K]]}, K+1} end,
                                  0, ColIds),
    #vstruct{tree = #vstruct_v{thresh = Rows, children = Phys},
             indices = orddict:from_list(Indices)}.

-spec columnCover([vid()], non_neg_integer(), grid_spec()) -> #vstruct{}.

columnCover(Ids, Col, GridSpec) ->
    #vstruct{tree = T, indices = I} = completeColumnCover(Ids, Col, GridSpec),
    #vstruct{tree = T#vstruct_v{thresh = 1}, indices = I}.

-spec makeGrid(non_neg_integer(), boolean()) -> grid_spec().

makeGrid(N, FavoringRows) ->
    {Rows, Cols} = makeGridFavoringRows(N),
    D = Rows * Cols - N,
    if FavoringRows -> {Rows, Cols, D};
       true -> {Cols, Rows, D}
    end.

-spec makeGridFavoringRows(non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer()}.

makeGridFavoringRows(N) when N < 3 ->
    {N, 1};

makeGridFavoringRows(N) ->
    Sqrt = math:sqrt(N),
    Rows = ceil(Sqrt),
    Floor = floor(Sqrt),
    Cols = if Rows * Floor < N -> Rows;
              true -> Floor
           end,
    {Rows, Cols}.

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceil(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

drop(_, []) ->
    [];

drop(0, L) ->
    L;

drop(N, [_|L]) ->
    drop(N-1, L).

chunk(_, []) ->
    [];

chunk(N, L) ->
    [lists:sublist(L, N) | chunk(N, drop(N, L))].
