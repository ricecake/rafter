-module(rafter_voting).

-export([quorum/2, merge_vstructs/3, init_vstate/1, vote/3, vote/1, to_list/1]).

-include("rafter.hrl").

-spec merge_vstructs(pos_integer(), pos_integer(), [#vstruct{}]) ->
    #vstruct{}.
merge_vstructs(Votes, Thresh, Structs0) ->
    {Structs1, {_, Indices}} = lists:mapfoldl(
                                 fun(Struct0, {I, AccIndices}) ->
                                         {Struct1, Indices} = prepend_paths(
                                                                I, Struct0),
                                         NewIndices = combine_indices(
                                                        AccIndices, Indices),
                                         {Struct1, {I + 1, NewIndices}}
                                 end, {0, orddict:new()}, Structs0),
    Root = #vstruct_v{votes = Votes, thresh = Thresh, children = Structs1},
    #vstruct{tree = Root, indices = Indices}.

-spec combine_indices([index()], [index()]) -> [index()].
combine_indices(IndicesA, IndicesB) ->
    orddict:fold(
      fun(Id, Paths, AccIndices) ->
              lists:foldl(fun(Path, Indices) ->
                                  orddict:append(Id, Path, Indices)
                          end, AccIndices, Paths)
      end, IndicesA, IndicesB).

-spec prepend_paths(non_neg_integer(), #vstruct{}) ->
    {#vstruct_p{} | #vstruct_v{}, [index()]}.
prepend_paths(Index, #vstruct{tree = Tree, indices = Indices}) ->
    NewIndices = orddict:map(
                   fun(_, Paths) ->
                           lists:map(fun(Path) -> [Index|Path] end, Paths)
                   end, Indices),
    {Tree, NewIndices}.

-spec init_vstate(#vstruct{}) -> #vstate{}.
init_vstate(#vstruct{tree = Tree, indices = Indices}) ->
    #vstate{tree = init_vstate_rec(Tree), indices = Indices}.

-spec init_vstate_rec(#vstruct_p{}) -> #vstate_p{}
                   ; (#vstruct_v{}) -> #vstate_v{}.
init_vstate_rec(#vstruct_p{votes = V}) ->
    #vstate_p{votes = V};
init_vstate_rec(#vstruct_v{votes = V, thresh = T, children = Structs}) ->
    States = lists:map(fun init_vstate_rec/1, Structs),
    #vstate_v{votes = V, thresh = T, children = States}.

-spec quorum(#vstruct{}, dict()) -> boolean().
quorum(Struct, Votes) ->
    State0 = init_vstate(Struct),
    State1 = dict:fold(
               fun(_Vid, _Vote, yes) -> yes;
                  (_Vid, _Vote, no) -> no;
                  (Vid, Vote, State) ->
                       NewState = vote(State, Vid, Vote),
                       Result = vote(NewState),
                       case Result of pending -> NewState; _ -> Result end
               end,
               State0, Votes),
    case State1 of yes -> true; _ -> false end.

-spec vote(#vstate{}, peer(), yes | no) -> #vstate{}.
vote(S = #vstate{tree = Tree, indices = Indices}, Vid, Vote) ->
    try orddict:fetch(Vid, Indices) of
        Paths ->
            NewTree = lists:foldl(
                        fun(Path, State) -> vote_rec(State, Path, Vote) end,
                        Tree, Paths),
            #vstate{tree = NewTree, indices = orddict:erase(Vid, Indices)}
    catch
        error:function_clause ->
            %% node identifier not found in indices list
            S
    end.

-spec vote(#vstate{} | #vstate_v{} | #vstate_p{}) -> vote().
vote(#vstate_v{yes_votes = Yes, thresh = T})
  when Yes >= T ->
    yes;
vote(#vstate_v{thresh = T, no_votes = No, children = States})
  when No > length(States) - T ->
    no;
vote(#vstate_v{}) ->
    pending;
vote(#vstate{tree = T}) ->
    vote(T);
vote(#vstate_p{vote = V}) ->
    V.

-spec vote_rec(#vstate_v{} | #vstate_p{}, path(), yes | no) ->
    #vstate_v{} | #vstate_p{}.
vote_rec(State = #vstate_v{children = States}, [Index|Path], Vote) ->
    {States1, [SubState|States2]} = lists:split(Index, States),
    NewSubState= vote_rec(SubState, Path, Vote),
    NewState = State#vstate_v{children = States1 ++ [NewSubState|States2]},
    acc_votes(NewState);
vote_rec(State = #vstate_p{vote = pending}, [], Vote) ->
    State#vstate_p{vote = Vote}.

-spec acc_votes(#vstate_v{}) -> #vstate_v{}.
acc_votes(State = #vstate_v{children = States}) ->
    Voted = fun(Vote) -> fun(S) -> vote(S) =:= Vote end end,
    Yes = length(lists:filter(Voted(yes), States)),
    No = length(lists:filter(Voted(no), States)),
    State#vstate_v{yes_votes = Yes, no_votes = No}.

-spec to_list(#vstate{} | #vstruct{} | undefined) -> [peer()].
to_list(undefined) ->
    [];
to_list(#vstate{indices = Indices}) ->
    try orddict:fetch_keys(Indices) of
        Ids -> Ids
    catch
        error:function_clause -> []
    end;
to_list(#vstruct{indices = Indices}) ->
    try orddict:fetch_keys(Indices) of
        Ids -> Ids
    catch
        error:function_clause -> []
    end.
