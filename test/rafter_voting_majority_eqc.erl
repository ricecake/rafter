-module(rafter_voting_majority_eqc).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("rafter.hrl").

-compile(export_all).

prop_majority_quorum() ->
    ?FORALL(Nodes, lists:seq(0, nat()),
            ?FORALL(YesVotes,
                    lists:sublist(
                      choose(0, length(Nodes) - 1),
                      shuffle(Nodes)),
                    has_quorum(Nodes, YesVotes) =:= length(YesVotes) > length(Nodes) div 2)).

has_quorum(Nodes, YesVotes) ->
    Struct = rafter_voting_majority:majority(Nodes),
    Votes = dict:from_list([{Node, yes} || Node <- Nodes]),
    rafter_voting:quorum(Struct, Votes).
