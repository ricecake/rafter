-module(rafter_config).

-include("rafter.hrl").

%% API
-export([quorum/3, quorum_min/3, voters/1, voters/2, followers/2,
         reconfig/2, allow_config/2, has_vote/2]).

%%====================================================================
%% API
%%====================================================================

-spec quorum_min(term(), #config{} | #vstruct{}, dict()) -> non_neg_integer().
quorum_min(_Me, #config{state=blank}, _) ->
    0;
quorum_min(Me, #config{state=stable, oldvstruct=OldServers}, Responses) ->
    quorum_min(Me, OldServers, Responses);
quorum_min(Me, #config{state=staging, oldvstruct=OldServers}, Responses) ->
    quorum_min(Me, OldServers, Responses);
quorum_min(Me, #config{state=transitional,
                   oldvstruct=Old,
                   newvstruct=New}, Responses) ->
    min(quorum_min(Me, Old, Responses),
        quorum_min(Me, New, Responses));

%% Responses doesn't contain the local response so it will be marked as 0
%% when it's a member of the consensus group. In this case we must
%% skip past it in a sorted list so we add 1 to the quorum offset.
quorum_min(_, [], _) ->
    0;
quorum_min(Me, Servers, Responses) ->
    Indices = unique_sort(
                lists:map(fun(S) -> index(S, Responses) end,
                          rafter_voting:to_list(Servers))),
    Accepted = lists:takewhile(
                 fun(Index) -> has_quorum(Me, Servers, Responses, Index) end,
                 Indices),
    lists:last(Accepted).

-spec quorum(term(), #config{} | #vstruct{}, dict()) -> boolean().
quorum(_Me, #config{state=blank}, _Responses) ->
    false;
quorum(Me, #config{state=stable, oldvstruct=OldServers}, Responses) ->
    quorum(Me, OldServers, Responses);
quorum(Me, #config{state=staging, oldvstruct=OldServers}, Responses) ->
    quorum(Me, OldServers, Responses);
quorum(Me, #config{state=transitional, oldvstruct=Old, newvstruct=New}, Responses) ->
    quorum(Me, Old, Responses) andalso quorum(Me, New, Responses);

%% Responses doesn't contain a local vote which must be true if the local
%% server is a member of the consensus group. Add 1 to TrueResponses in
%% this case.
quorum(Me, Struct, Responses) ->
    Servers = rafter_voting:to_list(Struct),
    Votes = filtermap(
              fun(Peer, R) -> R andalso lists:member(Peer, Servers) end,
              fun(_, _) -> yes end,
              dict:append(Me, yes, Responses)),
    rafter_voting:quorum(Struct, Votes).

%% @doc list of voters excluding me
-spec voters(term(), #config{}) -> list().
voters(Me, Config) ->
    lists:delete(Me, voters(Config)).

%% @doc list of all voters
-spec voters(#config{}) -> list().
voters(#config{state=transitional, oldvstruct=Old, newvstruct=New}) ->
    sets:to_list(sets:from_list(
                   rafter_voting:to_list(Old) ++ rafter_voting:to_list(New)));
voters(#config{oldvstruct=Old}) ->
    rafter_voting:to_list(Old).

-spec has_vote(term(), #config{}) -> boolean().
has_vote(_Me, #config{state=blank}) ->
    false;
has_vote(Me, #config{state=transitional, oldvstruct=Old, newvstruct=New})->
    lists:member(Me, rafter_voting:to_list(Old)) orelse
    lists:member(Me, rafter_voting:to_list(New));
has_vote(Me, #config{oldvstruct=Old}) ->
    lists:member(Me, rafter_voting:to_list(Old)).

%% @doc All followers. In staging, some followers are not voters.
-spec followers(term(), #config{}) -> list().
followers(Me, #config{state=transitional, oldvstruct=Old, newvstruct=New}) ->
    lists:delete(Me, sets:to_list(sets:from_list(
                                    rafter_voting:to_list(Old) ++
                                    rafter_voting:to_list(New))));
followers(Me, #config{state=staging, oldvstruct=Old, newvstruct=New}) ->
    lists:delete(Me, sets:to_list(sets:from_list(
                                    rafter_voting:to_list(Old) ++
                                    rafter_voting:to_list(New))));
followers(Me, #config{oldvstruct=Old}) ->
    lists:delete(Me, rafter_voting:to_list(Old)).

%% @doc Go right to stable mode if this is the initial configuration.
-spec reconfig(#config{}, #vstruct{}) -> #config{}.
reconfig(#config{state=blank}=Config, Struct) ->
    Config#config{state=stable, oldvstruct=Struct};
reconfig(#config{state=stable}=Config, Struct) ->
    Config#config{state=transitional, newvstruct=Struct}.

-spec allow_config(#config{}, list()) -> boolean().
allow_config(#config{state=blank}, _NewServers) ->
    true;
allow_config(#config{state=stable, oldvstruct=OldServers}, NewServers)
    when NewServers =/= OldServers ->
    %% TODO: is inequality well-defined on arbitrary records?
    true;
allow_config(#config{oldvstruct=OldServers}, NewServers)
    when NewServers =:= OldServers ->
    {error, not_modified};
allow_config(_Config, _NewServers) ->
    {error, config_in_progress}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec index(atom() | {atom(), atom()}, dict()) -> non_neg_integer().
index(Peer, Responses) ->
    case dict:find(Peer, Responses) of
        {ok, Index} ->
            Index;
        error ->
            0
    end.

-spec unique_sort(list()) -> list().
unique_sort(L) ->
    ordsets:to_list(ordsets:from_list(L)).

-spec has_quorum(term(), #vstruct{}, dict(), non_neg_integer()) ->
    boolean().
has_quorum(Me, Servers, Responses, Index) ->
    Gteq = dict:filter(
                fun(_P, I) -> I >= Index end,
                Responses),
    Votes = dict:map(fun(_P, _I) -> yes end, Gteq),
    case lists:member(Me, rafter_voting:to_list(Servers)) of
        true -> rafter_voting:quorum(Servers, dict:append(Me, yes, Votes));
        false -> rafter_voting:quorum(Servers, Votes)
    end.

-spec filtermap(fun((term(), term()) -> boolean()),
                fun((term(), term()) -> term()),
                dict())
    -> dict().
filtermap(P, F, Dict) ->
    dict:map(F, dict:filter(P, Dict)).
