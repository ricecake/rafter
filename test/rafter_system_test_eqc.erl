-module(rafter_system_test_eqc).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(eqc_statem).

%% eqc_statem exports
-export([command/1, initial_state/0, next_state/3, postcondition/3,
         precondition/2]).

-include("rafter.hrl").
-include("rafter_opts.hrl").

%% This file includes #state{}
-include("rafter_consensus_fsm.hrl").

-compile(export_all).

-record(model_state, {to :: atom(),
                      running :: #vstruct{},
                      state=init :: init | blank | transitional | stable,
                      oldvstruct :: #vstruct{},
                      newvstruct :: #vstruct{},
                      commit_index=0 :: non_neg_integer(),
                      last_committed_op :: term(),
                      leader :: atom()}).


-define(QC_OUT(P),
    eqc:on_output(fun(Str, Args) ->
                io:format(user, Str, Args) end, P)).

-define(logdir, "./rafter_logs").

%% ====================================================================
%% Tests
%% ====================================================================

eqc_test_() ->
    {spawn,
     [
      {setup,
       fun setup/0,
       fun cleanup/1,
       [%% Run the quickcheck tests
        {timeout, 120,
         ?_assertEqual(true,
             eqc:quickcheck(
                 ?QC_OUT(eqc:numtests(50, prop_rafter()))))}
       ]
      }
     ]
    }.

%% ====================================================================
%% Helper functions
%% ====================================================================
setup() ->
    os:cmd(["rm -rf ", ?logdir]),
    os:cmd(["mkdir ", ?logdir]),
    {ok, _Started} = application:ensure_all_started(rafter).

cleanup(_) ->
    application:stop(rafter),
    application:stop(lager).

run_prop(Cmds) ->
        aggregate(command_names(Cmds), run_print_cleanup(Cmds)).

run_print_cleanup(Cmds) ->
    {H, S, Res} = run_commands(?MODULE, Cmds),
    eqc_statem:pretty_commands(?MODULE,
        Cmds,
        {H, S, Res},
        cleanup_test(S, Res)).

cleanup_test(S, Res) ->
    Peers = rafter_voting:to_list(S#model_state.running),
    [rafter:stop_node(P) || P <- Peers],
    os:cmd(["rm -rf ", ?logdir]),
    os:cmd(["mkdir ", ?logdir]),
    ok =:= Res.

start_node(Peer) ->
    Opts = #rafter_opts{logdir=?logdir},
    rafter:start_node(Peer, Opts).

start_nodes(Servers) when is_list(Servers) ->
    [start_node(S) || S <- Servers];
start_nodes(Servers) ->
    [start_node(S) || S <- rafter_voting:to_list(Servers)].

%% ====================================================================
%% EQC Properties
%% ====================================================================

prop_rafter() ->
    ?FORALL(Cmds, more_commands(5, commands(?MODULE)), run_prop(Cmds)).


%% ====================================================================
%% eqc_statem callbacks
%% ====================================================================
initial_state() ->
    #model_state{}.

command(#model_state{state=init}) ->
    {call, ?MODULE, start_nodes, [vstruct()]};

command(#model_state{state=blank, to=To, running=Running}) ->
    {call, rafter, set_config, [To, Running]};

command(#model_state{state=stable, to=To, running=Running, oldvstruct=Old}) ->
    RunningList = rafter_voting:to_list(Running),
    YesVotes = dict:from_list([{S, yes} || S <- RunningList]),
    case rafter_voting:quorum(Old, YesVotes) of
        false ->
            OldList = rafter_voting:to_list(Old),
            NodeToStart = oneof(lists:subtract(OldList, RunningList)),
            {call, rafter, start_node, [NodeToStart]};
        true ->
            frequency([{100, {call, rafter, op, [To, command()]}},
                    {1, {call, rafter, stop_node, [oneof(RunningList)]}}])
    end.

precondition(#model_state{state=init}, _) ->
    true;
precondition(#model_state{running=undefined}, {call, rafter, _, _}) ->
    false;
precondition(#model_state{running=Running}, {call, rafter, _, [To]}) ->
    rafter_voting:member(To, Running);
precondition(#model_state{running=Running}, {call, rafter, op, [To, _]}) ->
    rafter_voting:member(To, Running);
precondition(#model_state{running=Running}, {call, rafter, set_config, [To, _]}) ->
    rafter_voting:member(To, Running).

next_state(#model_state{state=init}=S, _,
    {call, ?MODULE, start_nodes, [Running]}) ->
        Leader = lists:nth(1, rafter_voting:to_list(Running)),
        S#model_state{state=blank, running=Running, to=Leader, leader=Leader};

%% The initial config is always just the running servers
next_state(#model_state{state=blank, to=To, running=Running}=S,
    _Result, {call, rafter, set_config, [To, Running]}) ->
        S#model_state{commit_index=1, state=stable, oldvstruct=Running};

next_state(#model_state{state=stable, commit_index=CI, leader=Leader,
  last_committed_op=LastOp}=S, Result, {call, rafter, op, [_, Op]}) ->
    S#model_state{commit_index={call, ?MODULE, maybe_increment, [CI, Result]},
                  leader={call, ?MODULE, maybe_change_leader, [Leader, Result]},
                  last_committed_op={call, ?MODULE, maybe_change_last_op, [LastOp, Op, Result]}};

next_state(#model_state{state=stable, to=To, running=Running}=S,
    _Result, {call, rafter, stop_node, [Node]}) ->
        NewRunningList = lists:delete(Node, rafter_voting:to_list(Running)),
        NewRunning = vstruct(NewRunningList),
        case To of
            Node ->
                S#model_state{leader=unknown,
                              running=NewRunning,
                              to=lists:nth(1, NewRunningList)};
            _ ->
                S#model_state{running=NewRunning}
        end.


postcondition(#model_state{state=init}, {call, ?MODULE, start_nodes, _}, _) ->
    true;
postcondition(#model_state{state=blank},
  {call, rafter, set_config, [_To, _Servers]}, {ok, _}) ->
    true;

postcondition(#model_state{state=stable, oldvstruct=Servers, to=To},
  {call, rafter, op, [To, _]}, {ok, _}) ->
    true =:= rafter_voting:member(To, Servers);
postcondition(#model_state{state=stable, to=To}, {call, rafter, op, [To, _]},
    {error, {redirect, Leader}}) ->
        Leader =/= To;
postcondition(#model_state{state=stable, to=To}, {call, rafter, op, [To, _]},
    {error, election_in_progress}) ->
        true;

postcondition(#model_state{state=stable},
    {call, rafter, stop_node, [_Node]}, ok) ->
        true.

invariant(ModelState=#model_state{to=To, state=stable}) ->
    {_, State} = sys:get_state(To),
    commit_indexes_monotonic(ModelState, State) andalso
    committed_entry_exists_in_log(ModelState, To);
invariant(_) ->
    true.

%% ====================================================================
%% Helper Functions
%% ====================================================================

maybe_increment(CommitIndex, {ok, _}) ->
    CommitIndex + 1;
maybe_increment(CommitIndex, {error, _}) ->
    CommitIndex.

maybe_change_leader(Leader, {ok, _}) ->
    Leader;
maybe_change_leader(_Leader, {error, {redirect, NewLeader}}) ->
    NewLeader;
maybe_change_leader(_Leader, {error, _}) ->
    unknown.

maybe_change_last_op(_, Op, {ok, _}) ->
    Op;
maybe_change_last_op(CurrentLastOp, _, {error, _}) ->
    CurrentLastOp.

%% ====================================================================
%% Invariants
%% ====================================================================

commit_indexes_monotonic(#model_state{commit_index=CI}, State) ->
    State#state.commit_index =< CI.

committed_entry_exists_in_log(#model_state{commit_index=0}, _) ->
    true;
committed_entry_exists_in_log(#model_state{leader=unknown}, _) ->
    true;
committed_entry_exists_in_log(#model_state{commit_index=CI,
                                           last_committed_op=Op}, To) ->
   %% TODO: Make this search backwards for much greater efficiency
   {ok, #rafter_entry{type=Type, index=CommitIndex, cmd=Cmd}} = rafter:get_entry(To, CI),
   case Type of
       config ->
           true;
       op ->
           CommitIndex =:= CI andalso Cmd =:= Op
   end.

%% ====================================================================
%% EQC Generators
%% ====================================================================

%% Commands for a hypothetical backend. Tested with rafter_sm_echo backend.
%% This module is here to test consensus, not the operational capabilities
%% of the backend.
command() ->
    oneof(["inc key val", "get key", "set key val", "keyspace", "config"]).

server() ->
    oneof([a,b,c,d,e,f,g,h,i]).

vsgen() ->
    oneof([{rafter_voting_majority, majority},
           {rafter_voting_grid, grid}]).

vstruct(Peers) ->
    ?LET({Mod, Fun}, vsgen(),
         apply(Mod, Fun, [Peers])).

vstruct() ->
    vstruct(servers()).

servers() ->
    ?SUCHTHAT(Servers, oneof([three_servers(), five_servers(), seven_servers()]),
       begin
            Uniques = sets:to_list(sets:from_list(Servers)),
            length(Uniques) =:= length(Servers)
       end).

three_servers() ->
    vector(3, server()).

five_servers() ->
    vector(5, server()).

seven_servers() ->
    vector(7, server()).

-endif.
