-module(learner_test).
-include_lib("eunit/include/eunit.hrl").

% %% TestCase: awaiting_accepts state
% awaiting_accepts_test_() ->
%     {foreach, 
%      fun setup/0, 
%      fun teardown/1,
%      [
%       fun first_accept_received/0,
%       fun on_accept_quorum_proposer_delivers_value/0,
%       fun on_accept_quorum_state_moves_to_accepted/0
%      ]
%     }.

-define (STATE (Round, Value, AcceptCount), [{{Round, Value}, AcceptCount}]).

first_accept_received_for_round_value_pair_counts_1_test() ->
    Round = 100, Value = foo, AcceptCount = 0,
    InitialState = [],
    Result = learner:handle_event({accepted, Round, Value}, InitialState),
    ?assertEqual({ok, ?STATE(Round, Value, AcceptCount + 1)}, Result).

accept_received_for_known_value_increases_count_test() ->
    Round = 1, Value = foo, AcceptCount = 1,
    InitialState = ?STATE(Round, Value, AcceptCount),
    Result = learner:handle_event({accepted, Round, Value}, InitialState),
    ?assertEqual({ok, ?STATE(Round, Value, AcceptCount + 1)}, Result).

on_accept_quorum_proposer_delivers_value() ->
    Round = 20, Value = foo, AcceptCount = 2,
    meck:new(learners),
    meck:expect(learners, broadcast_result, 1, ok),
    InitialState = ?STATE(Round, Value, AcceptCount),
    Result = learner:handle_event({accepted, Round, Value}, InitialState),
    ?assert(meck:called(learners, broadcast_result, [Value])).


%     Round = 1,
%     InitialState = #state{},
%     Result = learner:handle_event({accepted, Round, foo}, InitialState),
%     ?assertMatch({ok, #state{accepts=1, value=foo}}, Result).

% 
% on_accept_quorum_state_moves_to_accepted() ->
%     Round = 1,
%     Value = 1,
%     InitialState = ?PROMISES(3)?ACCEPTS(2)?ROUND(Round)?VALUE(Value),
%     Result = {_, _, AcceptedState} = 
%   proposer:awaiting_accepts({accepted, Round}, InitialState),
%     ?assertMatch({next_state, accepted, _}, Result),
%     ?assertEqual(InitialState?ACCEPTS(3), AcceptedState).
