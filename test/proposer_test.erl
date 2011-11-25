-module(proposer_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("proposer_state.hrl").
-include_lib("accepted_record.hrl").

-define(PROMISES(N),#state{promises=N}).
-define(ACCEPTS(N), #state{accepts=N}).
-define(ROUND(N),   #state{round=N}).
-define(VALUE(V),   #state{value=#proposal{value=V}}).


%%% =============================
%%% Startup tests
%%% =============================
startup_behaviour_test_() -> {foreach, fun setup/0, fun teardown/1, [
    fun awaiting_promises/0,
    fun starts_with_round_1/0,
    fun starts_with_0_promises/0,
    fun remembers_proposal/0,
    fun remembers_reply_to_pid/0,
    fun should_broadcast_prepare_request/0
]}.

    awaiting_promises() ->
        ?assertMatch({ok, awaiting_promises, _}, proposer:init([foo, self()])).

    starts_with_round_1() ->
        ?assertMatch({ok, _, #state{round=1}}, proposer:init([foo, self()])).

    starts_with_0_promises() ->
        ?assertMatch({ok, _, #state{promises=0}}, proposer:init([foo, self()])).

    remembers_proposal() ->
        Proposal = foo,
        {ok, _, State} = proposer:init([Proposal, self()]),
        Value = State#state.value#proposal.value,
        ?assertEqual(Proposal, Value).

    remembers_reply_to_pid() ->
        ReplyTo = self(),
        ?assertMatch({ok, _, #state{reply_to=ReplyTo}}, proposer:init([foo, ReplyTo])).

    should_broadcast_prepare_request() ->
        proposer:init([val, self()]),
        ?assert(meck:called(acceptors, send_promise_requests, [self(), _Round=1])).


%%% =============================
%%% Awaiting Promises state tests
%%% =============================

awaiting_promises_higher_promise_seen_test_() -> {foreach, fun setup/0, fun teardown/1, [
    fun should_restart_prepare_with_higher_round_when_higher_round_promise_seen/0
]}.

    should_restart_prepare_with_higher_round_when_higher_round_promise_seen() ->
        InitialState = #state{round = 1, value = #proposal{value = foo}},
        ReceivedRound = InitialState#state.round + 1,
        Result = proposer:awaiting_promises({promised, ReceivedRound, bar}, InitialState),
        {next_state, awaiting_promises, NewState=#state{}} = Result,
        ?assertEqual(ReceivedRound + 1, NewState#state.round),
        ?assertEqual(0, NewState#state.promises).

awaiting_promises_count_promise_and_check_proposal_vs_past_accepts_test_() -> [
    fun should_count_promises_for_same_round/0,
    fun should_not_count_promise_for_lower_round/0,
    fun should_ignore_proposal_value_sent_with_promise_when_round_lower/0,
    fun should_adopt_proposal_value_sent_with_promise_when_round_higher/0
].

    should_count_promises_for_same_round() -> 
        InitialState = #state{round = 10, promises = 1},
        Result = proposer:awaiting_promises(
            {promised, InitialState#state.round, no_value}, InitialState),
        {next_state, awaiting_promises, NewState=#state{}} = Result,
        ?assertEqual(InitialState#state.promises + 1, NewState#state.promises).

    %TODO property-testing candidate ?
    should_not_count_promise_for_lower_round() -> 
        InitialState = #state{round = 10, promises = 1},
        Result = proposer:awaiting_promises(
            {promised, InitialState#state.round - 1, no_value}, InitialState),
        {next_state, awaiting_promises, NewState=#state{}} = Result,
        ?assertEqual(InitialState#state.promises + 0, NewState#state.promises).

    should_ignore_proposal_value_sent_with_promise_when_round_lower() -> 
        ReceivedRound = 2, ReceivedValue = bar,
        InitialState = #state{round = 4, value = #proposal{value = foo}},
        Result = proposer:awaiting_promises({promised, ReceivedRound, ReceivedValue}, InitialState),
        {next_state, awaiting_promises, NewState=#state{}} = Result,
        ?assertEqual(InitialState#state.value, NewState#state.value).

    should_adopt_proposal_value_sent_with_promise_when_round_higher() ->
        InitialState = #state{round = 10, value = #proposal{value = foo}},
        {next_state, awaiting_promises, NewState} = proposer:awaiting_promises(
            {promised, InitialState#state.round, {4, bar}}, InitialState
        ),
        ?assertMatch(
            #state{ 
                value = #proposal{
                    accepted_in_round = 4, 
                    value = bar
                }
            }, NewState).


awaiting_promises_prepare_quorum_test_() -> {foreach, fun setup/0, fun teardown/1, [
    fun should_transition_to_awaiting_accepts_when_2_promises_and_promise_for_round_received/0,
    fun should_broadcast_accept_request_on_promise_quorum/0
]}.

    should_transition_to_awaiting_accepts_when_2_promises_and_promise_for_round_received() ->
        Proposal = #proposal{value = v},
        InitialState = #state{round = 10, promises = 2, value = Proposal},
        ?assertMatch(
            {next_state, awaiting_accepts, #state{
                round = 10,
                accepts = 0,
                rejects = 0,
                value = Proposal
            }},
            proposer:awaiting_promises(
                {promised, InitialState#state.round, no_value}, InitialState
            )
        ).

    should_broadcast_accept_request_on_promise_quorum() ->
        Round = 1,
        InitialState = ?PROMISES(2)?ROUND(Round)?VALUE(foo),
        proposer:awaiting_promises({promised, Round, no_value},
                     InitialState),
        ?assert(meck:called(acceptors, send_accept_requests, '_')).



%%% =============================
%%% Awaiting Accepts state tests
%%% =============================

awaiting_accepts_count_accept_test_() -> {foreach, fun setup/0, fun teardown/1, [
    fun should_increment_accepts_when_accept_for_round_received/0,
    fun should_not_increment_accepts_when_accept_for_another_round_received/0
]}.

    should_increment_accepts_when_accept_for_round_received() ->
        {next_state, awaiting_accepts, #state{round = 10, accepts = Accepts}} = 
            proposer:awaiting_accepts({accepted, 10}, #state{round = 10}),
        ?assertEqual(1, Accepts).

    should_not_increment_accepts_when_accept_for_another_round_received() ->
        {next_state, awaiting_accepts, #state{round = 10, accepts = Accepts}} = 
            proposer:awaiting_accepts({accepted, 8}, #state{round = 10}),
        ?assertEqual(0, Accepts).

awaiting_accepts_count_reject_test_() -> {foreach, fun setup/0, fun teardown/1, [
    fun should_increment_rejects_when_reject_for_round_received/0,
    fun should_not_increment_rejects_when_reject_for_another_round_received/0
]}.

    should_increment_rejects_when_reject_for_round_received() ->
        {next_state, awaiting_accepts, #state{round = 10, rejects = Rejects}} = 
            proposer:awaiting_accepts({rejected, 10}, #state{round = 10}),
        ?assertEqual(1, Rejects).

    should_not_increment_rejects_when_reject_for_another_round_received() ->
        {next_state, awaiting_accepts, #state{round = 10, rejects = Rejects}} = 
            proposer:awaiting_accepts({rejected, 8}, #state{round = 10}),
        ?assertEqual(0, Rejects).

awaiting_accepts_reject_quorum_test_() -> {foreach, fun setup/0, fun teardown/1, [
    fun should_transition_to_prepare_if_reject_quorum_reached/0,
    fun should_broadcast_new_prepare_request_on_reject_quorum/0
]}.

    should_transition_to_prepare_if_reject_quorum_reached() ->
        Proposal = #proposal{value = v},
        Round = 10,
        ?assertMatch(
            {next_state, awaiting_promises, #state{
                round = 12,
                promises = 0,
                value = Proposal
            }},
            proposer:awaiting_accepts(
                {rejected, Round}, #state{rejects = 2, round = Round, value = Proposal}
            )
        ).

    should_broadcast_new_prepare_request_on_reject_quorum() ->
        proposer:awaiting_accepts({rejected, 10}, #state{rejects = 2, round = 10}),
        ?assert(meck:called(acceptors, send_promise_requests, [self(), _Round=12])).


awaiting_accepts_accept_quorum_test_() -> {foreach, fun setup/0, fun teardown/1, [
    fun should_halt_when_accept_quorum_reached/0,
    fun should_broadcast_result_notification_to_learners_on_accept_quorum/0,
    fun should_notify_client_of_result/0
]}.

should_halt_when_accept_quorum_reached() ->
    Proposal = #proposal{value = v},
    ?assertMatch(
        {stop, learned, _},
        proposer:awaiting_accepts(
            {accepted, 10}, #state{accepts = 2, round = 10, value = Proposal, reply_to=self()}
        )
    ).

should_broadcast_result_notification_to_learners_on_accept_quorum() ->
    Proposal = #proposal{value = v},
    proposer:awaiting_accepts(
        {accepted, 10}, #state{accepts = 2, round = 10, value = Proposal, reply_to=self()}
    ),
    ?assert(meck:called(learners, broadcast_result, [v])).


should_notify_client_of_result() ->
    Proposal = #proposal{value = v},
    ReplyTo = nspy:mock(),
    proposer:awaiting_accepts(
        {accepted, 10}, #state{accepts = 2, round = 10, value = Proposal, reply_to=ReplyTo}
    ),
    timer:sleep(2),
    nspy:assert_message_received(ReplyTo, {learned, v}).

%%% =============================
%%% Test Helpers
%%% =============================

%% Meck stub modules, used to enable decoupled unit tests
setup() ->
    Mods = [acceptors, gaoler, learners],
    meck:new(Mods),
    meck:expect(gaoler, deliver, 1, ok),
    meck:expect(acceptors, send_accept_requests, 3, ok),
    meck:expect(acceptors, send_promise_requests, 2, ok),
    meck:expect(learners, broadcast_result, 1, ok),    
    Mods.

teardown(Mods) ->
    meck:unload(Mods).
