-module(proposer).
-behaviour(gen_fsm).
-include_lib("proposer_state.hrl").
-include_lib("accepted_record.hrl").

%% API
-export([
    start_link/2,
    deliver_promise/2,
    deliver_accept/2,
    propose/1
    ]).

%% gen_fsm callbacks
-export([init/1, 
	 awaiting_promises/2,
	 awaiting_accepts/2,
	 accepted/2
	]).


-export([handle_event/3,
	 handle_sync_event/4, 
	 handle_info/3, 
	 terminate/3, 
	 code_change/4]).

-define(SERVER, ?MODULE).
-define(MAJORITY, 3).


%%%===================================================================
%%% API
%%%===================================================================

%% begins an election where the proposer will seek
%% concensus on a value, proposing Proposal if no
%% other value has already been accepted by a majority
propose(Proposal) ->
    % TODO: is start_link appropriate? what about proc failures?
    ?MODULE:start_link(Proposal, self()).

start_link(Proposal, ReplyPid) ->
    gen_fsm:start_link(?MODULE, [Proposal, ReplyPid], []).

deliver_promise(Proposer, AcceptorReply) ->
    gen_fsm:send_event(Proposer, AcceptorReply).

deliver_accept(Proposer, AcceptorReply) ->
    gen_fsm:send_event(Proposer, AcceptorReply).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([Proposal, ReplyPid]) ->
    Round = 1,
    acceptors:send_promise_requests(self(), Round),
    {ok, awaiting_promises, #state{
        round = Round, 
        value=#proposal{value = Proposal},
        reply_to = ReplyPid
    }}.

% on discovering a higher round has been promised
awaiting_promises({promised, PromisedRound, _}, State) 
    when PromisedRound > State#state.round -> % restart with Round+1 
    NextRound = PromisedRound + 1,
    NewState = State#state{round = NextRound, promises = 0},
    % TODO: add exponential backoff
    acceptors:send_promise_requests(self(), NextRound),
    {next_state, awaiting_promises, NewState};
        
% on receiving a promise without past-vote data
awaiting_promises({promised, PromisedRound, no_value}, #state{round = PromisedRound}=State) ->
    loop_until_promise_quorum(State#state{promises = State#state.promises + 1});

% on receiving a promise with accompanying previous-vote data
awaiting_promises( {promised, PromisedRound, {AcceptedRound, AcceptedValue}}, 
    #state{round=PromisedRound}=State) -> 
    NewState = State#state{ 
        value = case AcceptedRound > State#state.value#proposal.accepted_in_round of
            true -> #proposal{accepted_in_round = AcceptedRound, value=AcceptedValue} ;
            false -> State#state.value
        end,
        promises = State#state.promises + 1
    },
    loop_until_promise_quorum(NewState);

% on receiving unknown message
awaiting_promises(_, State) ->
    {next_state, awaiting_promises, State}.

% majority reached
loop_until_promise_quorum(#state{promises = ?MAJORITY}=State) ->
    Proposal = State#state.value#proposal.value,
    acceptors:send_accept_requests(self(), State#state.round, Proposal),
    {next_state, awaiting_accepts, State};

loop_until_promise_quorum(State) -> % keep waiting
    {next_state, awaiting_promises, State}.



awaiting_accepts({rejected, Round}, #state{round = Round, rejects = 2}=State) ->
    RetryRound = Round + 2,
    acceptors:send_promise_requests(self(), RetryRound),
    {next_state, awaiting_promises, State#state{round = RetryRound, promises = 0}};

awaiting_accepts({rejected, Round}, #state{round=Round}=State) ->
    {next_state, awaiting_accepts, State#state{rejects = State#state.rejects + 1}};

awaiting_accepts({accepted, Round, _Value}, #state{round=Round}=State) ->
    io:format("Got accept ~n", []),
    NewState = State#state{accepts = State#state.accepts + 1},
    case NewState#state.accepts >= ?MAJORITY of
    false ->
        {next_state, awaiting_accepts, NewState};
    true ->
        % deliver result to coordinator
        LearnedValue = State#state.value#proposal.value,
            io:format("Reached majority ~n", []),
        NewState#state.reply_to ! {learned, LearnedValue},
        {stop, learned, NewState}
    end;

awaiting_accepts(_, State) ->
    {next_state, awaiting_accepts, State}.


%% OTP Boilerplate
accepted(_, State) -> {next_state, accepted, State}.
handle_event(_Event, StateName, State) -> {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.
handle_info(_Info, StateName, State) -> {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) -> ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.
