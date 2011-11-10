-module(proposer).
-behaviour(gen_fsm).
-include_lib("proposer_state.hrl").
-include_lib("accepted_record.hrl").

%% API
-export([
	 start_link/2,
	 deliver_promise/2,
         deliver_accept/2
	]).

%% gen_fsm callbacks
-export([init/1, 
	 awaiting_promises/2,
	 awaiting_accepts/2,
	 accepted/2,
	 aborted/2
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

start_link(Round, Value) ->
    gen_fsm:start_link(?MODULE, [Round, Value], []).

deliver_promise(Proposer, AcceptorReply) ->
    gen_fsm:send_event(Proposer, AcceptorReply).

deliver_accept(Proposer, AcceptorReply) ->
    gen_fsm:send_event(Proposer, AcceptorReply).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([Round, Value]) ->
    acceptors:send_promise_request(self(), Round),
    {ok, awaiting_promises, #state{round=Round, value=Value}}.

% on discovering a higher round has been promised
awaiting_promises({promised, Round, _Accepted}, State) 
    when Round > State#state.round -> % restart with Round+1 
    NextRound = Round + 1,
    NewState = State#state{round = NextRound, promises = 0},
    % TODO: add exponential backoff
    acceptors:send_promise_request(self(), NextRound),
    {next_state, awaiting_promises, NewState};
        
% on receiving a promise without past-vote data
awaiting_promises({promised, Round, no_value}, #state{round=Round}=State) ->
    loop_until_promise_quorum(State#state{promises = State#state.promises + 1});

% on receiving a promise with accompanying previous-vote data
awaiting_promises( {promised, Round, #accepted{}=Accepted}, 
    #state{round=Round}=State) -> 
    loop_until_promise_quorum(
        State#state{ 
            promises = State#state.promises + 1,
            past_accepts = [ Accepted | State#state.past_accepts ]
        }
    );

% on receiving unknown message
awaiting_promises(_, State) ->
    {next_state, awaiting_promises, State}.

loop_until_promise_quorum(#state{promises = ?MAJORITY}=State) -> %quorum
    % if promises carried accepted values: propose the one accepted in the highest round
    % else when no accepted values sent: propose a new value
    ProposalValue = case highest_round_accepted_value(State#state.past_accepts) of 
        #accepted{value=HighestRoundAcceptedValue} -> 
            HighestRoundAcceptedValue;
        no_value -> 
            State#state.value
    end,
    acceptors:send_accept_request(self(), State#state.round, ProposalValue),
    % move to the next state
    {next_state, awaiting_accepts, State};

loop_until_promise_quorum(State) -> % keep waiting
    {next_state, awaiting_promises, State}.

highest_round_accepted_value([]) -> no_value;
highest_round_accepted_value([A]) -> A;
highest_round_accepted_value(PastAccepts) ->
    [HighestValue|_] = lists:sort(
        fun (#accepted{}=A, #accepted{}=B) ->
            A#accepted.round =< B#accepted.round 
        end,
        PastAccepts
    ),
    HighestValue.

awaiting_accepts({accepted, Round}, #state{round=Round}=State) ->
    % collect accept
    NewState = State#state{accepts = State#state.accepts + 1},
    case NewState#state.accepts >= ?MAJORITY of
	false ->
	    {next_state, awaiting_accepts, NewState};
	true ->
	    % deliver result to application (client) 
	    gaoler:deliver(State#state.value),
	    {next_state, accepted, NewState}
    end;
awaiting_accepts(_, State) ->
    {next_state, awaiting_accepts, State}.


%% OTP Boilerplate
accepted(_, State) -> {next_state, accepted, State}.
aborted(_, State) -> {next_state, aborted, State}.
handle_event(_Event, StateName, State) -> {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.
handle_info(_Info, StateName, State) -> {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) -> ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.
