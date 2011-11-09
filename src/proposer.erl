-module(proposer).
-behaviour(gen_fsm).
-include_lib("proposer_state.hrl").
%% API
-export([
	 start_link/2,
	 promised/2
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

promised(Proposer, AcceptorReply) ->
    gen_fsm:send_event(Proposer, AcceptorReply).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([Round, Value]) ->
    State = #state{round=Round, value=Value},

    % broadcast to acceptors 
    acceptors:send_promise_request(Round),

    {ok, awaiting_promises, State}.

awaiting_promises({promised, Round, _AcceptedValue}, State) 
  when Round > State#state.round ->
    
    % restart with Round+1 
    NewState = State#state{round = Round+1,
			   promises = 0},
    
    % add exponential backoff
    acceptors:send_promise_request(Round),
    
    {next_state, awaiting_promises, NewState};
awaiting_promises({promised, Round, {Round, VotedValue}}, 
		  #state{round=Round}=State) ->
    NewState = State#state{promises = State#state.promises + 1,
			   value = VotedValue},
    move_state_on_majority(NewState);
awaiting_promises({promised, Round, _AcceptedValue}, 
		  #state{round=Round}=State) -> 
    % collect promise
    NewState = State#state{promises = State#state.promises + 1},
    move_state_on_majority(NewState);
awaiting_promises(_Event, State) ->
    {next_state, awaiting_promises, State}.


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

%%%===================================================================
%%% Internal functions
%%%===================================================================

move_state_on_majority(#state{promises = Promises}=State) 
  when Promises >= ?MAJORITY ->
    acceptors:send_accept_request(State#state.round, State#state.value),
    {next_state, awaiting_accepts, State};
move_state_on_majority(State) ->
    {next_state, awaiting_promises, State}.
