-module(proposer).
-behaviour(gen_fsm).
-include_lib("proposer_state.hrl").
%% API
-export([
	 start_link/2
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

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([Round, Value]) ->
    State = #state{round=Round, value=Value},

    % broadcast to acceptors 
    acceptors:send_promise_request(Round),

    {ok, awaiting_promises, State}.

awaiting_promises({promised, Round}, State) 
  when Round > State#state.round ->
    {next_state, aborted, State};
awaiting_promises({promised, Round}, #state{round=Round, value=Value}=State) -> 
    % collect promise
    NewState = State#state{promises = State#state.promises + 1},

    case NewState#state.promises >= ?MAJORITY of
	false ->
	    {next_state, awaiting_promises, NewState};
	true ->
            acceptors:send_accept_request(Round, Value),
	    {next_state, awaiting_accepts, NewState}
    end;
awaiting_promises(_Event, State) ->
    {next_state, awaiting_promises, State}.

% shouldn't value be in here too?
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

accepted(_, State) ->
    {next_state, accepted, State}.


aborted(_, State) ->
    {next_state, aborted, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================





%% propose(Round, Value) ->
%%     spawn (
%%     Proposal = prepare(Round, Value),
%%     Result = ballot(Round, Proposal),
%%       from ! result ),
%%     receive 
%% 	Result  ->
%% 	    Result;
%%     after ?TIMEOUT ->
%% 	    dead
%%     end.

%% prepare(Acceptors, Round, Value) ->
%%     Message = {prepare, Round},
    
%%     case wait_for_promises(Round, 0, no_previous_value) of
%% 	no_previous_value ->
%% 	    Value;
%% 	PreviousValue ->
%% 	    PreviousValue
%%     end,

%%     {Round, Value}.

%% wait_for_promises(_Round, ?MAJORITY, Votes) -> 
%% % votes = previously accepted values for an acceptor    
%%     Votes;
%% wait_for_promises(Round, NumberOfReplies, PreviousVote) ->
%%     receive 
%% 	{promised, Round, LatestVote} ->
%% 	    NewVote = add_latest_vote(LatestVote, PreviousVote),
%% 	    wait_for_promises(Round, NumberOfReplies+1, NewVote);
%% 	_Other ->
%% 	    wait_for_promises(Round, NumberOfReplies, PreviousVote)
%%     end.
	    

%% ballot(Round, Proposal) ->
%%     % send ballot to acceptors
%%     Ballot = msg,
    
%%     Result = wait_for_ballot(Round, Proposal, 0, no_previous_value),
%%     % wait for quorum
%%     % return 
%%     Result.

%% wait_for_ballot(Round, Proposal, ?MAJORITY, Votes) ->
%%     Votes;
%% wait_for_ballot(Round, Proposal, NumberOfReplies, PreviousVote) ->
%%     ok.


%% broadcast([], _, _) ->
%%     ok;
%% broadcast([Target|TargetGroup], From, Message) ->
%%     Target ! {Message, From},
%%     broadcast(TargetGroup, From, Message).



%% add_latest_vote({Round, _Value}=Vote, {OldRound,_OldValue}=OldVote) ->
%%     case Round > OldRound of
%% 	true ->
%% 	    Vote;
%% 	false ->
%% 	    OldVote
%%     end;
%% add_latest_vote(Vote, no_previous_value) ->
%%     Vote.
