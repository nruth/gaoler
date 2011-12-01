-module(acceptor).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
        terminate/2, code_change/3]).

-include_lib("acceptor_state.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-export([
	 start_link/0, 
	 start_link/1,
	 stop/0, 
	 stop/1,
	 prepare/2, 
	 accept/3
	]).

%%--------------------------------------------------------------------
%% @doc Starts the server.
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop).
stop(Name) ->
    gen_server:cast(Name, stop).

%%--------------------------------------------------------------------
%% @doc Request that the acceptor promises not to vote on older rounds
%%--------------------------------------------------------------------
prepare(Acceptor, Round) ->
  gen_server:call(Acceptor, {prepare, {1, Round}}).

%%--------------------------------------------------------------------
%% @doc Request that the acceptor votes for the proposal
%%--------------------------------------------------------------------
accept(Acceptor, Round, Value) ->
  gen_server:call(Acceptor, {accept, {1, Round}, Value}).

%%%===================================================================
%%% Implementation
%%%===================================================================
init([]) -> 
    StartState = persister:load_saved_state(),
    {ok, StartState}.

% gen_server callback
handle_call({prepare, {ElectionId, Round}}, _From, State) ->
    handle_prepare({ElectionId, Round}, State);
handle_call({accept, {ElectionId, Round}, Value}, _From, State) ->
    handle_accept({ElectionId, Round}, Value, State).

%%%===================================================================
%%% Prepare requests
%%%=================================================================== 
handle_prepare({ElectionId, Round}, State) ->
    case lists:keyfind(ElectionId, 1, State#state.elections) of
        {ElectionId, FoundElection} ->
            handle_prepare_for_existing_election(ElectionId, Round, FoundElection, State);
        false ->
            create_new_election_from_prepare_request(ElectionId, Round, State)
    end.    

handle_prepare_for_existing_election(ElectionId, Round, FoundElection, State) ->
    HighestPromise = max(Round, FoundElection#election.promised),
    NewElection = FoundElection#election{promised = HighestPromise},
    NewState = update_election(ElectionId, NewElection, State),
    Reply = {promised, HighestPromise, NewElection#election.accepted},
    persister:remember_promise(ElectionId, NewElection#election.promised),
    {reply, Reply, NewState}.

create_new_election_from_prepare_request(ElectionId, Round, State) ->
    NewElection = #election{promised = Round},
    NewState = add_new_election(ElectionId, NewElection, State),
    persister:remember_promise(ElectionId, Round),
    {reply, {promised, Round, NewElection#election.accepted}, NewState}.

%%%===================================================================
%%% Accept requests
%%%=================================================================== 
handle_accept({ElectionId, Round}, Value, State) ->
    {Reply, NextState} = 
        case lists:keyfind(ElectionId, 1, State#state.elections) of
            {ElectionId, _ElectionRecord}=Election ->
                handle_accept_for_election(Round, Value, Election, State);
            false ->
                create_new_election_from_accept_request(ElectionId, Round, Value, State)
        end,
    {reply, Reply, NextState}.

handle_accept_for_election(Round, Value, {ElectionId, Election}, State) 
  when Round >= Election#election.promised ->
    NewElection = Election#election{promised = Round, 
                                    accepted = {Round, Value}},
    NewState = update_election(ElectionId, NewElection, State),
    persister:remember_vote(ElectionId, Round, Value),
    {{accepted, Round, Value}, NewState};
handle_accept_for_election(Round, _Value, _Election, State) ->
    {{reject, Round}, State}.

create_new_election_from_accept_request(ElectionId, Round, Value, State) ->
    NewElection = #election{promised = Round,
                            accepted = {Round, Value}},
    NewState = add_new_election(ElectionId, NewElection, State),
    persister:remember_vote(ElectionId, Round, Value),
    {{accepted, Round, Value}, NewState}.


%%%===================================================================
%%% Internal functions
%%%=================================================================== 
add_new_election(ElectionId, NewElection, State) ->
    State#state{elections = [{ElectionId, NewElection}|
                             State#state.elections]}.

update_election(ElectionId, NewElection, State) ->
    UpdatedElections = 
        lists:keyreplace(ElectionId, 1, 
                         State#state.elections,
                         {ElectionId, NewElection}),
    State#state{elections = UpdatedElections}.

%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_cast(stop, State) -> {stop, normal, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
