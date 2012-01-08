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
prepare(Acceptor, {Election,Round}) ->
  gen_server:call(Acceptor, {prepare, {Election, Round}}).

%%--------------------------------------------------------------------
%% @doc Request that the acceptor votes for the proposal
%%--------------------------------------------------------------------
accept(Acceptor, {Election,Round}, Value) ->
  gen_server:call(Acceptor, {accept, {Election, Round}, Value}).

%%%===================================================================
%%% Implementation
%%%===================================================================
init([]) -> 
%    StartState = persister:load_saved_state(),
    Store = statestore:create(),
    StartState = #state{elections = Store},
    {ok, StartState}.

% gen_server callback
handle_call({prepare, {ElectionId, Round}}, _From, State) ->
    handle_prepare({ElectionId, Round}, State);
handle_call({accept, {ElectionId, Round}, Value}, _From, State) ->
    handle_accept({ElectionId, Round}, Value, State).

handle_cast({gc_older_than, ElectionId}, State) ->
    {noreply, garbage_collect_elections_older_than(ElectionId, State)};
handle_cast(stop, State) -> {stop, normal, State}.

%%%===================================================================
%%% Prepare requests
%%%=================================================================== 
handle_prepare({ElectionId, Round}, State) ->
    case statestore:find(ElectionId) of
        {ElectionId, FoundElection} ->
            handle_prepare_for_existing_election(ElectionId, Round, FoundElection, State);
        false ->
            create_new_election_from_prepare_request(ElectionId, Round, State)
    end.    

handle_prepare_for_existing_election(_ElectionId, Round, FoundElection, State) ->
    HighestPromise = max(Round, FoundElection#election.promised),
    NewElection = FoundElection#election{promised = HighestPromise},
    update_election(NewElection),
    Reply = {promised, HighestPromise, NewElection#election.accepted},
    %persister:remember_promise(ElectionId, NewElection#election.promised),
    {reply, Reply, State}.

create_new_election_from_prepare_request(ElectionId, Round, State) ->
    NewElection = #election{id = ElectionId, promised = Round},
    add_new_election(NewElection),
    %persister:remember_promise(ElectionId, Round),
    {reply, {promised, Round, NewElection#election.accepted}, State}.

%%%===================================================================
%%% Accept requests
%%%=================================================================== 
handle_accept({ElectionId, Round}, Value, State) ->
    {Reply, NextState} = 
        case statestore:find(ElectionId) of
            {ElectionId, _ElectionRecord}=Election ->
                handle_accept_for_election(Round, Value, Election, State);
            false ->
                create_new_election_from_accept_request(ElectionId, Round, Value, State)
        end,
    {reply, Reply, NextState}.

handle_accept_for_election(Round, Value, {ElectionId, Election}, State) 
  when Round >= Election#election.promised ->
    NewElection = Election#election{id = ElectionId, 
                                    promised = Round, 
                                    accepted = {Round, Value}},
    update_election(NewElection),
    %persister:remember_vote(ElectionId, Round, Value),
    {{accepted, Round, Value}, State};
handle_accept_for_election(Round, _Value, _Election, State) ->
    {{reject, Round}, State}.

create_new_election_from_accept_request(ElectionId, Round, Value, State) ->
    NewElection = #election{id = ElectionId, 
                            promised = Round,
                            accepted = {Round, Value}},
    add_new_election(NewElection),
    %persister:remember_vote(ElectionId, Round, Value),
    {{accepted, Round, Value}, State}.


%%%===================================================================
%%% Internal functions
%%%=================================================================== 
add_new_election(NewElection) ->
    statestore:add(NewElection).

update_election(NewElection) ->
    statestore:replace(NewElection).

garbage_collect_elections_older_than(OldestNeededElectionId, State) ->
    NeededElectionPredicate = fun({ElectionId, _}) -> 
        ElectionId >= OldestNeededElectionId
    end,
    UpdatedElections = lists:takewhile(NeededElectionPredicate, State#state.elections),
    State#state{elections = UpdatedElections, oldest_remembered_state = OldestNeededElectionId}.

%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
