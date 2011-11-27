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
  gen_server:call(Acceptor, {prepare, Round}).

%%--------------------------------------------------------------------
%% @doc Request that the acceptor votes for the proposal
%%--------------------------------------------------------------------
accept(Acceptor, Round, Value) ->
  gen_server:call(Acceptor, {accept, Round, Value}).

%%%===================================================================
%%% Implementation
%%%===================================================================
init([]) -> {ok, #state{}}.

% gen_server callback
handle_call({prepare, Round}, _From, State) ->
    handle_prepare({1, Round}, State);
handle_call({prepare, ElectionId, Round}, _From, State) ->
    handle_prepare({ElectionId, Round}, State);
handle_call({accept, Round, Value}, _From, State) ->
    handle_accept({1, Round}, Value, State);
handle_call({accept, {ElectionId, Round}, Value}, _From, State) ->
    handle_accept({ElectionId, Round}, Value, State).


handle_prepare({ElectionId, Round}, State) ->
    case lists:keyfind(ElectionId, 1, State#state.elections) of
        {ElectionId, FoundElection} ->
            HighestPromise = max(Round, FoundElection#election.promised),
            NewElection = FoundElection#election{promised = HighestPromise},
            NewState = update_election(ElectionId, NewElection, State),
            Reply = {promised, HighestPromise, NewElection#election.accepted},
            {reply, Reply, NewState};
        false ->
            NewElection = #election{promised = Round},
            NewState = add_new_election(ElectionId, NewElection, State),
            {reply, {promised, Round, NewElection#election.accepted}, NewState}
    end.    

handle_accept({ElectionId, Round}, Value, State) ->
    {Reply, NextState} = 
        case lists:keyfind(ElectionId, 1, State#state.elections) of
            {ElectionId, _ElectionRecord}=Election ->
                handle_accept_for_election(Round, Value, Election, State);
            false ->
                NewElection = #election{promised = Round,
                                        accepted = {Round, Value}},
                NewState = add_new_election(ElectionId, NewElection, State),
                {{accepted, Round, Value}, NewState}
        end,
    {reply, Reply, NextState}.

handle_accept_for_election(Round, Value, {ElectionId, Election}, State) 
  when Round >= Election#election.promised ->
    NewElection = Election#election{promised = Round, 
                                    accepted = {Round, Value}},
    NewState = update_election(ElectionId, NewElection, State),
    {{accepted, Round, Value}, NewState};
handle_accept_for_election(Round, _Value, _Election, State) ->
    {{reject, Round}, State}.

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
