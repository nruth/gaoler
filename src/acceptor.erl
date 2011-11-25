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
%% handle_prepare(Round, State) ->
%%     HighestPromise = max(Round, State#state.promised),
%%     NewState = State#state{promised = HighestPromise},
%%     {reply, {promised, HighestPromise, NewState#state.accepted}, NewState}.

handle_prepare({ElectionId, Round}, State) ->
    case lists:keyfind(ElectionId, 1, State#state.elections) of
        {ElectionId, _Value}=FoundElection ->
            HighestPromise = max(Round, 
                                 FoundElection#election.promised),
            NewElection = FoundElection#election{promised = HighestPromise},
            NewState = lists:keyreplace(ElectionId, 1, State#state.elections,
                                        {ElectionId, NewElection}),
            {reply, {promised, Round, NewElection#election.accepted}, NewState};
        false ->
            NewElection = #election{promised = Round},
            NewState = add_new_election(ElectionId, NewElection, State),
            {reply, {promised, Round, NewElection#election.accepted}, NewState}
    end.    

%% handle_accept(Round, Value, State) when Round >= State#state.promised ->
%%     {reply, {accepted, Round, Value}, State#state{accepted={Round, Value}}};
%% handle_accept(Round, _, State) -> 
%%     {reply, {reject, Round}, State}.    

handle_accept({ElectionId, Round}, Value, State) ->
    {Reply, NextState} = 
        case lists:keyfind(ElectionId, 1, State#state.elections) of
            {ElectionId, ElectionRecord} ->
                case handle_accept_for_election(Round, Value, ElectionRecord) of
                    {accepted, NewElection} ->
                        NewState = lists:keyreplace(ElectionId, 1, 
                                                    State#state.elections,
                                                    {ElectionId, NewElection}),
                        {{accepted, Round, Value}, NewState};
                    reject ->
                        {{reject, Round}, State}
                end;
            false ->
                NewElection = #election{promised = Round,
                                        accepted = {Round, Value}},
                NewState = add_new_election(ElectionId, NewElection, State),
                {{accepted, Round, Value}, NewState}
        end,
    {reply, Reply, NextState}.

handle_accept_for_election(Round, Value, Election) 
  when Round >= Election#election.promised ->
    NewElection = Election#election{promised = Round, 
                                    accepted = {Round, Value}},
    {accepted, NewElection};
handle_accept_for_election(_, _, _) ->
    reject.

add_new_election(ElectionId, NewElection, State) ->
    State#state{elections = [{ElectionId, NewElection}|
                             State#state.elections]}.

init([]) -> {ok, #state{}}.

% gen_server callback
handle_call({prepare, Round}, _From, State) ->
  handle_prepare({1, Round}, State);
handle_call({accept, Round, Value}, _From, State) ->
  handle_accept({1, Round}, Value, State).

%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_cast(stop, State) -> {stop, normal, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
