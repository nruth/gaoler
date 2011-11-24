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
handle_prepare(Round, State) ->
    HighestPromise = max(Round, State#state.promised),
    NewState = State#state{promised = HighestPromise},
    persister:remember_promise(HighestPromise),
    {reply, {promised, HighestPromise, NewState#state.accepted}, NewState}.

handle_accept(Round, Value, State) when Round >= State#state.promised ->
    persister:remember_vote(Round, Value),
    {reply, {accepted, Round, Value}, State#state{accepted={Round, Value}}};
handle_accept(Round, _, State) -> 
    {reply, {reject, Round}, State}.

init([]) -> 
    StartState = persister:load_saved_state(),
    {ok, StartState}.

% gen_server callback
handle_call({prepare, Round}, _From, State) ->
    handle_prepare(Round, State);
handle_call({accept, Round, Value}, _From, State) ->
    handle_accept(Round, Value, State).

%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_cast(stop, State) -> {stop, normal, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
