-module(acceptor).
-behaviour(gen_server).
-include_lib("acceptor_state.hrl").

%% API
-export([start_link/0, stop/0, prepare/1, vote/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid::pid()}
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%%--------------------------------------------------------------------
stop() ->
  gen_server:cast(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc Request that the acceptor promises not to vote on older rounds
%% @end
%% TODO: spec
%%--------------------------------------------------------------------
prepare(Round) ->
  gen_server:cast(?MODULE, {prepare, Round}).

%%--------------------------------------------------------------------
%% @doc Request that the acceptor votes for the proposal
%% @end
%% TODO: spec
%%--------------------------------------------------------------------
vote(Round, Value) ->
  gen_server:cast(?MODULE, {vote, Round, Value}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) -> {ok, #state{}}.

handle_call({prepare, Round}, _From, State) ->
  handle_prepare(Round, State);
handle_call({accept, Round, Value}, _From, State) ->
  handle_accept(Round, Value, State).

handle_cast(stop, State) -> {stop, normal, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_prepare(Round, State) ->
  MaxPromised = max(Round, State#state.promised),
  NewState = State#state{promised = MaxPromised},
  {reply, {promised, MaxPromised}, NewState}.

handle_accept(Round, Value, State) when Round >= State#state.promised ->
  {reply, {accept, Round}, State#state{accepted=Value}};
handle_accept(Round, _, State) -> 
  {reply, {reject, Round}, State}.
