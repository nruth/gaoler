-module(learner).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(MAJORITY, 3).
-include_lib("decided_record.hrl").
-include_lib("learner_state.hrl").

-export([get/0, await_result/1, register_callback/0]).

start_link() ->
    gen_server:start_link({local, learner}, ?MODULE, [], []).

%% Sends a blocking value query to the local learner 
%% returns dontknow on timeout, i.e. assumes value was not learned
get() ->
    % only query the local node's registered learner;
    % sidesteps consensus/asynch issues with remotes
    gen_server:call(learner, get_learned).

%% Register with local learner for callback on next decision of value
%%  * Blocks until registration confirmed
register_callback() ->
    gen_server:call(learner, register_callback).

%% Blocks awaiting result callback from a learner
%%  * returns timeout when Timeout exceded
await_result(Timeout) ->
    receive
        {result, _Value}=Result -> Result
    after
        Timeout -> timeout
    end.

% starts with empty data store
init([]) -> {ok, #learner{}}.

handle_call(get_learned, _From, State) ->
    handle_get_learned(State);
handle_call(register_callback, From, State) ->
    {reply, registered, State#learner{callbacks=[From|State#learner.callbacks]}}.

handle_cast({result, Value}, State) -> 
    handle_result_notice(Value, State);
handle_cast({accepted, Round, Value}, State) -> 
    handle_accepted_notice(Round, Value, State);
handle_cast(stop, State) -> 
    {stop, normal, State}.


%%%===================================================================
%%% Internals
%%%===================================================================

learn_value_and_update_state(Value, State) ->
    learners:broadcast_result(Value),
    send_callbacks(Value, State#learner.callbacks),
    % discard all callbacks, which have now been sent, 
    % and discard vote counts, which has now been decided
    State#learner{
        accepts = [],
        learned = #decided{value = Value},
        callbacks = []
    }.

send_callbacks(Value, RegisteredCallbacks) ->
    [Pid ! {result, Value} || Pid <- RegisteredCallbacks],
    ok.

%% responds to value queries
handle_get_learned(State) -> 
    case State of 
    #learner{learned=#decided{value=Value}} -> 
        {reply, {learned, Value}, State};
    _else -> 
        {reply, unknown, State}
    end.

handle_result_notice(Value, State) ->
    case State of
        #learner{learned=#decided{}} ->
            % already decided and rebroadcast, do nothing
            {ok, State};
        _ -> % short-circuit to decided value when notified of a result
            {ok, learn_value_and_update_state(Value, State)}
    end.

handle_accepted_notice(_Round, _Value, #learner{learned=#decided{}}=State) ->
    % already decided, so discard msgs
    {ok, State};
handle_accepted_notice(Round, Value, #learner{accepts=Accepted}=State) ->
    % counting accept votes
    Key = {Round, Value},
    NewState = case lists:keyfind(Key, 1, Accepted) of
        {Key, ?MAJORITY - 1} ->
            learn_value_and_update_state(Value, State);
        {Key, AcceptedCount} ->
            NewAccepted = lists:keyreplace(Key, 1, Accepted, {Key, AcceptedCount + 1}),
            State#learner{accepts=NewAccepted};
        false -> 
            NewAccepted = [{Key, 1}  | Accepted],
            State#learner{accepts=NewAccepted}
    end,
    {ok, NewState}.

%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
