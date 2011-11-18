-module(learner).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(MAJORITY, 3).
-include_lib("decided_record.hrl").
-include_lib("learner_state.hrl").

-export([get/0, await_result/1]).

%% Sends a blocking value query to the local learner 
%% returns dontknow on timeout, i.e. assumes value was not learned
get() ->
    % only query the local node's registered learner;
    % sidesteps consensus/asynch issues with remotes
    gen_event:call(learner, get_learned).

await_result(Timeout) ->
    receive
        {result, _Value}=Result -> Result
    after
        Timeout -> timeout
    end.

% starts with empty data store
init([]) -> {ok, #learner{}}.

%respond to value queries
handle_call(get_learned, _From, #learner{learned=#decided{value=Value}}=State) ->
    {reply, {learned, Value}, State};
handle_call(get_learned, _From, State) ->
    {reply, dontknow, State}.

handle_cast({register_callback, CallbackPid}, State) ->
    {ok, State#learner{callbacks=[CallbackPid|State#learner.callbacks]}};

% already decided and rebroadcast, do nothing
handle_cast({result, _Value}, #learner{learned=#decided{}}=State) ->
    {ok, State};
% already decided, so discard msgs
handle_cast({accepted, _Round, _Value}, #learner{learned=#decided{}}=State) -> 
    {ok, State};

% short-circuit to decided value when notified of a result
handle_cast({result, Value}, State) ->
    {ok, learn_value_and_update_state(Value, State)};
% counting accept votes
handle_cast({accepted, Round, Value}, #learner{accepted=Accepted}=State) ->
    Key = {Round, Value},
    NewState = case lists:keyfind(Key, 1, Accepted) of
        {Key, ?MAJORITY - 1} ->
            learn_value_and_update_state(Value, State);
        {Key, AcceptedCount} ->
            NewAccepted = lists:keyreplace(Key, 1, Accepted, {Key, AcceptedCount + 1}),
            State#learner{accepted=NewAccepted};
        false -> 
            NewAccepted = [{Key, 1}  | Accepted],
            State#learner{accepted=NewAccepted}
    end,
    {ok, NewState};

handle_cast(stop, State) -> 
    {stop, normal, State}.

learn_value_and_update_state(Value, State) ->
    learners:broadcast_result(Value),
    send_callbacks(Value, State#learner.callbacks),
    % discard all callbacks, which have now been sent, 
    % and discard vote counts, which has now been decided
    State#learner{
        accepted = [],
        learned = #decided{value = Value},
        callbacks = []
    }.

send_callbacks(Value, RegisteredCallbacks) ->
    [Pid ! {result, Value} || Pid <- RegisteredCallbacks],
    ok.

%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.