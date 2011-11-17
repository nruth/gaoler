-module(learner).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(MAJORITY, 3).
-include_lib("decided_record.hrl").

-export([get/0]).

%% Sends a blocking value query to the local learner 
%% returns dontknow on timeout, i.e. assumes value was not learned
get() ->
    % only query the local node's registered learner;
    % sidesteps consensus/asynch issues with remotes
    gen_event:call(learner, get_learned).

% starts with empty data store
init([]) -> {ok, []}.

%respond to value queries
handle_call(get_learned, _From, #decided{value=Value}=State) ->
    {reply, Value, State};
handle_call(get_learned, _From, State) ->
    {reply, dontknow, State}.

% already decided and rebroadcast, do nothing
handle_cast({result, _Value}, #decided{}=State) ->
    {ok, State};
% already decided, so discard msgs
handle_cast({accepted, _Round, _Value}, #decided{}=State) -> 
    {ok, State};

% short-circuit to decided value when notified of a result
handle_cast({result, Value}, _State) ->
    learners:broadcast_result(Value),
    {ok, #decided{value = Value}};
% counting accept votes
handle_cast({accepted, Round, Value}, State) ->
    Key = {Round, Value},
    NewState = case lists:keyfind(Key, 1, State) of
        {Key, ?MAJORITY - 1} ->
            learners:broadcast_result(Value),
            #decided{value=Value};
        {Key, AcceptedCount} ->
            lists:keyreplace(Key, 1, State, {Key, AcceptedCount + 1});
        false -> 
            [{Key, 1}  | State]
    end,
    {ok, NewState};

handle_cast(stop, State) -> 
    {stop, normal, State}.

%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.