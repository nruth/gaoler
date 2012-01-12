%TODO: does this module serve any purpose other than extra complexity in a total-ordering system?
% it's not supporting get requests after-the-fact so caching is moot
-module(learner).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(MAJORITY, 3).
-include_lib("decided_record.hrl").
-include_lib("learner_state.hrl").

-export([get/0]).

start_link() ->
    gen_server:start_link({local, learner}, ?MODULE, [], []).

%% Sends a blocking value query to the local learner 
%% returns dontknow on timeout, i.e. assumes value was not learned
get() ->
    % only query the local node's registered learner;
    % sidesteps consensus/asynch issues with remotes
    gen_server:call(learner, get_learned).

% starts with empty data store
init([]) -> {ok, #learner{}}.

handle_call(get_learned, _From, State) ->
    handle_get_learned(State).

handle_cast({result, Value}, State) -> 
    handle_result_notice(Value, State);
handle_cast(stop, State) -> 
    {stop, normal, State}.


%%%===================================================================
%%% Internals
%%%===================================================================

learn_value_and_update_state(Value, State) ->
    learners:broadcast_result(Value), %- this is going to cause a rebroadcast storm / not halt, because the value changes
    State#learner{learned = #decided{value = Value}}.

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
            {noreply, State};
        _ -> % short-circuit to decided value when notified of a result
            {noreply, learn_value_and_update_state(Value, State)}
    end.

%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
