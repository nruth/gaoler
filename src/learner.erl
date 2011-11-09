-module(learner).
-behaviour(gen_event).
-define(MAJORITY, 3).
 
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).
 
init([]) ->
    {ok, []}.

handle_event({accepted, Round, Value}, State) ->
    Key = {Round, Value},
    NewState = case lists:keyfind(Key, 1, State) of
        {Key, AcceptedCount} ->
            NewAcceptedCount = AcceptedCount + 1,
            broadcast_value_if_majority(NewAcceptedCount, Value),
            lists:keyreplace(Key, 1, State, {Key, NewAcceptedCount});
        false -> 
            [{Key, 1}  | State]
    end,
    {ok, NewState}.

broadcast_value_if_majority(?MAJORITY, Value) ->
    learners:broadcast_result(Value);
broadcast_value_if_majority(_, _) ->
    no.

handle_call(_, State) ->
{ok, ok, State}.

handle_info(_, State) ->
{ok, State}.

code_change(_OldVsn, State, _Extra) ->
{ok, State}.

terminate(_Reason, _State) ->
ok.