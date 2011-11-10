-module(learner).
-behaviour(gen_event).
-define(MAJORITY, 3).
-include_lib("decided_record.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).
 
init([]) ->
    {ok, []}.

% short-circuit to decided value
handle_event({result, Value}, _State) -> 
    {ok, #decided{value = Value}};

% already decided, so discard msgs
handle_event({accepted, _Round, _Value}, #decided{}=State) -> {ok, State};
handle_event({accepted, Round, Value}, State) ->
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
    {ok, NewState}.

handle_call(_, State) ->
{ok, ok, State}.

handle_info(_, State) ->
{ok, State}.

code_change(_OldVsn, State, _Extra) ->
{ok, State}.

terminate(_Reason, _State) ->
ok.