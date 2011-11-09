-module(learner).
-behaviour(gen_event).
-define(MAJORITY, 3).
 
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).
 
init([]) ->
    {ok, []}.

handle_event({accepted, Round, Value}, State) ->
    {ok, update_accepted(Round, Value, State)}.

update_accepted(Round, Value, State) ->
    Key = {Round, Value},
    NewState = case lists:keyfind(Key, 1, State) of
        {Key,AcceptedCount} -> 
            lists:keyreplace(Key, 1, State, {Key, AcceptedCount+1});
        false -> 
            [{Key, 1}  | State]
    end.







%% otp boilerplate

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.