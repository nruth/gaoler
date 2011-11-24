-module(persister_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("persister.hrl").

setup() ->
    ok.

teardown(_) ->
    file:delete(?LOGFILE),
    ok.

file_storage_test_() ->
    {foreach, 
     fun setup/0,
     fun teardown/1, 
     [
      fun append_promise_value_to_file_api/0,
      fun assert_that_promise_value_is_written_to_file/0,
      fun append_voted_value_to_file_api/0,
      fun assert_that_round_and_value_is_written_to_file_on_vote/0
     ]
    }.

append_promise_value_to_file_api() ->
    Round = 1,
    ?assertEqual(ok, persister:remember_promise(Round)).
    
assert_that_promise_value_is_written_to_file() ->
    Round = 1,
    persister:remember_promise(Round),
    {ok, Data} = file:consult(?LOGFILE),
    ?assertEqual([{promised, Round}], Data).


append_voted_value_to_file_api() ->
    {Round, Value} = {1, value},
    ?assertEqual(ok, persister:remember_vote(Round, Value)).

assert_that_round_and_value_is_written_to_file_on_vote() ->
    {Round, Value} = {1, value},
    persister:remember_vote(Round, Value),
    {ok, Data} = file:consult(?LOGFILE),
    ?assertEqual([{accepted, Round, Value}], Data).
