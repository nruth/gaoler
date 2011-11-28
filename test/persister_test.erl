-module(persister_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("persister.hrl").
-include_lib("acceptor_state.hrl").

-define(ONE_ELECTION(E), #state{elections=[{1,E}]}).

setup() ->
    % make sure we always start with no file
    cleanup_logdirectory(),
    ok.

teardown(_) ->
    % clean up last file
    cleanup_logdirectory(),
    ok.

cleanup_logdirectory() ->
    file:delete(?LOGFILE), % old
    case file:list_dir(?LOGDIRECTORY) of
        {ok, Files} ->
            [file:delete(?LOGDIRECTORY++"/"++File) || File <- Files];
        _ ->
            ok
    end,
    file:del_dir(?LOGDIRECTORY).

log_values_to_file_test_() ->
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

%%% Promises
append_promise_value_to_file_api() ->
    Round = 1,
    Election = 1, 
    ?assertEqual(ok, persister:remember_promise(Election, Round)).
    
assert_that_promise_value_is_written_to_file() ->
    Round = 1,
    Election = 1,
    persister:remember_promise(Election, Round),
    {ok, [_IgnoreElectionId|Data]} = file:consult(?LOG(Election)),
    ?assertEqual([{promised, Round}], Data).


%%% Votes
append_voted_value_to_file_api() ->
    {Round, Value} = {1, value},
    Election = 1,
    ?assertEqual(ok, persister:remember_vote(Election, Round, Value)).

assert_that_round_and_value_is_written_to_file_on_vote() ->
    {Round, Value} = {1, value},
    Election = 1,
    persister:remember_vote(Election, Round, Value),
    {ok, [_IgnoreElectionId|Data]} = file:consult(?LOG(Election)),
    ?assertEqual([{accepted, Round, Value}], Data).


%%% Restore state
recover_state_from_logfiles_test_() ->
    {foreach, 
     fun setup/0,
     fun teardown/1, 
     [
      fun when_no_file_available_return_empty_state/0,
      fun when_file_exists_load_last_entries/0,
      fun when_file_is_corrupted_start_with_blank_state/0,
      fun when_two_election_files_exists/0,
      fun when_two_elections_one_is_corrupt/0
     ]
    }.

when_no_file_available_return_empty_state() ->
    ?assertEqual(#state{elections=[]}, 
                 persister:load_saved_state()). 

when_file_exists_load_last_entries()->
    Data = "{election,1}.\n{promised, 10}.\n{accepted, 9, value}.\n",
    ok = file:make_dir(?LOGDIRECTORY),
    ok = file:write_file(?LOG(1), Data, [append]),
    LoadedState = persister:load_saved_state(),
    ?assertEqual(?ONE_ELECTION(#election{promised=10, accepted={9, value}}), 
                 LoadedState).

when_file_is_corrupted_start_with_blank_state() ->
    Data = "{promised....}",
    ok = file:make_dir(?LOGDIRECTORY),
    ok = file:write_file(?LOG(1), Data, [append]),
    LoadedState = persister:load_saved_state(),
    ?assertEqual(#state{elections=[]}, LoadedState).
        
when_two_election_files_exists() ->
    Data1 = "{election,1}.\n{promised, 10}.\n{accepted, 9, value}.\n",
    Data2 = "{election,2}.\n{promised, 5}.\n{accepted, 5, value}.\n",
    ok = file:make_dir(?LOGDIRECTORY),
    ok = file:write_file(?LOG(1), Data1, [append]),
    ok = file:write_file(?LOG(2), Data2, [append]),
    LoadedState = persister:load_saved_state(),
    ?assertEqual(2, length(LoadedState#state.elections)).

when_two_elections_one_is_corrupt() ->
    Data1 = "{election,1}.\n{promised, 10}.\n{accepted, 9, value}.\n",
    Data2 = "{corrupt......\n",
    ok = file:make_dir(?LOGDIRECTORY),
    ok = file:write_file(?LOG(1), Data1, [append]),
    ok = file:write_file(?LOG(2), Data2, [append]),
    LoadedState = persister:load_saved_state(),
    ?assertEqual(1, length(LoadedState#state.elections)).
