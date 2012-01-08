-module(lock_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("lock_state.hrl").

%%% =============================
%%% Test Helpers
%%% =============================

%% Meck stub modules, used to enable decoupled unit tests
setup() ->
    Mods = [mock_lock_persistence, mock_comms],
    meck:new(Mods),
    meck:expect(mock_lock_persistence, lock_available, 1, ok),
    meck:expect(mock_lock_persistence, lock_holder_changed, 1, ok),
    meck:expect(mock_lock_persistence, lock_granted, 1, ok),
    meck:expect(mock_comms, send_lock, 1, ok),
    Mods.

teardown(Mods) ->
    meck:unload(Mods).

add_dummy_client_to_queue(State) ->
    add_client_to_queue(State, dummy_client).
add_client_to_queue(State, Client) ->    
    State#state{queue = queue:in(Client, State#state.queue)}.


%%% =============================
%%% Startup tests
%%% =============================
new_lock_has_no_client_queue_test() ->
    {ok, State} = lock:init([mock_lock_persistence, mock_comms]),
    ?assert(queue:is_empty(State#state.queue)).

new_lock_stores_persistence_callback_module_test() ->
    {ok, State} = lock:init([mock_lock_persistence, mock_comms]),
    ?assertEqual(State#state.persistence_module, mock_lock_persistence).

new_lock_stores_comms_module_test() ->
    {ok, State} = lock:init([mock_lock_persistence, mock_comms]),
    ?assertEqual(State#state.comms_module, mock_comms).


%%% =============================
%%% Acquire lock
%%% =============================
behaviour_on_acquire_request_test_() -> {foreach, fun setup/0, fun teardown/1, [
    fun with_empty_lock_queue_add_client_to_queue/0,
    fun with_empty_lock_queue_send_lock_to_requester/0,
    fun with_empty_lock_queue_run_persistence_lock_granted_callback/0,
    fun with_queued_clients_add_client_to_queue/0,
    fun with_queued_clients_dont_send_lock_to_requester/0,
    fun with_queued_clients_dont_run_persistence_lock_granted_callback/0
]}.

with_empty_lock_queue_send_lock_to_requester() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    lock:handle_call({acquire, someclient}, sender, InitState),
    ?assert(meck:called(mock_comms, send_lock, [someclient])).

with_queued_clients_dont_send_lock_to_requester() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    ClientsQueuedState = add_dummy_client_to_queue(InitState),
    lock:handle_call({acquire, someclient}, sender, ClientsQueuedState),
    ?assertNot(meck:called(mock_comms, send_lock, [someclient])).

with_empty_lock_queue_add_client_to_queue() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    {reply, ok, State} = lock:handle_call({acquire, someclient}, sender, InitState),
    ?assertEqual(someclient, queue:last(State#state.queue)).

with_queued_clients_add_client_to_queue() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    ClientsQueuedState = add_dummy_client_to_queue(InitState),
    {reply, ok, State} = lock:handle_call({acquire, someclient}, sender, ClientsQueuedState),
    ?assertEqual(someclient, queue:last(State#state.queue)).

with_empty_lock_queue_run_persistence_lock_granted_callback() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    {reply, ok, FinState} = lock:handle_call({acquire, someclient}, sender, InitState),
    ?assert(meck:called(mock_lock_persistence, lock_granted, [FinState])).

with_queued_clients_dont_run_persistence_lock_granted_callback() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    ClientsQueuedState = add_dummy_client_to_queue(InitState),
    {reply, ok, FinState} = lock:handle_call({acquire, someclient}, sender, ClientsQueuedState),
    ?assertNot(meck:called(mock_lock_persistence, lock_granted, [FinState])).

%%% =============================
%%% Release lock
%%% =============================
behaviour_on_release_request_test_() -> {foreach, fun setup/0, fun teardown/1, [
    fun with_only_holder_queued_make_empty_queue/0,
    fun with_only_holder_queued_run_persistence_lock_available_callback/0,
    fun with_queued_clients_remove_releaser_from_queue/0,
    fun with_queued_clients_send_lock_to_next_in_queue/0,
    fun with_queued_clients_run_persistence_lock_holder_changed_callback/0
]}.

with_only_holder_queued_make_empty_queue() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    HolderQueuedState = add_dummy_client_to_queue(InitState),
    {reply, ok, FinState} = lock:handle_call({release, someclient}, sender, HolderQueuedState),
    ?assert(queue:is_empty(FinState#state.queue)).

with_only_holder_queued_run_persistence_lock_available_callback() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    HolderQueuedState = add_dummy_client_to_queue(InitState),
    {reply, ok, FinState} = lock:handle_call({release, someclient}, sender, HolderQueuedState),
    ?assertNot(meck:called(mock_lock_persistence, lock_holder_changed, [FinState])).

with_queued_clients_run_persistence_lock_holder_changed_callback() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    ClientsQueuedState = add_client_to_queue(add_client_to_queue(InitState, a), b),
    {reply, ok, FinState} = lock:handle_call({release, a}, sender, ClientsQueuedState),
    ?assertNot(meck:called(mock_lock_persistence, lock_holder_changed, [FinState])).

with_queued_clients_remove_releaser_from_queue() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    ClientsQueuedState = add_client_to_queue(add_client_to_queue(InitState, a), b),
    {reply, ok, FinState} = lock:handle_call({release, a}, sender, ClientsQueuedState),
    ?assertNot(queue:member(a, FinState#state.queue)),
    ?assertEqual(b, queue:get(FinState#state.queue)).

with_queued_clients_send_lock_to_next_in_queue() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    ClientsQueuedState = add_client_to_queue(add_client_to_queue(InitState, a), b),
    {reply, ok, _FinState} = lock:handle_call({release, a}, sender, ClientsQueuedState),
    ?assert(meck:called(mock_comms, send_lock, [b])).
