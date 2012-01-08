-module(lock_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("lock_state.hrl").
-include_lib("queue_lib.hrl").

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
    State#state{queue = ?QUEUE_LIB:in(dummy_client, State#state.queue)}.


%%% =============================
%%% Startup tests
%%% =============================
new_lock_has_no_client_queue_test() ->
    {ok, State} = lock:init([mock_lock_persistence, mock_comms]),
    ?assert(?QUEUE_LIB:is_empty(State#state.queue)).

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
    ?assertEqual(someclient, ?QUEUE_LIB:last(State#state.queue)).

with_queued_clients_add_client_to_queue() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    ClientsQueuedState = add_dummy_client_to_queue(InitState),
    {reply, ok, State} = lock:handle_call({acquire, someclient}, sender, ClientsQueuedState),
    ?assertEqual(someclient, ?QUEUE_LIB:last(State#state.queue)).

with_empty_lock_queue_run_persistence_lock_granted_callback() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    {reply, ok, FinState} = lock:handle_call({acquire, someclient}, sender, InitState),
    ?assert(meck:called(mock_lock_persistence, lock_granted, [FinState])).

with_queued_clients_dont_run_persistence_lock_granted_callback() ->
    {ok, InitState} = lock:init([mock_lock_persistence, mock_comms]),
    ClientsQueuedState = add_dummy_client_to_queue(InitState),
    {reply, ok, FinState} = lock:handle_call({acquire, someclient}, sender, ClientsQueuedState),
    ?assertNot(meck:called(mock_lock_persistence, lock_granted, [FinState])).
