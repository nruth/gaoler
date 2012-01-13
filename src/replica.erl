-module(replica).
-export([request/2]).
-export([loop/1, start_link/1, stop/0]).

-record(replica, {slot_num = 1,
                  proposals = [],
                  application = undefined
                  }).

-define (SERVER, ?MODULE).
-define (GC_INTERVAL, 60000).

-include_lib("stdlib/include/ms_transform.hrl").

%%% Client API
request(Operation, Client) ->
    spawn(fun() -> client_proxy(Operation, Client) end),
    ok.

client_proxy(Operation, Client) ->
    ClientProxy = self(),
    UniqueRef = make_ref(),
    [{?SERVER, Node} ! {request, {ClientProxy, UniqueRef, {Operation, Client}}} || Node <- [node()|nodes()]],
    %?SERVER ! {request, {ClientProxy, UniqueRef, {Operation, Client}}},
    receive
        {response, UniqueRef, {_, Result}} ->
            {ok, Result}
    end.
    
%%% Replica 
start_link(LockApplication) ->
    ReplicaState = #replica{application=LockApplication},
    Pid = spawn_link(fun() ->
        ets:new(replica_decisions, [set, named_table, public]),
        loop(ReplicaState)
    end),
    register(replica, Pid),
    erlang:send_after(?GC_INTERVAL, replica, gc_trigger),
    {ok, Pid}.

stop() ->
    ?SERVER ! stop.

loop(State) ->
    receive
        {request, Command} ->
            NewState = propose(Command, State),
            loop(NewState);
        {decision, Slot, Command} ->
            NewState = handle_decision(Slot, Command, State),
            loop(NewState);        
        gc_trigger ->
            NewState = gc_decisions(State),
            erlang:send_after(?GC_INTERVAL, replica, gc_trigger),
            loop(NewState);
        stop ->
            ok
    end.

%%% Internals

%% Push the command into the replica command queue
%% Should check that it hasn't already been delivered (duplicate msg)
%% Side-effects: sends messages to total ordering layer
%% Returns: updated state, with the command added to proposals
propose(Command, State) ->
    case is_command_already_decided(Command, State) of 
        false ->
            Proposal = {slot_for_next_proposal(State), Command},
            send_to_leaders(Proposal, State),
            add_proposal_to_state(Proposal, State);
        true ->
            State
    end.

%% returns the next available command sequence slot, using the local replica's state
slot_for_next_proposal(#replica{proposals=Proposals}) ->
    MaxSlotFn = fun({Slot, _Command}, Highest) -> max(Slot, Highest) end,
    MaxPropSlot = lists:foldl(MaxSlotFn, 0, Proposals),
    HighSlot = ets:foldl(MaxSlotFn, MaxPropSlot, replica_decisions),
    1 + HighSlot.

%% predicate returning true/false 
%% whether the command has already been decided and applied to the replica state
is_command_already_decided(Command, _State) ->
    CommandMatch = ets:fun2ms(
        fun({_Slot, Cmd}) -> % return true to delete
            Cmd =:= Command
        end
    ),
    ets:select_count(replica_decisions, CommandMatch) > 0.

handle_decision(Slot, Command, State) ->
    NewStateA = add_decision_to_state({Slot, Command}, State),
    NewStateB = remove_proposal_for_decided_slot(Slot, Command, NewStateA),
    consume_decisions(NewStateB).

%% Performs as many decided (queued) commands as possible
%%   * Delivers contiguous commands from the holdback queue, 
%%      halting when reaching an empty slot
%%   * checks whether any proposals were pre-empted
%% returns: updated state, with changes to application, slot_number, and the command queues
consume_decisions(State) ->
    Slot = State#replica.slot_num,
    case ets:lookup(replica_decisions, Slot) of
        [] ->
            State;
        [{Slot, DecidedCommand}] ->
            StateAfterPerform = perform(DecidedCommand, State),
            % multicast slot to acceptors for gc every nth decision
            check_gc_acceptor(Slot),
            consume_decisions(StateAfterPerform)
    end.

check_gc_acceptor(Slot) when Slot rem 300 == 0 ->
    [acceptor:gc_this(Acceptor, node(), Slot) || 
        Acceptor <- gaoler:get_acceptors()];
check_gc_acceptor(_) ->
    noop.

%% Garbage-collects the proposal for the given slot
%% When the command does not match the decided command
%% it will be re-proposed for another slot
remove_proposal_for_decided_slot(Slot, DecidedCommand, State) ->
    % do we have a proposal for the same slot?
    case lists:keyfind(Slot, 1, State#replica.proposals) of 
        false -> % no match, noop
            State;
        {Slot, DecidedCommand} -> % we proposed this decision, gc it
            remove_proposal_from_state(Slot, State);
        {Slot, ConflictingCommand} -> 
            % we proposed a different command in this slot, repropose it in another slot
            CleanedState = remove_proposal_from_state(Slot, State),
            propose(ConflictingCommand, CleanedState)
    end.

%% carries out Command, unless it's already been performed by a previous decision
perform({ClientProxy, UniqueRef, {Operation, Client}}=Command, State) ->
    case has_command_already_been_performed(Command, State) of
        true -> % don't apply repeat messages
            inc_slot_number(State);
        false -> % command not seen before, apply
            ResultFromFunction = (catch (State#replica.application):Operation(Client)),
            NewState = inc_slot_number(State),
            ClientProxy ! {response, UniqueRef, {Operation, ResultFromFunction}},            
            NewState
    end.

%% predicate: if exists an S : S < slot_num and {slot, command} in decisions
%TODO: enforce S < slot_num
has_command_already_been_performed(Command, State) ->
    is_command_already_decided(Command, State).

inc_slot_number(State) ->
    State#replica{slot_num=State#replica.slot_num + 1}.

gc_decisions(State) ->
    CleanUpto = State#replica.slot_num - 200,
    DeleteOlderThanIdSpec = ets:fun2ms(
        fun({Id, _Val}) -> % return true to delete
            Id < CleanUpto
        end
    ),
    ets:select_delete(replica_decisions, DeleteOlderThanIdSpec),
    State.

add_decision_to_state({Slot, Command}, State) ->
    ets:insert(replica_decisions, {Slot, Command}),
    State.

add_proposal_to_state(Proposal, State) ->
    State#replica{ proposals = [Proposal | State#replica.proposals] }.

remove_proposal_from_state(SlotNumber, State) ->
    State#replica{proposals = lists:keydelete(SlotNumber, 1, State#replica.proposals)}.

send_to_leaders(Proposal, _State) ->
%    self() ! {decision, Slot, Proposal}.
    proposer:propose(Proposal).
