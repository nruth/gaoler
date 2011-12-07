-module(replica).
-export([request/2]).
-export([loop/1, start/1, stop/0]).

-record(replica, {slot_num = 1,
                  proposals = [],
                  decisions = [],
                  application = undefined
                  }).

-define (SERVER, ?MODULE).

%%% Client API
request(Operation, Client) ->
    ClientProxy = self(),
    UniqueRef = make_ref(),
    [{?SERVER, Node} ! {request, {ClientProxy, UniqueRef, {Operation, Client}}} ||
        Node <- [node()|nodes()]],
    receive
        {response, UniqueRef, Result} ->
            {ok, Result}
    after 1000 ->
            timeout
    end.
    
%%% Replica 
start(LockApplication) ->
    process_flag(trap_exit, true),
    ReplicaState = #replica{application=LockApplication},
    register(replica, spawn_link(fun() -> loop(ReplicaState) end)).    

stop() ->
    ?SERVER ! stop.

loop(State) ->
    receive
        {request, Command} ->
            NewState = propose(Command, State),
            io:format("proposing ~p~n", [Command]),
            loop(NewState);
        {decision, Slot, Command} ->
            NewState = handle_decision(Slot, Command, State),
            loop(NewState);
        {'EXIT', _FromPid, _Reason} ->
            'a proposal crashed or exited',
            loop(State);
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
slot_for_next_proposal(#replica{proposals=Proposals, decisions=Decisions}) ->
    MaxSlotFn = fun({Slot, _Command}, Highest) -> max(Slot, Highest) end,
    MaxPropSlot = lists:foldl(MaxSlotFn, 0, Proposals),
    HighSlot = lists:foldl(MaxSlotFn, MaxPropSlot, Decisions),
    1 + HighSlot.

%% predicate returning true/false 
%% whether the command has already been decided and applied to the replica state
is_command_already_decided(Command, State) ->
    lists:any(
        fun({_S, DecidedCommand}) -> 
            Command == DecidedCommand 
        end, 
        State#replica.decisions
    ).

handle_decision(Slot, Command, State) ->
    NewStateA = add_decision_to_state({Slot, Command}, State),
    consume_decisions(NewStateA).

%% Performs as many decided (queued) commands as possible
%%   * Delivers contiguous commands from the holdback queue, 
%%      halting when reaching an empty slot
%%   * checks whether any proposals were pre-empted
%% returns: updated state, with changes to application, slot_number, and the command queues
consume_decisions(State) ->
    Slot = State#replica.slot_num,
    case lists:keyfind(Slot, 1, State#replica.decisions) of
        false ->
            State;
        {Slot, DecidedCommand} ->
            StateAfterProposalGC = gc_proposals(Slot, DecidedCommand, State),
            StateAfterPerform = perform(DecidedCommand, StateAfterProposalGC),
            consume_decisions(StateAfterPerform)
    end.

%% Garbage-collects the proposal for the given slot
%% When the command does not match the decided command
%% it will be re-proposed for another slot
gc_proposals(Slot, DecidedCommand, State) ->
    case lists:keyfind(Slot, 1, State#replica.proposals) of 
        false ->
            State;
        {Slot, DecidedCommand} ->
            % leave 
            State;
        {Slot, ConflictingCommand} ->
            CleanedState = remove_proposal_from_state(Slot, State),
            propose(ConflictingCommand, CleanedState)
    end.

perform({ClientProxy, UniqueRef, {Operation, Client}}, State) ->
    FunFilter = fun({S, _Operation}) -> S < State#replica.slot_num end,
    case lists:any(FunFilter, State#replica.decisions) of
        true ->
            tick_slot_number(State);
        false ->
            ResultFromFunction = (catch (State#replica.application):Operation(Client)),
            NewState = tick_slot_number(State),
            ClientProxy ! {response, UniqueRef, {Operation, ResultFromFunction}},
            NewState
    end.

tick_slot_number(State) ->
    State#replica{slot_num=State#replica.slot_num + 1}.

add_decision_to_state(SlotCommand, State) ->
    State#replica{ decisions = [SlotCommand | State#replica.decisions] }.

add_proposal_to_state(Proposal, State) ->
    State#replica{ proposals = [Proposal | State#replica.proposals] }.

remove_proposal_from_state(SlotNumber, State) ->
    State#replica{proposals = lists:keydelete(SlotNumber, State#replica.proposals)}.

send_to_leaders(Proposal, _State) ->
    proposer:propose(Proposal).
    
