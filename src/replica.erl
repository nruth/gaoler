-module(replica).
-export([request/2]).
-export([loop/1, start_link/1, stop/0]).

-record(replica, {slot_num = 1,
                  proposals = [],
                  decisions = [],
                  application = undefined
                  }).

-define (SERVER, ?MODULE).
-define (GC_INTERVAL, 20000).

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
    Pid = spawn_link(fun() -> loop(ReplicaState) end),
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

gc_decisions(State) ->
    CleanUpto = State#replica.slot_num - 100,
    Pred = fun({Slot, _Op}) ->
        Slot >= CleanUpto
    end,
    CleanedDecisions = lists:filter(Pred, State#replica.decisions),
    State#replica{decisions = CleanedDecisions}.


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
            StateAfterProposalGC = handle_received_decision(Slot, DecidedCommand, State),
            StateAfterPerform = perform(DecidedCommand, StateAfterProposalGC),
            % multicast slot to acceptors for gc every 50th decision
            check_gc_acceptor(Slot),
            consume_decisions(StateAfterPerform)
    end.

check_gc_acceptor(Slot) when Slot rem 50 == 0 ->
    [acceptor:gc_this(Acceptor, node(), Slot) || 
        Acceptor <- gaoler:get_acceptors()];
check_gc_acceptor(_) ->
    noop.

%% Garbage-collects the proposal for the given slot
%% When the command does not match the decided command
%% it will be re-proposed for another slot
handle_received_decision(Slot, DecidedCommand, State) ->
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
has_command_already_been_performed(Command, State) ->
    PastCmdMatcher = fun(
        {S, P}) -> 
            (P == Command) and (S < State#replica.slot_num)
    end,
    lists:any(PastCmdMatcher, State#replica.decisions).

inc_slot_number(State) ->
    State#replica{slot_num=State#replica.slot_num + 1}.

add_decision_to_state(SlotCommand, State) ->
    State#replica{ decisions = [SlotCommand | State#replica.decisions] }.

add_proposal_to_state(Proposal, State) ->
    State#replica{ proposals = [Proposal | State#replica.proposals] }.

remove_proposal_from_state(SlotNumber, State) ->
    State#replica{proposals = lists:keydelete(SlotNumber, 1, State#replica.proposals)}.

send_to_leaders(Proposal, _State) ->
%    self() ! {decision, Slot, Proposal}.
    proposer:propose(Proposal).
