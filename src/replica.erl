-module(replica).
-export([request/1]).
-export([loop/1, start/1]).

-record(replica, {slot_num = 1,
                  proposals = [],
                  decisions = [],
                  application = undefined
                  }).

-define (SERVER, ?MODULE).

%%% Client API
request(Operation) ->
    Client = self(),
    UniqueRef = make_ref(),
    gen_server:abcast(?SERVER, {request, {Client, UniqueRef, Operation}}),
    receive
        {response, UniqueRef, Result} ->
            {ok, Result}
    % TODO: add timeout?
    end.
    
%%% Replica 
start(LockApplication) ->
    process_flag(trap_exit, true),
    ReplicaState = #replica{application=LockApplication},
    register(replica, spawn_link(fun() -> loop(ReplicaState) end)).    

slot_for_next_proposal(#replica{proposals=Proposals, decisions=Decisions}) ->
    MaxSlotFn = fun({Slot, _Command}, Highest) -> max(Slot, Highest) end,
    MaxPropSlot = lists:foldl(MaxSlotFn, 0, Proposals),
    HighSlot = lists:foldl(MaxSlotFn, MaxPropSlot, Decisions),
    1 + HighSlot.

propose(Command, State) ->
    case is_command_already_decided(Command, State) of 
        false ->
            Proposal = {slot_for_next_proposal(State), Command},
            send_to_leaders(Proposal, State),
            add_proposal_to_state(Proposal, State);
        true ->
            State
    end.

is_command_already_decided(Command, State) ->
    lists:any(
        fun({_S, DecidedCommand}) -> 
            Command == DecidedCommand 
        end, 
        State#replica.decisions
    ).

loop(State) ->
    receive
        {request, Command} ->
            NewState = propose(Command, State),
            loop(NewState);
        {decision, Slot, Command} ->
            NewStateA = add_decision_to_state({Slot, Command}, State),
            NewStateB = consume_decisions(NewStateA),
            loop(NewStateB);
        {'EXIT', _FromPid, _Reason} ->
            'a proposal crashed or exited',
            loop(State)
    end.

consume_decisions(State) ->
    case lists:keyfind(State#replica.slot_num, 1, State#replica.decisions) of
        false ->
            State;
        {_Slot, Command} ->
            % perform as many decided (queued) commands as possible
            % also check if any of our proposals have collided with another
            perform(Command, handle_proposal_preemption(State))
    end.

handle_proposal_preemption(State) ->
    % if someone stole our slot, propose again
    case lists:keyfind(State#replica.slot_num, 1, State#replica.proposals) of 
        false ->
            State;
        {_Slot, ConflictingCommand} ->
            propose(ConflictingCommand, State)
    end.
    
perform({Client, UniqueRef, Operation}, State) ->
    FunFilter = fun({S, _Operation}) -> S < State#replica.slot_num end,
    case lists:any(FunFilter, State#replica.decisions) of
        true ->
            tick_slot_number(State);
        false ->
            ResultFromFunction = (catch (State#replica.application):Operation(Client)),
            NewState = tick_slot_number(State),
            Client ! {response, UniqueRef, {Operation, ResultFromFunction}},
            NewState
    end.

tick_slot_number(State) ->
    State#replica{slot_num=State#replica.slot_num + 1}.

add_decision_to_state(SlotCommand, State) ->
    State#replica{ decisions = [SlotCommand | State#replica.decisions] }.

add_proposal_to_state(Proposal, State) ->
    State#replica{ proposals = [Proposal | State#replica.proposals] }.

send_to_leaders(Proposal, _State) ->
    proposer:propose(Proposal).
    