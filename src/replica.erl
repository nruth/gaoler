-module(replica).
-export([request/1]).
-export([loop/2, start/2]).

-record(replica, {slot_num = 1,
                  proposals = [],
                  decisions = [],
                  state = undefined,
                  application = centralised_lock}).

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
start(Leaders, InitialState) ->
    ReplicaState = #replica{state=InitialState},
    register(replica, spawn_link(fun() -> loop(Leaders, ReplicaState) end)).    

propose(_) ->
    error(writeme).

loop(Leaders, State) ->
    receive
        {request, Command} ->
            propose(Command),
            loop(Leaders, State);
        {decision, Slot, Command} ->
            NewState = State#replica{
                decisions=add_decision({Slot, Command}, State#replica.decisions)
            },
            UpdatedState = consume_decisions(NewState),
            loop(Leaders, UpdatedState)
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
            propose(ConflictingCommand)
    end.
    
perform({Client, UniqueRef, Operation}, State) ->
    FunFilter = fun({S, _Operation}) -> S < State#replica.slot_num end,
    case lists:any(FunFilter, State#replica.decisions) of
        true ->
            tick_slot_number(State);
        false ->
            (State#replica.application):Operation(Client),            
            NewState = tick_slot_number(State),
            Client ! {response, UniqueRef, Operation},
            NewState
    end.

tick_slot_number(State) ->
    State#replica{slot_num=State#replica.slot_num + 1}.

add_decision(SlotCommand, Decisions) ->
    [SlotCommand | Decisions].