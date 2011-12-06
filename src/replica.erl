-module(replica).
-export([request/1]).
-export([loop/2, start/2]).

-record(replica, {slot_num = 1,
                  proposals = [],
                  decisions = [],
                  state = undefined}).

-define (SERVER, ?MODULE).

%%% Client API
request(Operation) ->
    Client = self(),
    UniqueRef = make_ref(),
    gen_server:abcast(?SERVER, {request, {Client, UniqueRef, Operation}}),
    receive
        {response, UniqueRef, Result} ->
            {ok, Result}
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
    
perform(_Command, _State) ->
    error(writeme).

add_decision(SlotCommand, Decisions) ->
    [SlotCommand | Decisions].