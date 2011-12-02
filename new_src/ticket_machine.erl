-module(ticket_machine).
-behaviour(gen_server).

%% API
-export([
         start_link/0,
         next/1,
         stop/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {current_number = 0}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% Get the next bakery-algorithm ticket
%% Guarantees:  * no other will receive this ticket
%%              * tickets are totally ordered
%%              * tickets allocations are durable across crash; 
%%                  won't remember who it gave them to, but won't give them again
next(Resource) ->
    gen_server:call(?SERVER, {next_ticket, Resource}).

stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

%% issue ticket to caller and update ticket machine counter
handle_call({next_ticket, Resource}, _From, State) ->
    Ticket = lib_ticket:ticket(Resource, State#state.current_number),
    Reply = {ok, Ticket},

    % update state
    NewState = State#state{current_number = State#state.current_number+1},
    % TODO: add persistency

    {reply, Reply, NewState}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


