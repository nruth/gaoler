-module(ticket_machine).
-behaviour(gen_server).

%% API
-export([
         start_link/0,
         next/0,
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

next() ->
    gen_server:call(?SERVER, next_ticket).

stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(next_ticket, _From, State) ->
    NewState = State#state{current_number = State#state.current_number+1},
    % TODO: add persistency
    Reply = {ok, NewState#state.current_number},
    {reply, Reply, NewState}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
