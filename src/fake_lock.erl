-module (fake_lock).
-behaviour(gen_server).
-export ([acquire/1, release/1, stop/0]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define (SERVER, ?MODULE).

%%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

acquire (Client) ->
    gen_server:call(?SERVER, {acquire, Client}).

release (Client) ->
    gen_server:call(?SERVER, {release, Client}).
    
stop() ->
    gen_server:cast(?SERVER, stop).

%%% Internals

init([]) ->
    {ok, {}}.

%% gen_server callback
handle_call({acquire, Client}, _From, State) ->
    simple_comms:send_lock(Client),
    {reply, ok, State};
handle_call({release, _Client}, _From, State) -> 
    {reply, ok, State}.

%%%===================================================================
%%% Uninteresting gen_server boilerplate
%%%===================================================================
handle_cast(_,State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
