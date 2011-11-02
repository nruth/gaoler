-module(gaoler_sup).

-behaviour(supervisor).

%% API
-export([
	 start_link/0,
	 add_child_acceptor/1
	]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(SUPFLAGS, {
	  one_for_one, % restart strategy
	  1000,        % max restarts
	  3600         % max seconds between restarts
	 }).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    GaolerService = {gaoler, {gaoler, start_link, []},
		     Restart, Shutdown, Type, [gaoler]},

    {ok, {?SUPFLAGS, [GaolerService]}}.

add_child_acceptor(AcceptorName) ->
    AcceptorChild = {AcceptorName, {acceptor, start, []}, 
		     permanent, 2000, worker, [acceptor]},
    supervisor:start_child(?SERVER, AcceptorChild).
