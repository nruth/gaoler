-module(house_sup).
-behaviour(supervisor).

-export([start_link/0,
	 init/1]).

-define(SERVER, ?MODULE).

-define(SUPFLAGS, {
	  one_for_one, % restart strategy
	  1000,        % max restarts
	  3600         % max seconds between restarts
	 }).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Acceptor = {acceptor, {acceptor, start_link, []}, 
    		permanent, 2000, worker, [acceptor]},

%    Proposer = {proposer, {proposer, start_link, []},
%     		permanent, 2000, worker, [proposer]},

    {ok, {?SUPFLAGS, [Acceptor]}}.
