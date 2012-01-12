-module(gaoler_sup).
-behaviour(supervisor).

%% API
-export([
	 start_link/0
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

% Start the frontend service gaoler and the internal paxos components 
% 
%              gaoler_sup
%           /      \       \
%     acceptor_sup gaoler  lock
%     /       
% acceptor 
%
init([]) ->
    GaolerService = {gaoler, {gaoler, start_link, []},
		     permanent, 2000, worker, [gaoler]},

    
    AcceptorSup = {acceptor_sup, {acceptor_sup, start_link, []},
                permanent, 2000, supervisor, [acceptor_sup]},
    
    % Lock Service
    LockService = {lock, {lock, start_link, [lock_no_persistence, simple_comms]},
                              permanent, 2000, worker, [lock]},

    % RSM
    RSM = {replica, {replica, start_link, [lock]},
           permanent, 2000, worker, [replica]},
    
    {ok, {?SUPFLAGS, [GaolerService, AcceptorSup, LockService, RSM]}}.
