-module(house_sup).
-behaviour(supervisor).

-export([start_link/1,
	 init/1]).

-define(SERVER, ?MODULE).

-define(SUPFLAGS, {
	  one_for_one, % restart strategy
	  1000,        % max restarts
	  3600         % max seconds between restarts
	 }).

start_link(HouseSequence) ->
    supervisor:start_link(?MODULE, [HouseSequence]).

init([HouseSequence]) ->
    AcceptorName = list_to_atom("acceptor"++integer_to_list(HouseSequence)),
    ProposerName = list_to_atom("proposer"++integer_to_list(HouseSequence)),

    Acceptor = {AcceptorName, {acceptor, start_link, [AcceptorName]}, 
    		permanent, 2000, worker, [acceptor]},

    Proposer = {ProposerName, {proposer, start_link, [ProposerName]},
     		permanent, 2000, worker, [proposer]},

    {ok, {?SUPFLAGS, [Acceptor, Proposer]}}.
