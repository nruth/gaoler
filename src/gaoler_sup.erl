-module(gaoler_sup).

-behaviour(supervisor).

%% API
-export([
	 start_link/0,
	 start_house/0
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

start_house() ->
    start_house(next_house_sequence()).
start_house(HouseSequence) ->
    HouseName = house_name(HouseSequence),
    io:format("HouseName ~p ~n", [HouseName]),
    HouseSupChildSpec = {HouseName, {house_sup, start_link, 
				     [HouseName, HouseSequence]},
			 permanent, 2000, supervisor, [house_sup]},
    supervisor:start_child(?SERVER, HouseSupChildSpec).


%% Internal functions
house_name(Sequence) ->
    case Sequence of
	0 ->
	    house_sup;
	Else ->
	    list_to_atom("house_sup"++integer_to_list(Else))
    end.

next_house_sequence() ->
    Children = supervisor:count_children(?SERVER),
    case lists:keyfind(supervisors, 1, Children) of
	{supervisors, Count} ->
	    Count;
	false ->
	    {error, no_supervisors_in_list}
    end.
	    
