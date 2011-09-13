
-module(gaoler_sup).

-behaviour(supervisor).

%% API
-export([
	 start_link/0,
	 add_acceptor/0
	]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Gaoler = ?CHILD(gaoler, worker),
    {ok, { {one_for_one, 5, 10}, [Gaoler]} }.

add_acceptor() ->
    % Child processes added using start_child/2 behave in the same 
    % manner as the other child processes, with the following important
    % exception: If a supervisor dies and is re-created, then all 
    % child processes which were dynamically added to the supervisor
    % will be lost.
    supervisor:start_child(?MODULE, ?CHILD(acceptor, worker)).
