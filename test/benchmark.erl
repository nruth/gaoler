-module(benchmark).
-include_lib("eunit/include/eunit.hrl").

-export([
	 run/1,
	 exploit/1
	]).

run(Requests) ->
    test_prepare_one_node(),
    [{X, spawn(fun() ->
		       ?MODULE:exploit(Requests div 4)
	       end)} || X <- lists:seq(0,3)].

exploit(Requests) ->
    Requester = self(),
    
    Start = now(),
    Replies = [gaoler_frontend:get_lock(X, Requester)
	       || X <- lists:seq(1,Requests)],
    Stop = now(),

    Failed = length(lists:filter(fun(failed) -> true; 
				    (_Else) -> false end, Replies)),
    
    io:format("Diff: ~p Failed: ~p ~n", [timer:now_diff(Stop, Start), 
					 Failed]).
    

test_prepare_one_node() ->
    case application:start(gaoler) of 
	{error, _} ->
	    ok;
	_Else ->
	    gaoler:join(),
						% add 4 more acceptors
	    {ok, A2} = 
		supervisor:start_child(gaoler_sup, 
				       {acceptor2, {acceptor, start_link, []},
					permanent, 5000, worker, [acceptor2]}),
	    gaoler:join(A2),
	    {ok, A3} = 
		supervisor:start_child(gaoler_sup, 
				       {acceptor3, {acceptor, start_link, []},
					permanent, 5000, worker, [acceptor3]}),
	    gaoler:join(A3),
	    {ok, A4} = 
		supervisor:start_child(gaoler_sup, 
				       {acceptor4, {acceptor, start_link, []},
					permanent, 5000, worker, [acceptor4]}),
	    gaoler:join(A4),
	    {ok, A5} =
	supervisor:start_child(gaoler_sup, 
			       {acceptor5, {acceptor, start_link, []},
				permanent, 5000, worker, [acceptor5]}),
	    gaoler:join(A5)
    end.
