-module(acceptors).
-export([
	 send_promise_request/2,
	 send_accept_request/3
        ]).

-export([
	 send_promise/3,
	 send_accept/4
	]).


send_promise_request(ReplyToProposer, Round) ->
    [spawn(fun() -> 
		   acceptors:send_promise(ReplyToProposer, Acceptor, Round) 
	   end) || Acceptor <- gaoler:get_acceptors()],
    ok.

send_accept_request(ReplyToProposer, Round, Value) ->
    [spawn(fun() -> 
		   acceptors:send_accept(ReplyToProposer, 
					 Acceptor, Round, Value) 
	   end) || Acceptor <- gaoler:get_acceptors()],
    ok.


%% Internal functions
send_promise(Proposer, Acceptor, Round) ->    
    Reply = acceptor:prepare(Acceptor, Round),
    proposer:deliver_promise(Proposer, Reply).

send_accept(Proposer, Acceptor, Round, Value) ->
    Reply = acceptor:accept(Acceptor, Round, Value),
    proposer:accepted(Proposer, Reply).
