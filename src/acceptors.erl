-module(acceptors).
-export([
	 send_promise_requests/2,
	 send_accept_requests/3
        ]).

send_promise_requests(ReplyToProposer, Round) ->
    [spawn(fun() -> 
		   send_promise_request(ReplyToProposer, Acceptor, Round) 
	   end) || Acceptor <- gaoler:get_acceptors()],
    ok.

send_accept_requests(ReplyToProposer, Round, Value) ->
    [spawn(fun() -> 
		   send_accept_request(ReplyToProposer, 
					 Acceptor, Round, Value) 
	   end) || Acceptor <- gaoler:get_acceptors()],
    ok.


%% Internal functions
send_promise_request(Proposer, Acceptor, Round) ->    
    Reply = acceptor:prepare(Acceptor, Round),
    proposer:deliver_promise(Proposer, Reply).

send_accept_request(Proposer, Acceptor, Round, Value) ->
    Reply = acceptor:accept(Acceptor, Round, Value),
    proposer:deliver_accept(Proposer, Reply).
