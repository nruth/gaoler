-module(acceptors).
-export([
	 send_promise_request/1,
	 send_accept_request/2
        ]).

-export([
	 send_promise/2
	]).

send_promise_request(Round) ->
    [spawn(fun() -> acceptors:send_promise(Acceptor, Round) end) ||
	Acceptor <- gaoler:get_acceptors()],
    ok.

send_accept_request(_Round, _Value) ->
    ok.


%% Internal functions
send_promise(Acceptor, Round) ->    
    Reply = acceptor:prepare(Acceptor, Round),
    proposer:promised(Reply).
