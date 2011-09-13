%% convert this to a gen_fsm

-module(proposer).

%% Gaoler api
-export([
	 start_new_proposal/3
	]).

%% State machine api
-export([
	 propose/2,
	 accept/2
	]).

start_new_proposal(LockID, _Requester, RespondTo) ->
    RespondTo ! {got_lock, LockID}.

propose(_LockID, _ResourcePid) ->
    ok.

accept(_LockID, _ResourcePid) ->
    ok.
