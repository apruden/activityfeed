-module(activityfeed_id_server).
-behaviour(gen_server).

-export([start_link/1, next_id/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start_link(_State) ->
	%%TODO: should use a central db counter to init state.
	gen_server:start_link({local, ?MODULE}, ?MODULE, {1001, 10, 100}, []).

next_id() ->
	gen_server:call(?MODULE, next).

%% Behaviour implementation

init(State) ->
	{ok, State}.

handle_call(next, _From, State) ->
	{Id, Hi, MaxLo} = get_id(State),
	{reply, Id, {Id, Hi, MaxLo}};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

get_id({Id, Hi, MaxLo}) ->
	case Id of
		Id2 when Id2 < (Hi + 1) * MaxLo, Id2 > 0 ->
			{Id2 + 1, Hi, MaxLo};
		_ ->
			HiNew = Hi + 1, %TODO: should use a central db to increment Hi counter
			{MaxLo * HiNew + 1, HiNew, MaxLo}
	end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_id_test() ->
	State = handle_call(next, [], {1, 1, 100}),
	?assertEqual({reply, 2, {2, 1, 100}}, State).

-endif.


