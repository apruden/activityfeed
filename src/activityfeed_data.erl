-module(activityfeed_data).

-export([save/1, find/2, find_all/2]).

save(Activity) ->
	{Uid, Content} = Activity,
	Pages = ets_lookup_or_insert(activities_collection, Uid, []),
	Aid = activityfeed_id_server:next_id(),
	case lists:reverse(Pages) of
		[] ->
			ets:insert(activities_page, {Aid, [{Aid, Content}]}),
			ets:insert(activities_collection, {Uid, [Aid]}),
			Aid;
		[Last|_] ->
			[{_, Activities}] = ets:lookup(activities_page, Last),
			case length(Activities) of
				X when X =:= 100 ->
					ets:insert(activities_page, {Aid, [{Aid, Content}]}),
					NewPages = lists:append(Pages, [Aid]),
					ets:insert(activities_collection, {Uid, NewPages}),
					Aid;
				_ ->
					NewAct = lists:append(Activities, [{Aid, Content}]),
					ets:insert(activities_page, {Last, NewAct}),
					Aid
			end
	end.


find(Uid, Aid) ->
	{_, Pages} = case ets:lookup(activities_collection, Uid) of
		[] ->
			{Uid, []};
		[{_, P}|_] ->
			{Uid, P}
		end,
	PagesFiltered = [X || X <- Pages, X =< Aid],
	case lists:reverse(PagesFiltered) of
		[] ->
			false;
		[H | _] ->
			[{_, Activities}] = ets:lookup(activities_page, H),
			lists:keyfind(Aid, 1, Activities)
	end.

find_all(Uid, Limit) ->
	{_, Pages} = case ets:lookup(activities_collection, Uid) of
		[] ->
			{Uid, []};
		[{_, P}|_] ->
			{Uid, P}
		end,
	find_all_loop(lists:reverse(Pages), [], Limit).

%% Private
find_all_loop([], Acc, _Limit) ->
	Acc;
find_all_loop(_Pages, Acc, Limit) when length(Acc) >= Limit ->
	Acc;
find_all_loop([H|T], Acc, Limit) ->
	[{_, Activities}] = ets:lookup(activities_page, H),
	find_all_loop(T, Acc ++ Activities, Limit).


ets_lookup_or_insert(Db, K, Def) ->
	case ets:lookup(Db, K) of
		[] -> 
			ets:insert(Db, {K, Def}),
			Def;
		[{_, V}|_] -> V
	end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

crud_test_() ->
	{setup, fun start/0, fun stop/1, fun insert_lookup/1}.

start() ->
	activityfeed_id_server:start_link([]),
	ets:new(activities_collection, [public, named_table]),
	ets:new(activities_page, [public, named_table]),
	ok.

stop(_State) ->
	ets:delete(activities_collection),
	ets:delete(activities_page),
	ok.

insert_lookup(_State) ->
	Aid = save({1, "test"}),
	Val = find(1, Aid),
	Aid2 = save({1, "test"}),
	Val2 = find(1, Aid2),
	[?_assertEqual({Aid, "test"}, Val),
	 ?_assertEqual({Aid2, "test"}, Val2)].

-endif.

