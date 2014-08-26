-module(activityfeed_data).

-export([save/1, find/2]).

save(Activity) ->
	Uid = element(1, Activity),
	Pages = ets_lookup_or_insert(activities_collection, Uid, []),
	Aid = get_id(activities_page),
	case lists:reverse(Pages) of
		[] ->
			ets:insert(activities_page, {Aid, [{Aid, element(2, Activity)}]}),
			ets:insert(activities_collection, {Uid, [Aid]}),
			Aid;
		[Last|_] ->
			[{_, Activities}] = ets:lookup(activities_page, Last),
			case length(Activities) of
				X when X =:= 100 ->
					ets:insert(activities_page, {Aid, [{Aid, element(2, Activity)}]}),
					NewPages = lists:append(Pages, [Aid]),
					ets:insert(activities_collection, {Uid, NewPages}),
					Aid;
				_ ->
					NewAct = lists:append(Activities, [{Aid, element(2, Activity)}]),
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


get_id(Db) ->
	Id = ets_lookup_or_insert(Db, id, 0),
	Hi = ets_lookup_or_insert(Db, hi, 0),
	MaxLo = 100,
	case Id of
		Id2 when Id2 < ((MaxLo * Hi) + 1000), Id2 > 0 ->
			ets:update_counter(Db, id, 1);
		_ ->
			HiNew = ets:update_counter(Db, hi, 1),
			IdNew = (MaxLo * (HiNew - 1)) + 1000 + 1,
			ets:insert(Db, {id, IdNew}),
			IdNew
	end.

%% Private
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

get_id_test() ->
	ets:new(activities_page, [public, named_table]),
	Ids = [get_id(activities_page) || _X <- lists:seq(1, 300)],
	R = ets:lookup(activities_page, hi),
	?assertEqual( [X + 1000 || X <- lists:seq(1,300)], Ids),
	?assertEqual( ets:lookup(activities_page, hi), [{hi, 3}]).

-endif.

