-module(activityfeed_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0]). %utility function

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/users/:uid/activities/[:aid]", activities_handler, []},
			   {"/", cowboy_static, {file, "./priv/static/index.html"}}
			]}
    ]),
    cowboy:start_http(my_http_listener, 100,
        [{port, 80}],
        [{env, [{dispatch, Dispatch}]}]
    ),
	ok = create_buckets([user, activities_collection, activities_page]),
	activityfeed_id_server:start_link([]),
    activityfeed_sup:start_link().

stop(_State) ->
    ok.

%% @doc Used when starting the application from the shell.
start() ->
	application:ensure_all_started(activityfeed).

create_buckets([]) -> ok;
create_buckets([H | T]) ->
	ok = create_bucket(H, set),
	create_buckets(T).

create_bucket(Name, Type) ->
	ets:new(Name, [Type, public, named_table, compressed]),
	ok.
