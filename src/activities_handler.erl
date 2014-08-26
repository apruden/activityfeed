-module(activities_handler).
 
%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

%% Custom callbacks.
-export([create_activity/2]).
-export([find_activity/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, find_activity}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_activity}], Req, State}.

resource_exists(Req, _State) ->
	case cowboy_req:binding(aid, Req) of
		{undefined, Req2} ->
			{true, Req2, index};
		{Aid, Req2} ->
			{true, Req2, Aid}
	end.

create_activity(Req, State) ->
	{Uid, Req2} = cowboy_req:binding(uid, Req),
	{ok, Val, Req3} = cowboy_req:body_qs(Req2),
	case cowboy_req:method(Req3) of
		{<<"POST">>, Req4} ->
			Aid = activityfeed_data:save({binary_to_integer(Uid), proplists:get_value(<<"content">>, Val, <<"NONE">>)}),
			{true, Req4, State};
		{_, Req4} ->
			{true, Req4, State}
	end.

find_activity(Req, index) ->
	{_Size, Req2} = cowboy_req:qs_val(<<"size">>, Req, 15),
	{"[]", Req2, index};
find_activity(Req, Aid) ->
	{Uid, Req2} = cowboy_req:binding(uid, Req),
	case activityfeed_data:find(binary_to_integer(Uid), binary_to_integer(Aid)) of
		false ->
			{"null", Req, Aid};
		{_, A} ->
			{A, Req, Aid}
	end.


%init(_Type, Req, _Opts) ->
%    {ok, Req, undefined_state}.
 
%handle(Req, State) ->
%    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}
%	], <<"{}">>, Req),
%    {ok, Req2, State}.
 
%terminate(_Reason, _Req, _State) ->
%    ok.

