-module(activities_handler).
 
%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([known_content_type/2]).

%% Custom callbacks.
-export([create_activity/2]).
-export([find_activity/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

known_content_type(Req, State) ->
	{true, Req, State}.

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
	{Uid, Req2} = cowboy_req:binding(uid, Req),
	{Limit, Req3} = cowboy_req:qs_val(<<"limit">>, Req2, <<"15">>),
	Activities = activityfeed_data:find_all(binary_to_integer(Uid), binary_to_integer(Limit)),
	{jsx:encode([[{aid, Aid}, {content, Content}] || {Aid, Content} <- Activities]), Req3, index};
find_activity(Req, Aid) ->
	{Uid, Req2} = cowboy_req:binding(uid, Req),
	case activityfeed_data:find(binary_to_integer(Uid), binary_to_integer(Aid)) of
		false ->
			{"null", Req, Aid};
		{_, Content} ->
			{jsx:encode([{aid, Aid}, {content, Content}]), Req, Aid}
	end.
