#!/bin/sh
erl -pa ebin deps/*/ebin -s activityfeed +K true -eval "io:format(\"Point your browser at http://localhost:3999~n\")."
