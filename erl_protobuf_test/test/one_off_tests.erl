%% Author: jason
%% Created: Aug 28, 2011
%% Description: TODO: Add description to one_off_tests
-module(one_off_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("optional_message_pb.hrl").

-export([]).



all_test_() -> [
           {"Undefined optional message should not be encoded", ?_assertMatch(<<>>,iolist_to_binary(optional_message_pb:encode_outer(#outer{msg=undefined})))}
           ].

%%
%% API Functions
%%



%%
%% Local Functions
%%

