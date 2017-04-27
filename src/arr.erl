%%
%% Array with origin shift.
%%
-module(arr).
-export([new/3, get/2, set/3, size/1]).

new(Size, Zero, Defval) ->
	{arr, Zero, array:new(Size, [{default, Defval}, {fixed, true}])}.

get(Index, {arr, Zero, Arr}) ->
	array:get(Zero+Index, Arr).

set(Index, Value, {arr, Zero, Arr}) ->
	{arr, Zero, array:set(Zero+Index, Value, Arr)}.

size({arr, _Zero, Arr}) ->
	array:size(Arr).
