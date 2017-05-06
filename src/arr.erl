%%
%% Array with origin shift.
%%
-module(arr).
-export([new/3, get/2, set/3, size/1, lower/1, upper/1]).

-spec new(non_neg_integer(),integer(),_) -> {'arr',integer(),array:array(_)}.
new(Size, Zero, Defval) ->
	{arr, Zero, array:new(Size, [{default, Defval}, {fixed, true}])}.

-spec get(integer(),{'arr',integer(),array:array(_)}) -> any().
get(Index, {arr, Zero, Arr}) ->
	array:get(Zero+Index, Arr).

-spec set(integer(),_,{'arr',integer(),array:array(_)}) -> {'arr',integer(),array:array(_)}.
set(Index, Value, {arr, Zero, Arr}) ->
	{arr, Zero, array:set(Zero+Index, Value, Arr)}.

-spec size({'arr',_,array:array(_)}) -> non_neg_integer().
size({arr, _Zero, Arr}) ->
	array:size(Arr).

-spec lower({'arr',integer(),_}) -> integer().
lower({arr, Zero, _Arr}) ->
	-Zero.

-spec upper({'arr',integer(),array:array(_)}) -> integer().
upper({arr, Zero, Arr}) ->
	array:size(Arr) - Zero - 1.
