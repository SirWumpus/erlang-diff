-module(arr_test).
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
	{
	setup,
	fun () ->
		arr:new(6, 2, -1)
	end,
	fun (Arr) ->
		{arr, _, Inner} = Arr,
		[
		?_assertMatch({arr, 2, _}, Arr),
		?_assertMatch(-1, array:get(0, Inner)),
		?_assertMatch(-1, array:get(5, Inner)),
		?_assertError(badarg, array:get(6, Inner)),
		?_assertError(badarg, array:get(-1, Inner))
		]
	end
	}.

set_test_() ->
	{
	setup,
	fun () ->
		Arr0 = arr:new(6, 2, -1),
		Arr1 = arr:set(0, "Z", Arr0),
		Arr2 = arr:set(3, "Z+3", Arr1),
		       arr:set(-2, "Z-2", Arr2)
	end,
	fun (Arr) ->
		{arr, Zero, Inner} = Arr,
		[
		?_assertMatch("Z", array:get(Zero, Inner)),
		?_assertMatch("Z+3", array:get(Zero+3, Inner)),
		?_assertMatch("Z-2", array:get(Zero-2, Inner)),
		?_assertError(badarg, arr:set(4, "Z+4", Arr)),
		?_assertError(badarg, arr:set(-3, "Z-3", Arr))
		]
	end
	}.

get_test_() ->
	{
	setup,
	fun () ->
		Arr0 = arr:new(6, 2, -1),
		Arr1 = arr:set(0, "Z", Arr0),
		Arr2 = arr:set(3, "Z+3", Arr1),
		       arr:set(-2, "Z-2", Arr2)
	end,
	fun (Arr) ->
		[
		?_assertMatch("Z", arr:get(0, Arr)),
		?_assertMatch("Z+3", arr:get(3, Arr)),
		?_assertMatch("Z-2", arr:get(-2, Arr)),
		?_assertMatch(-1, arr:get(-1, Arr)),
		?_assertMatch(-1, arr:get(1, Arr)),
		?_assertMatch(-1, arr:get(2, Arr)),
		?_assertError(badarg, arr:get(4, Arr)),
		?_assertError(badarg, arr:get(-3, Arr))
		]
	end
	}.

size_test_() ->
	[
	?_assertMatch(6, arr:size(arr:new(6, 2, -1)))
	].
