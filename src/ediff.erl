%%#!/usr/bin/env escript
-module(ediff).
-export([main/1, diff/2, diff/3]).

-ifdef(EUNIT).
-compile(export_all).
-endif.

-define(BUFSIZ, (8*1024)).
-define(INIT_FILE_SIZE, 5).

usage() ->
	io:format("usage: ediff [-d] file1 file2~n"),
	io:format("-d\t\twrite edit distance~n"),
	halt(2).

main(Args) ->
	case egetopt:parse(Args, [
		{ $d, flag, write_distance }
	]) of
	{ok, Options, ArgsN} ->
		process(Options, ArgsN);
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage()
	end.

process(_Opts, Files) when length(Files) /= 2 ->
	usage();
process(Opts, [File1, File2]) ->
	try
		process(Opts, File1, File2)
	catch
		throw:{error, File, Reason} ->
			io:format(standard_error, "ediff: ~s: ~s~n", [File, str:error(Reason)]),
			halt(1)
	end.

process(Opts, File1, File2) ->
	Fp1 = open_file(File1),
	Fp2 = open_file(File2),
	case {Fp1, Fp2} of
	{standard_io, standard_io} ->
		throw({error, "-", 'only one file can refer to standard input'});
	_ ->
		diff(Fp1, Fp2, Opts)
	end,
	file:close(Fp1),
	file:close(Fp2).

open_file("-") ->
        io:setopts(standard_io, [binary]),
	standard_io;
open_file(File) ->
	case file:open(File, [read, binary, {read_ahead, ?BUFSIZ}]) of
	{error, Reason} ->
		throw({error, File, Reason});
	{ok, Fp} ->
		Fp
	end.

diff(Fp1, Fp2) ->
	diff(Fp1, Fp2, []).
diff(Fp1, Fp2, Opts) ->
	{distance, script} = edit_distance(hash_file(Fp1), hash_file(Fp2)),
	case proplists:get_value(write_distance, Opts, false) of
	true ->
		io:format("~B~n", [distance]);
	false ->
		dump_script(script)
	end.

array_add(Arr, Item) ->
	array:set(array:size(Arr), Item, Arr).

hash_file(Fp) ->
	Arr = array:new(),
	% Treat the array as 1-based, by adding dummy entry for index 0.
	Hashes = hash_file(Fp, array_add(Arr, {0, 0})),
	file:position(Fp, 0),
	Hashes.
hash_file(Fp, Lines) ->
	case file:read_line(Fp) of
	{ok, Line} ->
		{ok, Position} = file:postion(Fp, cur),
		hash_file(Fp, array_add(Lines, {fnv:hash56(Line), Position}));
	eof ->
		Lines;
	Error ->
		throw(Error)
	end.

edit_distance(A, B) ->
	case array:size(A) > array:size(B) of
	true ->
		% Swap A and B if N < M.  The edit distance will be the
		% same, but edit script operations will be reversed.
		edit_distance(B, A, #{invert => true});
	false ->
		edit_distance(A, B, #{invert => false})
	end.
edit_distance(A, B, Map) ->
	{0, Map}.

dump_script(script) ->
	ok.
