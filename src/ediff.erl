%%#!/usr/bin/env escript

%%%
%%% Erlang diff(1)
%%%
%%% Copyright 2017 by Anthony Howe.  All rights reserved.
%%%
%%% MIT License.
%%%
%%% Ported from http://www.ioccc.org/2015/howe/spoiler/prog.c
%%%

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
		throw({error, "-", 'only one file argument can refer to standard input'});
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
	{Distance, Script} = npdif:compute(hash_file(Fp1), hash_file(Fp2)),
	case proplists:get_value(write_distance, Opts, false) of
	true ->
		io:format("~B~n", [Distance]);
	false ->
		dump_script(Script)
	end.

hash_file(Fp) ->
	% Building list faster than an array when reading a file.
	Hashes = hash_file(Fp, []),
	file:position(Fp, 0),
	% Once built, convert list to array for faster lookups.
	array:from_list(lists:reverse(Hashes)).
hash_file(Fp, Lines) ->
	case file:read_line(Fp) of
	{ok, Line} ->
		{ok, Position} = file:postion(Fp, cur),
		hash_file(Fp, [#{seek => Position, hash => fnv:hash56(Line)} | Lines]);
	eof ->
		Lines;
	Error ->
		throw(Error)
	end.

dump_script(Script) ->
	io:format("~w~n", [Script]).
