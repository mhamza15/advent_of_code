-module(argv).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([load/0]).
-export_type([argv/0]).

-type argv() :: {argv, binary(), binary(), list(binary())}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/argv/src/argv.gleam", 16).
-spec load() -> argv().
load() ->
    {Runtime, Program, Arguments} = argv_ffi:load(),
    {argv, Runtime, Program, Arguments}.
