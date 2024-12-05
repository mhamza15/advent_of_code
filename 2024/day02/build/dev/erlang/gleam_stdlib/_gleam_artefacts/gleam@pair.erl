-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/pair.gleam", 10).
-spec first({XU, any()}) -> XU.
first(Pair) ->
    {A, _} = Pair,
    A.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/pair.gleam", 24).
-spec second({any(), XX}) -> XX.
second(Pair) ->
    {_, A} = Pair,
    A.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/pair.gleam", 38).
-spec swap({XY, XZ}) -> {XZ, XY}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/pair.gleam", 53).
-spec map_first({YA, YB}, fun((YA) -> YC)) -> {YC, YB}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/pair.gleam", 68).
-spec map_second({YD, YE}, fun((YE) -> YF)) -> {YD, YF}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/pair.gleam", 83).
-spec new(YG, YH) -> {YG, YH}.
new(First, Second) ->
    {First, Second}.
