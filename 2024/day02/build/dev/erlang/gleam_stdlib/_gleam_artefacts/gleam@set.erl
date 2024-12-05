-module(gleam@set).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, size/1, is_empty/1, contains/2, delete/2, to_list/1, fold/3, filter/2, drop/2, take/2, intersection/2, difference/2, is_subset/2, is_disjoint/2, each/2, insert/2, from_list/1, map/2, union/2, symmetric_difference/2]).
-export_type([set/1]).

-opaque set(FBH) :: {set, gleam@dict:dict(FBH, list(nil))}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 32).
-spec new() -> set(any()).
new() ->
    {set, maps:new()}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 50).
-spec size(set(any())) -> integer().
size(Set) ->
    maps:size(erlang:element(2, Set)).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 68).
-spec is_empty(set(any())) -> boolean().
is_empty(Set) ->
    Set =:= new().

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 110).
-spec contains(set(FBS), FBS) -> boolean().
contains(Set, Member) ->
    _pipe = erlang:element(2, Set),
    _pipe@1 = gleam_stdlib:map_get(_pipe, Member),
    gleam@result:is_ok(_pipe@1).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 131).
-spec delete(set(FBU), FBU) -> set(FBU).
delete(Set, Member) ->
    {set, gleam@dict:delete(erlang:element(2, Set), Member)}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 149).
-spec to_list(set(FBX)) -> list(FBX).
to_list(Set) ->
    maps:keys(erlang:element(2, Set)).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 190).
-spec fold(set(FCD), FCF, fun((FCF, FCD) -> FCF)) -> FCF.
fold(Set, Initial, Reducer) ->
    gleam@dict:fold(
        erlang:element(2, Set),
        Initial,
        fun(A, K, _) -> Reducer(A, K) end
    ).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 214).
-spec filter(set(FCG), fun((FCG) -> boolean())) -> set(FCG).
filter(Set, Predicate) ->
    {set,
        gleam@dict:filter(erlang:element(2, Set), fun(M, _) -> Predicate(M) end)}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 249).
-spec drop(set(FCN), list(FCN)) -> set(FCN).
drop(Set, Disallowed) ->
    gleam@list:fold(Disallowed, Set, fun delete/2).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 267).
-spec take(set(FCR), list(FCR)) -> set(FCR).
take(Set, Desired) ->
    {set, gleam@dict:take(erlang:element(2, Set), Desired)}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 287).
-spec order(set(FCZ), set(FCZ)) -> {set(FCZ), set(FCZ)}.
order(First, Second) ->
    case maps:size(erlang:element(2, First)) > maps:size(
        erlang:element(2, Second)
    ) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 305).
-spec intersection(set(FDE), set(FDE)) -> set(FDE).
intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 323).
-spec difference(set(FDI), set(FDI)) -> set(FDI).
difference(First, Second) ->
    drop(First, to_list(Second)).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 344).
-spec is_subset(set(FDM), set(FDM)) -> boolean().
is_subset(First, Second) ->
    intersection(First, Second) =:= First.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 362).
-spec is_disjoint(set(FDP), set(FDP)) -> boolean().
is_disjoint(First, Second) ->
    intersection(First, Second) =:= new().

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 402).
-spec each(set(FDW), fun((FDW) -> any())) -> nil.
each(Set, Fun) ->
    fold(
        Set,
        nil,
        fun(Nil, Member) ->
            Fun(Member),
            Nil
        end
    ).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 86).
-spec insert(set(FBP), FBP) -> set(FBP).
insert(Set, Member) ->
    {set, gleam@dict:insert(erlang:element(2, Set), Member, [])}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 167).
-spec from_list(list(FCA)) -> set(FCA).
from_list(Members) ->
    Dict = gleam@list:fold(
        Members,
        maps:new(),
        fun(M, K) -> gleam@dict:insert(M, K, []) end
    ),
    {set, Dict}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 232).
-spec map(set(FCJ), fun((FCJ) -> FCL)) -> set(FCL).
map(Set, Fun) ->
    fold(Set, new(), fun(Acc, Member) -> insert(Acc, Fun(Member)) end).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 282).
-spec union(set(FCV), set(FCV)) -> set(FCV).
union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun insert/2).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/set.gleam", 374).
-spec symmetric_difference(set(FDS), set(FDS)) -> set(FDS).
symmetric_difference(First, Second) ->
    difference(union(First, Second), intersection(First, Second)).
