-module(gleam@option).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([all/1, is_some/1, is_none/1, to_result/2, from_result/1, unwrap/2, lazy_unwrap/2, map/2, flatten/1, then/2, 'or'/2, lazy_or/2, values/1]).
-export_type([option/1]).

-type option(GD) :: {some, GD} | none.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 44).
-spec all_loop(list(option(GJ)), list(GJ)) -> option(list(GJ)).
all_loop(List, Acc) ->
    case List of
        [] ->
            {some, Acc};

        [X | Rest] ->
            Accumulate = fun(Acc@1, Item) -> case {Acc@1, Item} of
                    {{some, Values}, {some, Value}} ->
                        {some, [Value | Values]};

                    {_, _} ->
                        none
                end end,
            Accumulate(all_loop(Rest, Acc), X)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 40).
-spec all(list(option(GE))) -> option(list(GE)).
all(List) ->
    all_loop(List, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 73).
-spec is_some(option(any())) -> boolean().
is_some(Option) ->
    Option /= none.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 91).
-spec is_none(option(any())) -> boolean().
is_none(Option) ->
    Option =:= none.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 109).
-spec to_result(option(GT), GW) -> {ok, GT} | {error, GW}.
to_result(Option, E) ->
    case Option of
        {some, A} ->
            {ok, A};

        _ ->
            {error, E}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 130).
-spec from_result({ok, GZ} | {error, any()}) -> option(GZ).
from_result(Result) ->
    case Result of
        {ok, A} ->
            {some, A};

        _ ->
            none
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 151).
-spec unwrap(option(HE), HE) -> HE.
unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 172).
-spec lazy_unwrap(option(HG), fun(() -> HG)) -> HG.
lazy_unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default()
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 197).
-spec map(option(HI), fun((HI) -> HK)) -> option(HK).
map(Option, Fun) ->
    case Option of
        {some, X} ->
            {some, Fun(X)};

        none ->
            none
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 223).
-spec flatten(option(option(HM))) -> option(HM).
flatten(Option) ->
    case Option of
        {some, X} ->
            X;

        none ->
            none
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 262).
-spec then(option(HQ), fun((HQ) -> option(HS))) -> option(HS).
then(Option, Fun) ->
    case Option of
        {some, X} ->
            Fun(X);

        none ->
            none
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 293).
-spec 'or'(option(HV), option(HV)) -> option(HV).
'or'(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 324).
-spec lazy_or(option(HZ), fun(() -> option(HZ))) -> option(HZ).
lazy_or(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second()
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 345).
-spec values_loop(list(option(IH)), list(IH)) -> list(IH).
values_loop(List, Acc) ->
    case List of
        [] ->
            Acc;

        [First | Rest] ->
            Accumulate = fun(Acc@1, Item) -> case Item of
                    {some, Value} ->
                        [Value | Acc@1];

                    none ->
                        Acc@1
                end end,
            Accumulate(values_loop(Rest, Acc), First)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/option.gleam", 341).
-spec values(list(option(ID))) -> list(ID).
values(Options) ->
    values_loop(Options, []).
