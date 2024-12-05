-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, nil_error/1, values/1, try_recover/2]).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 20).
-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 41).
-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 66).
-spec map({ok, BVU} | {error, BVV}, fun((BVU) -> BVY)) -> {ok, BVY} |
    {error, BVV}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 91).
-spec map_error({ok, BWB} | {error, BWC}, fun((BWC) -> BWF)) -> {ok, BWB} |
    {error, BWF}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 120).
-spec flatten({ok, {ok, BWI} | {error, BWJ}} | {error, BWJ}) -> {ok, BWI} |
    {error, BWJ}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 158).
-spec 'try'({ok, BWQ} | {error, BWR}, fun((BWQ) -> {ok, BWU} | {error, BWR})) -> {ok,
        BWU} |
    {error, BWR}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 170).
-spec then({ok, BWZ} | {error, BXA}, fun((BWZ) -> {ok, BXD} | {error, BXA})) -> {ok,
        BXD} |
    {error, BXA}.
then(Result, Fun) ->
    'try'(Result, Fun).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 192).
-spec unwrap({ok, BXI} | {error, any()}, BXI) -> BXI.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 214).
-spec lazy_unwrap({ok, BXM} | {error, any()}, fun(() -> BXM)) -> BXM.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 236).
-spec unwrap_error({ok, any()} | {error, BXR}, BXR) -> BXR.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 258).
-spec unwrap_both({ok, BXU} | {error, BXU}) -> BXU.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 308).
-spec 'or'({ok, BYD} | {error, BYE}, {ok, BYD} | {error, BYE}) -> {ok, BYD} |
    {error, BYE}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 341).
-spec lazy_or({ok, BYL} | {error, BYM}, fun(() -> {ok, BYL} | {error, BYM})) -> {ok,
        BYL} |
    {error, BYM}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 367).
-spec all(list({ok, BYT} | {error, BYU})) -> {ok, list(BYT)} | {error, BYU}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 387).
-spec partition_loop(list({ok, BZI} | {error, BZJ}), list(BZI), list(BZJ)) -> {list(BZI),
    list(BZJ)}.
partition_loop(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            partition_loop(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            partition_loop(Rest@1, Oks, [E | Errors])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 383).
-spec partition(list({ok, BZB} | {error, BZC})) -> {list(BZB), list(BZC)}.
partition(Results) ->
    partition_loop(Results, [], []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 409).
-spec replace({ok, any()} | {error, BZR}, BZU) -> {ok, BZU} | {error, BZR}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 430).
-spec replace_error({ok, BZX} | {error, any()}, CAB) -> {ok, BZX} | {error, CAB}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 280).
-spec nil_error({ok, BXX} | {error, any()}) -> {ok, BXX} | {error, nil}.
nil_error(Result) ->
    replace_error(Result, nil).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 446).
-spec values(list({ok, CAE} | {error, any()})) -> list(CAE).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/result.gleam", 479).
-spec try_recover(
    {ok, CAK} | {error, CAL},
    fun((CAL) -> {ok, CAK} | {error, CAO})
) -> {ok, CAK} | {error, CAO}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
