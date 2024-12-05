-module(gleam@iterator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([unfold/2, repeatedly/1, repeat/1, from_list/1, transform/3, fold/3, run/1, to_list/1, step/1, take/2, drop/2, map/2, map2/3, append/2, flatten/1, concat/1, flat_map/2, filter/2, filter_map/2, cycle/1, find/2, find_map/2, index/1, iterate/2, take_while/2, drop_while/2, scan/3, zip/2, chunk/2, sized_chunk/2, intersperse/2, any/2, all/2, group/2, reduce/2, last/1, empty/0, once/1, range/2, single/1, interleave/2, fold_until/3, try_fold/3, first/1, at/2, length/1, each/2, yield/2]).
-export_type([action/1, iterator/1, step/2, chunk/2, sized_chunk/1]).

-type action(DRN) :: stop | {continue, DRN, fun(() -> action(DRN))}.

-opaque iterator(DRO) :: {iterator, fun(() -> action(DRO))}.

-type step(DRP, DRQ) :: {next, DRP, DRQ} | done.

-type chunk(DRR, DRS) :: {another_by,
        list(DRR),
        DRS,
        DRR,
        fun(() -> action(DRR))} |
    {last_by, list(DRR)}.

-type sized_chunk(DRT) :: {another, list(DRT), fun(() -> action(DRT))} |
    {last, list(DRT)} |
    no_more.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 39).
-spec stop() -> action(any()).
stop() ->
    stop.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 75).
-spec unfold_loop(DSB, fun((DSB) -> step(DSC, DSB))) -> fun(() -> action(DSC)).
unfold_loop(Initial, F) ->
    fun() -> case F(Initial) of
            {next, X, Acc} ->
                {continue, X, unfold_loop(Acc, F)};

            done ->
                stop
        end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 65).
-spec unfold(DRW, fun((DRW) -> step(DRX, DRW))) -> iterator(DRX).
unfold(Initial, F) ->
    _pipe = Initial,
    _pipe@1 = unfold_loop(_pipe, F),
    {iterator, _pipe@1}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 98).
-spec repeatedly(fun(() -> DSG)) -> iterator(DSG).
repeatedly(F) ->
    unfold(nil, fun(_) -> {next, F(), nil} end).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 114).
-spec repeat(DSI) -> iterator(DSI).
repeat(X) ->
    repeatedly(fun() -> X end).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 129).
-spec from_list(list(DSK)) -> iterator(DSK).
from_list(List) ->
    Yield = fun(Acc) -> case Acc of
            [] ->
                done;

            [Head | Tail] ->
                {next, Head, Tail}
        end end,
    unfold(List, Yield).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 140).
-spec transform_loop(
    fun(() -> action(DSN)),
    DSP,
    fun((DSP, DSN) -> step(DSQ, DSP))
) -> fun(() -> action(DSQ)).
transform_loop(Continuation, State, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                case F(State, El) of
                    done ->
                        stop;

                    {next, Yield, Next_state} ->
                        {continue, Yield, transform_loop(Next, Next_state, F)}
                end
        end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 176).
-spec transform(iterator(DSU), DSW, fun((DSW, DSU) -> step(DSX, DSW))) -> iterator(DSX).
transform(Iterator, Initial, F) ->
    _pipe = transform_loop(erlang:element(2, Iterator), Initial, F),
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 212).
-spec fold_loop(fun(() -> action(DTE)), fun((DTG, DTE) -> DTG), DTG) -> DTG.
fold_loop(Continuation, F, Accumulator) ->
    case Continuation() of
        {continue, Elem, Next} ->
            fold_loop(Next, F, F(Accumulator, Elem));

        stop ->
            Accumulator
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 203).
-spec fold(iterator(DTB), DTD, fun((DTD, DTB) -> DTD)) -> DTD.
fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    fold_loop(_pipe, F, Initial).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 229).
-spec run(iterator(any())) -> nil.
run(Iterator) ->
    fold(Iterator, nil, fun(_, _) -> nil end).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 248).
-spec to_list(iterator(DTJ)) -> list(DTJ).
to_list(Iterator) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, [], fun(Acc, E) -> [E | Acc] end),
    lists:reverse(_pipe@1).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 277).
-spec step(iterator(DTM)) -> step(DTM, iterator(DTM)).
step(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            done;

        {continue, E, A} ->
            {next, E, {iterator, A}}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 311).
-spec take_loop(fun(() -> action(DTU)), integer()) -> fun(() -> action(DTU)).
take_loop(Continuation, Desired) ->
    fun() -> case Desired > 0 of
            false ->
                stop;

            true ->
                case Continuation() of
                    stop ->
                        stop;

                    {continue, E, Next} ->
                        {continue, E, take_loop(Next, Desired - 1)}
                end
        end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 305).
-spec take(iterator(DTR), integer()) -> iterator(DTR).
take(Iterator, Desired) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = take_loop(_pipe, Desired),
    {iterator, _pipe@1}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 355).
-spec drop_loop(fun(() -> action(DUA)), integer()) -> action(DUA).
drop_loop(Continuation, Desired) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Desired > 0 of
                true ->
                    drop_loop(Next, Desired - 1);

                false ->
                    {continue, E, Next}
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 350).
-spec drop(iterator(DTX), integer()) -> iterator(DTX).
drop(Iterator, Desired) ->
    _pipe = fun() -> drop_loop(erlang:element(2, Iterator), Desired) end,
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 390).
-spec map_loop(fun(() -> action(DUH)), fun((DUH) -> DUJ)) -> fun(() -> action(DUJ)).
map_loop(Continuation, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, F(E), map_loop(Continuation@1, F)}
        end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 384).
-spec map(iterator(DUD), fun((DUD) -> DUF)) -> iterator(DUF).
map(Iterator, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = map_loop(_pipe, F),
    {iterator, _pipe@1}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 432).
-spec map2_loop(
    fun(() -> action(DUR)),
    fun(() -> action(DUT)),
    fun((DUR, DUT) -> DUV)
) -> fun(() -> action(DUV)).
map2_loop(Continuation1, Continuation2, Fun) ->
    fun() -> case Continuation1() of
            stop ->
                stop;

            {continue, A, Next_a} ->
                case Continuation2() of
                    stop ->
                        stop;

                    {continue, B, Next_b} ->
                        {continue, Fun(A, B), map2_loop(Next_a, Next_b, Fun)}
                end
        end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 423).
-spec map2(iterator(DUL), iterator(DUN), fun((DUL, DUN) -> DUP)) -> iterator(DUP).
map2(Iterator1, Iterator2, Fun) ->
    _pipe = map2_loop(
        erlang:element(2, Iterator1),
        erlang:element(2, Iterator2),
        Fun
    ),
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 470).
-spec append_loop(fun(() -> action(DVB)), fun(() -> action(DVB))) -> action(DVB).
append_loop(First, Second) ->
    case First() of
        {continue, E, First@1} ->
            {continue, E, fun() -> append_loop(First@1, Second) end};

        stop ->
            Second()
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 465).
-spec append(iterator(DUX), iterator(DUX)) -> iterator(DUX).
append(First, Second) ->
    _pipe = fun() ->
        append_loop(erlang:element(2, First), erlang:element(2, Second))
    end,
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 498).
-spec flatten_loop(fun(() -> action(iterator(DVJ)))) -> action(DVJ).
flatten_loop(Flattened) ->
    case Flattened() of
        stop ->
            stop;

        {continue, It, Next_iterator} ->
            append_loop(
                erlang:element(2, It),
                fun() -> flatten_loop(Next_iterator) end
            )
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 493).
-spec flatten(iterator(iterator(DVF))) -> iterator(DVF).
flatten(Iterator) ->
    _pipe = fun() -> flatten_loop(erlang:element(2, Iterator)) end,
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 522).
-spec concat(list(iterator(DVN))) -> iterator(DVN).
concat(Iterators) ->
    flatten(from_list(Iterators)).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 545).
-spec flat_map(iterator(DVR), fun((DVR) -> iterator(DVT))) -> iterator(DVT).
flat_map(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    flatten(_pipe@1).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 582).
-spec filter_loop(fun(() -> action(DVZ)), fun((DVZ) -> boolean())) -> action(DVZ).
filter_loop(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Iterator} ->
            case Predicate(E) of
                true ->
                    {continue, E, fun() -> filter_loop(Iterator, Predicate) end};

                false ->
                    filter_loop(Iterator, Predicate)
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 574).
-spec filter(iterator(DVW), fun((DVW) -> boolean())) -> iterator(DVW).
filter(Iterator, Predicate) ->
    _pipe = fun() -> filter_loop(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 626).
-spec filter_map_loop(
    fun(() -> action(DWJ)),
    fun((DWJ) -> {ok, DWL} | {error, any()})
) -> action(DWL).
filter_map_loop(Continuation, F) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {continue, E@1, fun() -> filter_map_loop(Next, F) end};

                {error, _} ->
                    filter_map_loop(Next, F)
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 618).
-spec filter_map(iterator(DWC), fun((DWC) -> {ok, DWE} | {error, any()})) -> iterator(DWE).
filter_map(Iterator, F) ->
    _pipe = fun() -> filter_map_loop(erlang:element(2, Iterator), F) end,
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 653).
-spec cycle(iterator(DWQ)) -> iterator(DWQ).
cycle(Iterator) ->
    _pipe = repeat(Iterator),
    flatten(_pipe).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 732).
-spec find_loop(fun(() -> action(DWY)), fun((DWY) -> boolean())) -> {ok, DWY} |
    {error, nil}.
find_loop(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                true ->
                    {ok, E};

                false ->
                    find_loop(Next, F)
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 724).
-spec find(iterator(DWU), fun((DWU) -> boolean())) -> {ok, DWU} | {error, nil}.
find(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    find_loop(_pipe, Is_desired).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 778).
-spec find_map_loop(
    fun(() -> action(DXK)),
    fun((DXK) -> {ok, DXM} | {error, any()})
) -> {ok, DXM} | {error, nil}.
find_map_loop(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {ok, E@1};

                {error, _} ->
                    find_map_loop(Next, F)
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 770).
-spec find_map(iterator(DXC), fun((DXC) -> {ok, DXE} | {error, any()})) -> {ok,
        DXE} |
    {error, nil}.
find_map(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    find_map_loop(_pipe, Is_desired).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 808).
-spec index_loop(fun(() -> action(DXV)), integer()) -> fun(() -> action({DXV,
    integer()})).
index_loop(Continuation, Next) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, {E, Next}, index_loop(Continuation@1, Next + 1)}
        end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 802).
-spec index(iterator(DXS)) -> iterator({DXS, integer()}).
index(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = index_loop(_pipe, 0),
    {iterator, _pipe@1}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 831).
-spec iterate(DXY, fun((DXY) -> DXY)) -> iterator(DXY).
iterate(Initial, F) ->
    unfold(Initial, fun(Element) -> {next, Element, F(Element)} end).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 859).
-spec take_while_loop(fun(() -> action(DYD)), fun((DYD) -> boolean())) -> fun(() -> action(DYD)).
take_while_loop(Continuation, Predicate) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Next} ->
                case Predicate(E) of
                    false ->
                        stop;

                    true ->
                        {continue, E, take_while_loop(Next, Predicate)}
                end
        end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 850).
-spec take_while(iterator(DYA), fun((DYA) -> boolean())) -> iterator(DYA).
take_while(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = take_while_loop(_pipe, Predicate),
    {iterator, _pipe@1}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 896).
-spec drop_while_loop(fun(() -> action(DYJ)), fun((DYJ) -> boolean())) -> action(DYJ).
drop_while_loop(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Predicate(E) of
                false ->
                    {continue, E, Next};

                true ->
                    drop_while_loop(Next, Predicate)
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 888).
-spec drop_while(iterator(DYG), fun((DYG) -> boolean())) -> iterator(DYG).
drop_while(Iterator, Predicate) ->
    _pipe = fun() -> drop_while_loop(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 935).
-spec scan_loop(fun(() -> action(DYQ)), fun((DYS, DYQ) -> DYS), DYS) -> fun(() -> action(DYS)).
scan_loop(Continuation, F, Accumulator) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                Accumulated = F(Accumulator, El),
                {continue, Accumulated, scan_loop(Next, F, Accumulated)}
        end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 925).
-spec scan(iterator(DYM), DYO, fun((DYO, DYM) -> DYO)) -> iterator(DYO).
scan(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = scan_loop(_pipe, F, Initial),
    {iterator, _pipe@1}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 969).
-spec zip_loop(fun(() -> action(DYZ)), fun(() -> action(DZB))) -> fun(() -> action({DYZ,
    DZB})).
zip_loop(Left, Right) ->
    fun() -> case Left() of
            stop ->
                stop;

            {continue, El_left, Next_left} ->
                case Right() of
                    stop ->
                        stop;

                    {continue, El_right, Next_right} ->
                        {continue,
                            {El_left, El_right},
                            zip_loop(Next_left, Next_right)}
                end
        end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 964).
-spec zip(iterator(DYU), iterator(DYW)) -> iterator({DYU, DYW}).
zip(Left, Right) ->
    _pipe = zip_loop(erlang:element(2, Left), erlang:element(2, Right)),
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1031).
-spec next_chunk(fun(() -> action(DZO)), fun((DZO) -> DZQ), DZQ, list(DZO)) -> chunk(DZO, DZQ).
next_chunk(Continuation, F, Previous_key, Current_chunk) ->
    case Continuation() of
        stop ->
            {last_by, lists:reverse(Current_chunk)};

        {continue, E, Next} ->
            Key = F(E),
            case Key =:= Previous_key of
                true ->
                    next_chunk(Next, F, Key, [E | Current_chunk]);

                false ->
                    {another_by, lists:reverse(Current_chunk), Key, E, Next}
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1018).
-spec chunk_loop(fun(() -> action(DZJ)), fun((DZJ) -> DZL), DZL, DZJ) -> action(list(DZJ)).
chunk_loop(Continuation, F, Previous_key, Previous_element) ->
    case next_chunk(Continuation, F, Previous_key, [Previous_element]) of
        {last_by, Chunk} ->
            {continue, Chunk, fun stop/0};

        {another_by, Chunk@1, Key, El, Next} ->
            {continue, Chunk@1, fun() -> chunk_loop(Next, F, Key, El) end}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1005).
-spec chunk(iterator(DZE), fun((DZE) -> any())) -> iterator(list(DZE)).
chunk(Iterator, F) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                chunk_loop(Next, F, F(E), E)
        end end,
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1103).
-spec next_sized_chunk(fun(() -> action(EAC)), integer(), list(EAC)) -> sized_chunk(EAC).
next_sized_chunk(Continuation, Left, Current_chunk) ->
    case Continuation() of
        stop ->
            case Current_chunk of
                [] ->
                    no_more;

                Remaining ->
                    {last, lists:reverse(Remaining)}
            end;

        {continue, E, Next} ->
            Chunk = [E | Current_chunk],
            case Left > 1 of
                false ->
                    {another, lists:reverse(Chunk), Next};

                true ->
                    next_sized_chunk(Next, Left - 1, Chunk)
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1082).
-spec sized_chunk_loop(fun(() -> action(DZY)), integer()) -> fun(() -> action(list(DZY))).
sized_chunk_loop(Continuation, Count) ->
    fun() -> case next_sized_chunk(Continuation, Count, []) of
            no_more ->
                stop;

            {last, Chunk} ->
                {continue, Chunk, fun stop/0};

            {another, Chunk@1, Next_element} ->
                {continue, Chunk@1, sized_chunk_loop(Next_element, Count)}
        end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1073).
-spec sized_chunk(iterator(DZU), integer()) -> iterator(list(DZU)).
sized_chunk(Iterator, Count) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = sized_chunk_loop(_pipe, Count),
    {iterator, _pipe@1}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1164).
-spec intersperse_loop(fun(() -> action(EAJ)), EAJ) -> action(EAJ).
intersperse_loop(Continuation, Separator) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            Next_interspersed = fun() -> intersperse_loop(Next, Separator) end,
            {continue, Separator, fun() -> {continue, E, Next_interspersed} end}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1151).
-spec intersperse(iterator(EAG), EAG) -> iterator(EAG).
intersperse(Iterator, Elem) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                {continue, E, fun() -> intersperse_loop(Next, Elem) end}
        end end,
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1213).
-spec any_loop(fun(() -> action(EAO)), fun((EAO) -> boolean())) -> boolean().
any_loop(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            false;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    true;

                false ->
                    any_loop(Next, Predicate)
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1205).
-spec any(iterator(EAM), fun((EAM) -> boolean())) -> boolean().
any(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    any_loop(_pipe, Predicate).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1263).
-spec all_loop(fun(() -> action(EAS)), fun((EAS) -> boolean())) -> boolean().
all_loop(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            true;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    all_loop(Next, Predicate);

                false ->
                    false
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1255).
-spec all(iterator(EAQ), fun((EAQ) -> boolean())) -> boolean().
all(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    all_loop(_pipe, Predicate).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1309).
-spec update_group_with(EBI) -> fun((gleam@option:option(list(EBI))) -> list(EBI)).
update_group_with(El) ->
    fun(Maybe_group) -> case Maybe_group of
            {some, Group} ->
                [El | Group];

            none ->
                [El]
        end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1300).
-spec group_updater(fun((EBA) -> EBB)) -> fun((gleam@dict:dict(EBB, list(EBA)), EBA) -> gleam@dict:dict(EBB, list(EBA))).
group_updater(F) ->
    fun(Groups, Elem) -> _pipe = Groups,
        gleam@dict:upsert(_pipe, F(Elem), update_group_with(Elem)) end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1291).
-spec group(iterator(EAU), fun((EAU) -> EAW)) -> gleam@dict:dict(EAW, list(EAU)).
group(Iterator, Key) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, maps:new(), group_updater(Key)),
    gleam@dict:map_values(_pipe@1, fun(_, Group) -> lists:reverse(Group) end).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1340).
-spec reduce(iterator(EBM), fun((EBM, EBM) -> EBM)) -> {ok, EBM} | {error, nil}.
reduce(Iterator, F) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            _pipe = fold_loop(Next, F, E),
            {ok, _pipe}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1371).
-spec last(iterator(EBQ)) -> {ok, EBQ} | {error, nil}.
last(Iterator) ->
    _pipe = Iterator,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1386).
-spec empty() -> iterator(any()).
empty() ->
    {iterator, fun stop/0}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1400).
-spec once(fun(() -> EBW)) -> iterator(EBW).
once(F) ->
    _pipe = fun() -> {continue, F(), fun stop/0} end,
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 679).
-spec range(integer(), integer()) -> iterator(integer()).
range(Start, Stop) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            once(fun() -> Start end);

        gt ->
            unfold(Start, fun(Current) -> case Current < Stop of
                        false ->
                            {next, Current, Current - 1};

                        true ->
                            done
                    end end);

        lt ->
            unfold(Start, fun(Current@1) -> case Current@1 > Stop of
                        false ->
                            {next, Current@1, Current@1 + 1};

                        true ->
                            done
                    end end)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1415).
-spec single(EBY) -> iterator(EBY).
single(Elem) ->
    once(fun() -> Elem end).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1447).
-spec interleave_loop(fun(() -> action(ECE)), fun(() -> action(ECE))) -> action(ECE).
interleave_loop(Current, Next) ->
    case Current() of
        stop ->
            Next();

        {continue, E, Next_other} ->
            {continue, E, fun() -> interleave_loop(Next, Next_other) end}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1439).
-spec interleave(iterator(ECA), iterator(ECA)) -> iterator(ECA).
interleave(Left, Right) ->
    _pipe = fun() ->
        interleave_loop(erlang:element(2, Left), erlang:element(2, Right))
    end,
    {iterator, _pipe}.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1492).
-spec fold_until_loop(
    fun(() -> action(ECM)),
    fun((ECO, ECM) -> gleam@list:continue_or_stop(ECO)),
    ECO
) -> ECO.
fold_until_loop(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            Accumulator;

        {continue, Elem, Next} ->
            case F(Accumulator, Elem) of
                {continue, Accumulator@1} ->
                    fold_until_loop(Next, F, Accumulator@1);

                {stop, Accumulator@2} ->
                    Accumulator@2
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1483).
-spec fold_until(
    iterator(ECI),
    ECK,
    fun((ECK, ECI) -> gleam@list:continue_or_stop(ECK))
) -> ECK.
fold_until(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    fold_until_loop(_pipe, F, Initial).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1536).
-spec try_fold_loop(
    fun(() -> action(ECY)),
    fun((EDA, ECY) -> {ok, EDA} | {error, EDB}),
    EDA
) -> {ok, EDA} | {error, EDB}.
try_fold_loop(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            {ok, Accumulator};

        {continue, Elem, Next} ->
            case F(Accumulator, Elem) of
                {ok, Result} ->
                    try_fold_loop(Next, F, Result);

                {error, _} = Error ->
                    Error
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1527).
-spec try_fold(iterator(ECQ), ECS, fun((ECS, ECQ) -> {ok, ECS} | {error, ECT})) -> {ok,
        ECS} |
    {error, ECT}.
try_fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    try_fold_loop(_pipe, F, Initial).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1567).
-spec first(iterator(EDG)) -> {ok, EDG} | {error, nil}.
first(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, _} ->
            {ok, E}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1598).
-spec at(iterator(EDK), integer()) -> {ok, EDK} | {error, nil}.
at(Iterator, Index) ->
    _pipe = Iterator,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1627).
-spec length_loop(fun(() -> action(any())), integer()) -> integer().
length_loop(Continuation, Length) ->
    case Continuation() of
        stop ->
            Length;

        {continue, _, Next} ->
            length_loop(Next, Length + 1)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1622).
-spec length(iterator(any())) -> integer().
length(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    length_loop(_pipe, 0).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1652).
-spec each(iterator(EDS), fun((EDS) -> any())) -> nil.
each(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    run(_pipe@1).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/iterator.gleam", 1678).
-spec yield(EDV, fun(() -> iterator(EDV))) -> iterator(EDV).
yield(Element, Next) ->
    {iterator,
        fun() ->
            {continue, Element, fun() -> (erlang:element(2, Next()))() end}
        end}.
