-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map2/3, index_map/2, try_map/2, drop/2, take/2, new/0, wrap/1, append/2, prepend/2, concat/1, flatten/1, flat_map/2, fold/3, count/2, group/2, map_fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, key_filter/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1]).
-export_type([continue_or_stop/1, sorting/0]).

-type continue_or_stop(YJ) :: {continue, YJ} | {stop, YJ}.

-type sorting() :: ascending | descending.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 61).
-spec length_loop(list(any()), integer()) -> integer().
length_loop(List, Count) ->
    case List of
        [_ | List@1] ->
            length_loop(List@1, Count + 1);

        _ ->
            Count
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 57).
-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 130).
-spec reverse_loop(list(YT), list(YT)) -> list(YT).
reverse_loop(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            reverse_loop(Rest, [Item | Accumulator])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 126).
-spec reverse(list(YQ)) -> list(YQ).
reverse(List) ->
    lists:reverse(List).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 158).
-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 194).
-spec contains(list(YZ), YZ) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [First | _] when First =:= Elem ->
            true;

        [_ | Rest] ->
            contains(Rest, Elem)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 221).
-spec first(list(AAB)) -> {ok, AAB} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 250).
-spec rest(list(AAF)) -> {ok, list(AAF)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Rest] ->
            {ok, Rest}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 292).
-spec update_group(fun((AAQ) -> AAR)) -> fun((gleam@dict:dict(AAR, list(AAQ)), AAQ) -> gleam@dict:dict(AAR, list(AAQ))).
update_group(F) ->
    fun(Groups, Elem) -> case gleam_stdlib:map_get(Groups, F(Elem)) of
            {ok, Existing} ->
                gleam@dict:insert(Groups, F(Elem), [Elem | Existing]);

            {error, _} ->
                gleam@dict:insert(Groups, F(Elem), [Elem])
        end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 320).
-spec filter_loop(list(ABB), fun((ABB) -> boolean()), list(ABB)) -> list(ABB).
filter_loop(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            New_acc = case Fun(First) of
                true ->
                    [First | Acc];

                false ->
                    Acc
            end,
            filter_loop(Rest, Fun, New_acc)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 316).
-spec filter(list(AAY), fun((AAY) -> boolean())) -> list(AAY).
filter(List, Predicate) ->
    filter_loop(List, Predicate, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 352).
-spec filter_map_loop(
    list(ABM),
    fun((ABM) -> {ok, ABO} | {error, any()}),
    list(ABO)
) -> list(ABO).
filter_map_loop(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            New_acc = case Fun(First) of
                {ok, First@1} ->
                    [First@1 | Acc];

                {error, _} ->
                    Acc
            end,
            filter_map_loop(Rest, Fun, New_acc)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 348).
-spec filter_map(list(ABF), fun((ABF) -> {ok, ABH} | {error, any()})) -> list(ABH).
filter_map(List, Fun) ->
    filter_map_loop(List, Fun, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 383).
-spec map_loop(list(ABY), fun((ABY) -> ACA), list(ACA)) -> list(ACA).
map_loop(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            map_loop(Rest, Fun, [Fun(First) | Acc])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 379).
-spec map(list(ABU), fun((ABU) -> ABW)) -> list(ABW).
map(List, Fun) ->
    map_loop(List, Fun, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 410).
-spec map2_loop(list(ACJ), list(ACL), fun((ACJ, ACL) -> ACN), list(ACN)) -> list(ACN).
map2_loop(List1, List2, Fun, Acc) ->
    case {List1, List2} of
        {[], _} ->
            lists:reverse(Acc);

        {_, []} ->
            lists:reverse(Acc);

        {[A | As_], [B | Bs]} ->
            map2_loop(As_, Bs, Fun, [Fun(A, B) | Acc])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 406).
-spec map2(list(ACD), list(ACF), fun((ACD, ACF) -> ACH)) -> list(ACH).
map2(List1, List2, Fun) ->
    map2_loop(List1, List2, Fun, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 465).
-spec index_map_loop(
    list(ACZ),
    fun((ACZ, integer()) -> ADB),
    integer(),
    list(ADB)
) -> list(ADB).
index_map_loop(List, Fun, Index, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            Acc@1 = [Fun(First, Index) | Acc],
            index_map_loop(Rest, Fun, Index + 1, Acc@1)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 461).
-spec index_map(list(ACV), fun((ACV, integer()) -> ACX)) -> list(ACX).
index_map(List, Fun) ->
    index_map_loop(List, Fun, 0, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 519).
-spec try_map_loop(list(ADN), fun((ADN) -> {ok, ADP} | {error, ADQ}), list(ADP)) -> {ok,
        list(ADP)} |
    {error, ADQ}.
try_map_loop(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, lists:reverse(Acc)};

        [First | Rest] ->
            case Fun(First) of
                {ok, First@1} ->
                    try_map_loop(Rest, Fun, [First@1 | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 512).
-spec try_map(list(ADE), fun((ADE) -> {ok, ADG} | {error, ADH})) -> {ok,
        list(ADG)} |
    {error, ADH}.
try_map(List, Fun) ->
    try_map_loop(List, Fun, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 554).
-spec drop(list(ADX), integer()) -> list(ADX).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Rest] ->
                    drop(Rest, N - 1)
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 589).
-spec take_loop(list(AED), integer(), list(AED)) -> list(AED).
take_loop(List, N, Acc) ->
    case N =< 0 of
        true ->
            lists:reverse(Acc);

        false ->
            case List of
                [] ->
                    lists:reverse(Acc);

                [First | Rest] ->
                    take_loop(Rest, N - 1, [First | Acc])
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 585).
-spec take(list(AEA), integer()) -> list(AEA).
take(List, N) ->
    take_loop(List, N, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 609).
-spec new() -> list(any()).
new() ->
    [].

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 629).
-spec wrap(AEJ) -> list(AEJ).
wrap(Item) ->
    [Item].

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 650).
-spec append_loop(list(AEP), list(AEP)) -> list(AEP).
append_loop(First, Second) ->
    case First of
        [] ->
            Second;

        [Item | Rest] ->
            append_loop(Rest, [Item | Second])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 646).
-spec append(list(AEL), list(AEL)) -> list(AEL).
append(First, Second) ->
    lists:append(First, Second).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 670).
-spec prepend(list(AET), AET) -> list(AET).
prepend(List, Item) ->
    [Item | List].

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 675).
-spec reverse_and_prepend(list(AEW), list(AEW)) -> list(AEW).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [First | Rest] ->
            reverse_and_prepend(Rest, [First | Suffix])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 698).
-spec concat_loop(list(list(AFE)), list(AFE)) -> list(AFE).
concat_loop(Lists, Acc) ->
    case Lists of
        [] ->
            lists:reverse(Acc);

        [List | Further_lists] ->
            concat_loop(Further_lists, reverse_and_prepend(List, Acc))
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 694).
-spec concat(list(list(AFA))) -> list(AFA).
concat(Lists) ->
    concat_loop(Lists, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 718).
-spec flatten(list(list(AFJ))) -> list(AFJ).
flatten(Lists) ->
    concat_loop(Lists, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 731).
-spec flat_map(list(AFN), fun((AFN) -> list(AFP))) -> list(AFP).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    flatten(_pipe).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 744).
-spec fold(list(AFS), AFU, fun((AFU, AFS) -> AFU)) -> AFU.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 90).
-spec count(list(YO), fun((YO) -> boolean())) -> integer().
count(List, Predicate) ->
    fold(List, 0, fun(Acc, Value) -> case Predicate(Value) of
                true ->
                    Acc + 1;

                false ->
                    Acc
            end end).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 288).
-spec group(list(AAK), fun((AAK) -> AAM)) -> gleam@dict:dict(AAM, list(AAK)).
group(List, Key) ->
    fold(List, maps:new(), update_group(Key)).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 435).
-spec map_fold(list(ACQ), ACS, fun((ACS, ACQ) -> {ACS, ACT})) -> {ACS,
    list(ACT)}.
map_fold(List, Initial, Fun) ->
    _pipe = fold(
        List,
        {Initial, []},
        fun(Acc, Item) ->
            {Current_acc, Items} = Acc,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun lists:reverse/1).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 766).
-spec fold_right(list(AFV), AFX, fun((AFX, AFV) -> AFX)) -> AFX.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 794).
-spec index_fold_loop(
    list(AGB),
    AGD,
    fun((AGD, AGB, integer()) -> AGD),
    integer()
) -> AGD.
index_fold_loop(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            index_fold_loop(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 786).
-spec index_fold(list(AFY), AGA, fun((AGA, AFY, integer()) -> AGA)) -> AGA.
index_fold(List, Initial, Fun) ->
    index_fold_loop(List, Initial, Fun, 0).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 826).
-spec try_fold(list(AGE), AGG, fun((AGG, AGE) -> {ok, AGG} | {error, AGH})) -> {ok,
        AGG} |
    {error, AGH}.
try_fold(List, Initial, Fun) ->
    case List of
        [] ->
            {ok, Initial};

        [First | Rest] ->
            case Fun(Initial, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 865).
-spec fold_until(list(AGM), AGO, fun((AGO, AGM) -> continue_or_stop(AGO))) -> AGO.
fold_until(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [First | Rest] ->
            case Fun(Initial, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 902).
-spec find(list(AGQ), fun((AGQ) -> boolean())) -> {ok, AGQ} | {error, nil}.
find(List, Is_desired) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, Is_desired)
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 938).
-spec find_map(list(AGU), fun((AGU) -> {ok, AGW} | {error, any()})) -> {ok, AGW} |
    {error, nil}.
find_map(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 973).
-spec all(list(AHC), fun((AHC) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    all(Rest, Predicate);

                false ->
                    false
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1010).
-spec any(list(AHE), fun((AHE) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    true;

                false ->
                    any(Rest, Predicate)
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1052).
-spec zip_loop(list(AHL), list(AHN), list({AHL, AHN})) -> list({AHL, AHN}).
zip_loop(One, Other, Acc) ->
    case {One, Other} of
        {[First_one | Rest_one], [First_other | Rest_other]} ->
            zip_loop(Rest_one, Rest_other, [{First_one, First_other} | Acc]);

        {_, _} ->
            lists:reverse(Acc)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1048).
-spec zip(list(AHG), list(AHI)) -> list({AHG, AHI}).
zip(List, Other) ->
    zip_loop(List, Other, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1086).
-spec strict_zip(list(AHR), list(AHT)) -> {ok, list({AHR, AHT})} | {error, nil}.
strict_zip(List, Other) ->
    case erlang:length(List) =:= erlang:length(Other) of
        true ->
            {ok, zip(List, Other)};

        false ->
            {error, nil}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1114).
-spec unzip_loop(list({AID, AIE}), list(AID), list(AIE)) -> {list(AID),
    list(AIE)}.
unzip_loop(Input, One, Other) ->
    case Input of
        [] ->
            {lists:reverse(One), lists:reverse(Other)};

        [{First_one, First_other} | Rest] ->
            unzip_loop(Rest, [First_one | One], [First_other | Other])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1110).
-spec unzip(list({AHY, AHZ})) -> {list(AHY), list(AHZ)}.
unzip(Input) ->
    unzip_loop(Input, [], []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1149).
-spec intersperse_loop(list(AIN), AIN, list(AIN)) -> list(AIN).
intersperse_loop(List, Separator, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Rest] ->
            intersperse_loop(Rest, Separator, [X, Separator | Acc])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1142).
-spec intersperse(list(AIK), AIK) -> list(AIK).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            intersperse_loop(Rest, Elem, [X])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1167).
-spec unique(list(AIR)) -> list(AIR).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1248).
-spec sequences(
    list(AIX),
    fun((AIX, AIX) -> gleam@order:order()),
    list(AIX),
    sorting(),
    AIX,
    list(list(AIX))
) -> list(list(AIX)).
sequences(List, Compare, Growing, Direction, Prev, Acc) ->
    Growing@1 = [Prev | Growing],
    case List of
        [] ->
            case Direction of
                ascending ->
                    [reverse_loop(Growing@1, []) | Acc];

                descending ->
                    [Growing@1 | Acc]
            end;

        [New | Rest] ->
            case {Compare(Prev, New), Direction} of
                {gt, descending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {lt, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {eq, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {gt, ascending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [reverse_loop(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {lt, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [reverse_loop(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {eq, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [reverse_loop(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1396).
-spec merge_ascendings(
    list(AJU),
    list(AJU),
    fun((AJU, AJU) -> gleam@order:order()),
    list(AJU)
) -> list(AJU).
merge_ascendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            reverse_loop(List, Acc);

        {List, []} ->
            reverse_loop(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_ascendings(Rest1, List2, Compare, [First1 | Acc]);

                gt ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc]);

                eq ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc])
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1349).
-spec merge_ascending_pairs(
    list(list(AJI)),
    fun((AJI, AJI) -> gleam@order:order()),
    list(list(AJI))
) -> list(list(AJI)).
merge_ascending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            reverse_loop(Acc, []);

        [Sequence] ->
            reverse_loop([reverse_loop(Sequence, []) | Acc], []);

        [Ascending1, Ascending2 | Rest] ->
            Descending = merge_ascendings(Ascending1, Ascending2, Compare, []),
            merge_ascending_pairs(Rest, Compare, [Descending | Acc])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1423).
-spec merge_descendings(
    list(AJZ),
    list(AJZ),
    fun((AJZ, AJZ) -> gleam@order:order()),
    list(AJZ)
) -> list(AJZ).
merge_descendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            reverse_loop(List, Acc);

        {List, []} ->
            reverse_loop(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_descendings(List1, Rest2, Compare, [First2 | Acc]);

                gt ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc]);

                eq ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc])
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1371).
-spec merge_descending_pairs(
    list(list(AJO)),
    fun((AJO, AJO) -> gleam@order:order()),
    list(list(AJO))
) -> list(list(AJO)).
merge_descending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            reverse_loop(Acc, []);

        [Sequence] ->
            reverse_loop([reverse_loop(Sequence, []) | Acc], []);

        [Descending1, Descending2 | Rest] ->
            Ascending = merge_descendings(Descending1, Descending2, Compare, []),
            merge_descending_pairs(Rest, Compare, [Ascending | Acc])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1315).
-spec merge_all(
    list(list(AJE)),
    sorting(),
    fun((AJE, AJE) -> gleam@order:order())
) -> list(AJE).
merge_all(Sequences, Direction, Compare) ->
    case {Sequences, Direction} of
        {[], _} ->
            [];

        {[Sequence], ascending} ->
            Sequence;

        {[Sequence@1], descending} ->
            reverse_loop(Sequence@1, []);

        {_, ascending} ->
            Sequences@1 = merge_ascending_pairs(Sequences, Compare, []),
            merge_all(Sequences@1, descending, Compare);

        {_, descending} ->
            Sequences@2 = merge_descending_pairs(Sequences, Compare, []),
            merge_all(Sequences@2, ascending, Compare)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1186).
-spec sort(list(AIU), fun((AIU, AIU) -> gleam@order:order())) -> list(AIU).
sort(List, Compare) ->
    case List of
        [] ->
            [];

        [X] ->
            [X];

        [X@1, Y | Rest] ->
            Direction = case Compare(X@1, Y) of
                lt ->
                    ascending;

                eq ->
                    ascending;

                gt ->
                    descending
            end,
            Sequences = sequences(Rest, Compare, [X@1], Direction, Y, []),
            merge_all(Sequences, ascending, Compare)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1463).
-spec range_loop(integer(), integer(), list(integer())) -> list(integer()).
range_loop(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [Stop | Acc];

        gt ->
            range_loop(Start, Stop + 1, [Stop | Acc]);

        lt ->
            range_loop(Start, Stop - 1, [Stop | Acc])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1459).
-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    range_loop(Start, Stop, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1489).
-spec repeat_loop(AKJ, integer(), list(AKJ)) -> list(AKJ).
repeat_loop(Item, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            repeat_loop(Item, Times - 1, [Item | Acc])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1485).
-spec repeat(AKH, integer()) -> list(AKH).
repeat(A, Times) ->
    repeat_loop(A, Times, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1522).
-spec split_loop(list(AKQ), integer(), list(AKQ)) -> {list(AKQ), list(AKQ)}.
split_loop(List, N, Taken) ->
    case N =< 0 of
        true ->
            {lists:reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {lists:reverse(Taken), []};

                [First | Rest] ->
                    split_loop(Rest, N - 1, [First | Taken])
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1518).
-spec split(list(AKM), integer()) -> {list(AKM), list(AKM)}.
split(List, Index) ->
    split_loop(List, Index, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1558).
-spec split_while_loop(list(AKZ), fun((AKZ) -> boolean()), list(AKZ)) -> {list(AKZ),
    list(AKZ)}.
split_while_loop(List, F, Acc) ->
    case List of
        [] ->
            {lists:reverse(Acc), []};

        [First | Rest] ->
            case F(First) of
                false ->
                    {lists:reverse(Acc), List};

                _ ->
                    split_while_loop(Rest, F, [First | Acc])
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1551).
-spec split_while(list(AKV), fun((AKV) -> boolean())) -> {list(AKV), list(AKV)}.
split_while(List, Predicate) ->
    split_while_loop(List, Predicate, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1598).
-spec key_find(list({ALE, ALF}), ALE) -> {ok, ALF} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1629).
-spec key_filter(list({ALJ, ALK}), ALJ) -> list(ALK).
key_filter(Keyword_list, Desired_key) ->
    filter_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1670).
-spec pop_loop(list(BDS), fun((BDS) -> boolean()), list(BDS)) -> {ok,
        {BDS, list(BDS)}} |
    {error, nil}.
pop_loop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, lists:append(lists:reverse(Checked), Rest)}};

                false ->
                    pop_loop(Rest, Predicate, [X | Checked])
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1663).
-spec pop(list(ALN), fun((ALN) -> boolean())) -> {ok, {ALN, list(ALN)}} |
    {error, nil}.
pop(List, Is_desired) ->
    pop_loop(List, Is_desired, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1710).
-spec pop_map_loop(
    list(AMF),
    fun((AMF) -> {ok, AMH} | {error, any()}),
    list(AMF)
) -> {ok, {AMH, list(AMF)}} | {error, nil}.
pop_map_loop(List, Mapper, Checked) ->
    case List of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, lists:append(lists:reverse(Checked), Rest)}};

                {error, _} ->
                    pop_map_loop(Rest, Mapper, [X | Checked])
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1703).
-spec pop_map(list(ALW), fun((ALW) -> {ok, ALY} | {error, any()})) -> {ok,
        {ALY, list(ALW)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    pop_map_loop(Haystack, Is_desired, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1748).
-spec key_pop(list({AMP, AMQ}), AMP) -> {ok, {AMQ, list({AMP, AMQ})}} |
    {error, nil}.
key_pop(List, Key) ->
    pop_map(
        List,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end
        end
    ).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1775).
-spec key_set(list({AMV, AMW}), AMV, AMW) -> list({AMV, AMW}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1797).
-spec each(list(AMZ), fun((AMZ) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [First | Rest] ->
            F(First),
            each(Rest, F)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1823).
-spec try_each(list(ANC), fun((ANC) -> {ok, any()} | {error, ANF})) -> {ok, nil} |
    {error, ANF}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [First | Rest] ->
            case Fun(First) of
                {ok, _} ->
                    try_each(Rest, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1856).
-spec partition_loop(list(BFX), fun((BFX) -> boolean()), list(BFX), list(BFX)) -> {list(BFX),
    list(BFX)}.
partition_loop(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {lists:reverse(Trues), lists:reverse(Falses)};

        [First | Rest] ->
            case Categorise(First) of
                true ->
                    partition_loop(Rest, Categorise, [First | Trues], Falses);

                false ->
                    partition_loop(Rest, Categorise, Trues, [First | Falses])
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1849).
-spec partition(list(ANK), fun((ANK) -> boolean())) -> {list(ANK), list(ANK)}.
partition(List, Categorise) ->
    partition_loop(List, Categorise, [], []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1876).
-spec permutations(list(ANT)) -> list(list(ANT)).
permutations(List) ->
    case List of
        [] ->
            [[]];

        _ ->
            _pipe@3 = index_map(
                List,
                fun(I, I_idx) ->
                    _pipe = index_fold(
                        List,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@1 = lists:reverse(_pipe),
                    _pipe@2 = permutations(_pipe@1),
                    map(_pipe@2, fun(Permutation) -> [I | Permutation] end)
                end
            ),
            flatten(_pipe@3)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1916).
-spec window_loop(list(list(AOB)), list(AOB), integer()) -> list(list(AOB)).
window_loop(Acc, List, N) ->
    Window = take(List, N),
    case erlang:length(Window) =:= N of
        true ->
            window_loop([Window | Acc], drop(List, 1), N);

        false ->
            lists:reverse(Acc)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1909).
-spec window(list(ANX), integer()) -> list(list(ANX)).
window(List, N) ->
    case N =< 0 of
        true ->
            [];

        false ->
            window_loop([], List, N)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1939).
-spec window_by_2(list(AOH)) -> list({AOH, AOH}).
window_by_2(List) ->
    zip(List, drop(List, 1)).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1952).
-spec drop_while(list(AOK), fun((AOK) -> boolean())) -> list(AOK).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    drop_while(Rest, Predicate);

                false ->
                    [First | Rest]
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1982).
-spec take_while_loop(list(AOQ), fun((AOQ) -> boolean()), list(AOQ)) -> list(AOQ).
take_while_loop(List, Predicate, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    take_while_loop(Rest, Predicate, [First | Acc]);

                false ->
                    lists:reverse(Acc)
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 1975).
-spec take_while(list(AON), fun((AON) -> boolean())) -> list(AON).
take_while(List, Predicate) ->
    take_while_loop(List, Predicate, []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2014).
-spec chunk_loop(list(AOZ), fun((AOZ) -> APB), APB, list(AOZ), list(list(AOZ))) -> list(list(AOZ)).
chunk_loop(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [First | Rest] ->
            Key = F(First),
            case Key =:= Previous_key of
                false ->
                    New_acc = [lists:reverse(Current_chunk) | Acc],
                    chunk_loop(Rest, F, Key, [First], New_acc);

                _ ->
                    chunk_loop(Rest, F, Key, [First | Current_chunk], Acc)
            end;

        _ ->
            lists:reverse([lists:reverse(Current_chunk) | Acc])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2007).
-spec chunk(list(AOU), fun((AOU) -> any())) -> list(list(AOU)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            chunk_loop(Rest, F, F(First), [First], [])
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2059).
-spec sized_chunk_loop(
    list(APL),
    integer(),
    integer(),
    list(APL),
    list(list(APL))
) -> list(list(APL)).
sized_chunk_loop(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    lists:reverse(Acc);

                Remaining ->
                    lists:reverse([lists:reverse(Remaining) | Acc])
            end;

        [First | Rest] ->
            Chunk = [First | Current_chunk],
            case Left > 1 of
                true ->
                    sized_chunk_loop(Rest, Count, Left - 1, Chunk, Acc);

                false ->
                    sized_chunk_loop(
                        Rest,
                        Count,
                        Count,
                        [],
                        [lists:reverse(Chunk) | Acc]
                    )
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2055).
-spec sized_chunk(list(APH), integer()) -> list(list(APH)).
sized_chunk(List, Count) ->
    sized_chunk_loop(List, Count, Count, [], []).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2103).
-spec reduce(list(APS), fun((APS, APS) -> APS)) -> {ok, APS} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, fold(Rest, First, Fun)}
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2127).
-spec scan_loop(list(AQA), AQC, list(AQC), fun((AQC, AQA) -> AQC)) -> list(AQC).
scan_loop(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            lists:reverse(Accumulated);

        [First | Rest] ->
            Next = Fun(Accumulator, First),
            scan_loop(Rest, Next, [Next | Accumulated], Fun)
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2119).
-spec scan(list(APW), APY, fun((APY, APW) -> APY)) -> list(APY).
scan(List, Initial, Fun) ->
    scan_loop(List, Initial, [], Fun).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2160).
-spec last(list(AQF)) -> {ok, AQF} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2179).
-spec combinations(list(AQJ), integer()) -> list(list(AQJ)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _ ->
            case Items of
                [] ->
                    [];

                [First | Rest] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Rest, N - 1),
                            fun(Com) -> [First | Com] end
                        ),
                        lists:reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Rest, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2211).
-spec combination_pairs_loop(list(AQQ)) -> list(list({AQQ, AQQ})).
combination_pairs_loop(Items) ->
    case Items of
        [] ->
            [];

        [First | Rest] ->
            First_combinations = map(Rest, fun(Other) -> {First, Other} end),
            [First_combinations | combination_pairs_loop(Rest)]
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2206).
-spec combination_pairs(list(AQN)) -> list({AQN, AQN}).
combination_pairs(Items) ->
    _pipe = combination_pairs_loop(Items),
    flatten(_pipe).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2248).
-spec transpose(list(list(AQY))) -> list(list(AQY)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Rest] ->
            transpose(Rest);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                flatten(_pipe@1)
            end,
            Rest@1 = transpose(
                map(Rows, fun(_capture) -> drop(_capture, 1) end)
            ),
            [Firsts | Rest@1]
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2230).
-spec interleave(list(list(AQU))) -> list(AQU).
interleave(List) ->
    _pipe = transpose(List),
    flatten(_pipe).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2289).
-spec shuffle_pair_unwrap_loop(list({float(), ARG}), list(ARG)) -> list(ARG).
shuffle_pair_unwrap_loop(List, Acc) ->
    case List of
        [] ->
            Acc;

        [Elem_pair | Enumerable] ->
            shuffle_pair_unwrap_loop(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2297).
-spec do_shuffle_by_pair_indexes(list({float(), ARK})) -> list({float(), ARK}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/list.gleam", 2282).
-spec shuffle(list(ARD)) -> list(ARD).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(_pipe, [], fun(Acc, A) -> [{rand:uniform(), A} | Acc] end),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    shuffle_pair_unwrap_loop(_pipe@2, []).
