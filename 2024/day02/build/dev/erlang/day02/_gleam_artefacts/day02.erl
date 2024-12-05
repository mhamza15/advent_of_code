-module(day02).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-file("/Users/hove/dev/advent_of_code/2024/day02/src/day02.gleam", 31).
-spec parse_reports(binary()) -> list(list(integer())).
parse_reports(Str) ->
    _pipe = Str,
    _pipe@1 = gleam@string:split(_pipe, <<"\n"/utf8>>),
    _pipe@2 = gleam@list:filter(
        _pipe@1,
        fun(X) -> not gleam@string:is_empty(X) end
    ),
    _pipe@3 = gleam@list:map(
        _pipe@2,
        fun(_capture) -> gleam@string:split(_capture, <<" "/utf8>>) end
    ),
    gleam@list:map(
        _pipe@3,
        fun(Reports) ->
            gleam@list:map(
                Reports,
                fun(X@1) ->
                    _assert_subject = gleam_stdlib:parse_int(X@1),
                    {ok, Val} = case _assert_subject of
                        {ok, _} -> _assert_subject;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        value => _assert_fail,
                                        module => <<"day02"/utf8>>,
                                        function => <<"parse_reports"/utf8>>,
                                        line => 38})
                    end,
                    Val
                end
            )
        end
    ).

-file("/Users/hove/dev/advent_of_code/2024/day02/src/day02.gleam", 62).
-spec is_safe_windows(list({integer(), integer()}), {boolean(), boolean()}) -> boolean().
is_safe_windows(Pairs, State) ->
    case {Pairs, State} of
        {_, {true, true}} ->
            false;

        {[], _} ->
            true;

        {[Pair | Rest], {Incremented, Decremented}} ->
            {A, B} = Pair,
            Diff = gleam@int:absolute_value(A - B),
            gleam@bool:guard(
                (Diff < 1) orelse (Diff > 3),
                false,
                fun() -> case gleam@int:compare(A, B) of
                        lt ->
                            is_safe_windows(Rest, {true, Decremented});

                        gt ->
                            is_safe_windows(Rest, {Incremented, true});

                        eq ->
                            is_safe_windows(Rest, {Incremented, Decremented})
                    end end
            )
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/src/day02.gleam", 56).
-spec is_safe(list(integer())) -> boolean().
is_safe(Report) ->
    _pipe = Report,
    _pipe@1 = gleam@list:window_by_2(_pipe),
    is_safe_windows(_pipe@1, {false, false}).

-file("/Users/hove/dev/advent_of_code/2024/day02/src/day02.gleam", 45).
-spec safe_reports(list(list(integer()))) -> integer().
safe_reports(Reports) ->
    gleam@list:fold(
        Reports,
        0,
        fun(Num_safe_reports, Report) -> case is_safe(Report) of
                true ->
                    Num_safe_reports + 1;

                false ->
                    Num_safe_reports
            end end
    ).

-file("/Users/hove/dev/advent_of_code/2024/day02/src/day02.gleam", 17).
-spec main() -> nil.
main() ->
    _assert_subject = erlang:element(4, argv:load()),
    [Input_filepath] = case _assert_subject of
        [_] -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"day02"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 20})
    end,
    _assert_subject@1 = simplifile:read(Input_filepath),
    {ok, File_contents} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@1,
                        module => <<"day02"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 21})
    end,
    Num_safe_reports = begin
        _pipe = parse_reports(File_contents),
        safe_reports(_pipe)
    end,
    gleam_stdlib:println(
        <<"Safe reports: "/utf8,
            (erlang:integer_to_binary(Num_safe_reports))/binary>>
    ).
