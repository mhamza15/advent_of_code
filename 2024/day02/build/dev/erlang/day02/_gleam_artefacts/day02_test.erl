-module(day02_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0, hello_world_test/0]).

-file("/Users/hove/dev/advent_of_code/2024/day02/test/day02_test.gleam", 4).
-spec main() -> nil.
main() ->
    gleeunit:main().

-file("/Users/hove/dev/advent_of_code/2024/day02/test/day02_test.gleam", 9).
-spec hello_world_test() -> nil.
hello_world_test() ->
    _pipe = 1,
    gleeunit_ffi:should_equal(_pipe, 1).
