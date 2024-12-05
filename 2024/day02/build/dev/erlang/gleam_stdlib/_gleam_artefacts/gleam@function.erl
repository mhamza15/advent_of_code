-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 2).
-spec compose(fun((DOG) -> DOH), fun((DOH) -> DOI)) -> fun((DOG) -> DOI).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 7).
-spec curry2(fun((DOJ, DOK) -> DOL)) -> fun((DOJ) -> fun((DOK) -> DOL)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 12).
-spec curry3(fun((DON, DOO, DOP) -> DOQ)) -> fun((DON) -> fun((DOO) -> fun((DOP) -> DOQ))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 17).
-spec curry4(fun((DOS, DOT, DOU, DOV) -> DOW)) -> fun((DOS) -> fun((DOT) -> fun((DOU) -> fun((DOV) -> DOW)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 22).
-spec curry5(fun((DOY, DOZ, DPA, DPB, DPC) -> DPD)) -> fun((DOY) -> fun((DOZ) -> fun((DPA) -> fun((DPB) -> fun((DPC) -> DPD))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 27).
-spec curry6(fun((DPF, DPG, DPH, DPI, DPJ, DPK) -> DPL)) -> fun((DPF) -> fun((DPG) -> fun((DPH) -> fun((DPI) -> fun((DPJ) -> fun((DPK) -> DPL)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 36).
-spec flip(fun((DPN, DPO) -> DPP)) -> fun((DPO, DPN) -> DPP).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 42).
-spec identity(DPQ) -> DPQ.
identity(X) ->
    X.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 47).
-spec constant(DPR) -> fun((any()) -> DPR).
constant(Value) ->
    fun(_) -> Value end.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 56).
-spec tap(DPT, fun((DPT) -> any())) -> DPT.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 62).
-spec apply1(fun((DPV) -> DPW), DPV) -> DPW.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 67).
-spec apply2(fun((DPX, DPY) -> DPZ), DPX, DPY) -> DPZ.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-file("/Users/hove/dev/advent_of_code/2024/day02/build/packages/gleam_stdlib/src/gleam/function.gleam", 72).
-spec apply3(fun((DQA, DQB, DQC) -> DQD), DQA, DQB, DQC) -> DQD.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
