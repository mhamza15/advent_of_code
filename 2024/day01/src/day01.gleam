import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import part02
import simplifile

pub type Pair =
  #(Int, Int)

pub fn main() {
  let result =
    process_args()
    |> result.try(read_file)
    |> result.try(parse)

  result
  |> result.map(part01)
  |> result.map(part02.similarity)
  |> result.map_error(fn(err) { io.println(err) })
}

fn part01(pairs: List(Pair)) {
  let diff =
    pairs
    |> difference

  io.println("Difference: " <> int.to_string(diff))
  pairs
}

fn process_args() {
  case argv.load().arguments {
    [input_filepath] -> Ok(input_filepath)
    _ -> Error("input filepath required")
  }
}

/// Read the file at the given path and return its contents.
fn read_file(filepath: String) -> Result(String, String) {
  case simplifile.read(from: filepath) {
    Ok(contents) -> Ok(contents)
    Error(_) -> Error("invalid input file")
  }
}

/// Parses the given string, returning the pair of integers on each line.
fn parse(contents: String) -> Result(List(Pair), String) {
  contents
  |> string.split("\n")
  |> list.filter(is_not_empty)
  |> list.map(to_pair(_))
  |> result.all
}

/// Split the given string on spaces, returning a pair of integers.
fn to_pair(str: String) -> Result(Pair, String) {
  let maybe_pair =
    str
    |> string.split(" ")
    |> list.filter(is_not_empty)

  case maybe_pair {
    [a, b] -> to_int_pair(a, b)
    _ -> Error("expected two integers on each line")
  }
}

/// Returns a `Pair`.
fn to_int_pair(a: String, b: String) -> Result(Pair, String) {
  use a <- result.try(
    int.parse(a)
    |> result.map_error(fn(_) { "failed to parse " <> a }),
  )

  use b <- result.try(
    int.parse(b)
    |> result.map_error(fn(_) { "failed to parse " <> b }),
  )

  Ok(#(a, b))
}

/// Returns true if the string is not empty.
fn is_not_empty(x) {
  !string.is_empty(x)
}

/// Calculates the difference between the values of each column.
fn difference(pairs: List(Pair)) {
  let #(a, b) =
    split(pairs)
    |> sort

  list.zip(a, b)
  |> list.fold(0, fn(acc, pair) { acc + int.absolute_value(pair.0 - pair.1) })
}

/// Splits the list of pairs into lists of each column.
fn split(pairs: List(Pair)) {
  list.fold(pairs, #([], []), fn(acc, pair) {
    #([pair.0, ..acc.0], [pair.1, ..acc.1])
  })
}

/// Sorts the two lists of columns.
fn sort(columns: #(List(Int), List(Int))) -> #(List(Int), List(Int)) {
  let #(a, b) = columns
  #(list.sort(a, int.compare), list.sort(b, int.compare))
}
