import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result

pub type Pair =
  #(Int, Int)

pub fn similarity(pairs: List(Pair)) {
  // Keep track of the counts of the right list
  let right_counts =
    list.fold(pairs, dict.new(), fn(acc, pair) {
      let #(_, b) = pair

      // Get the count from the dict, otherwise default at 0 (and increment to 1 below)
      let count = result.unwrap(dict.get(acc, b), 0)
      dict.insert(acc, b, count + 1)
    })

  // Calculate the similarity
  let similarity =
    list.fold(pairs, 0, fn(acc, pair) {
      let #(a, _) = pair

      let count = result.unwrap(dict.get(right_counts, a), 0)
      acc + { count * a }
    })

  io.println("Similarity: " <> int.to_string(similarity))
}
