import argv
import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/order
import gleam/string
import simplifile

pub type Report =
  List(Int)

const lower_limit = 1

const upper_limit = 3

pub fn main() {
  // `let assert` panics if the pattern does not match. Normally we'd handle errors more gracefully like
  // we did in day01, but I don't feel like doing that anymore.
  let assert [input_filepath] = argv.load().arguments
  let assert Ok(file_contents) = simplifile.read(input_filepath)

  let num_safe_reports =
    parse_reports(file_contents)
    |> safe_reports

  io.println("Safe reports: " <> int.to_string(num_safe_reports))
}

/// Parse the string into a list of reports.
fn parse_reports(str: String) -> List(Report) {
  str
  |> string.split("\n")
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> list.map(string.split(_, " "))
  |> list.map(fn(reports) {
    list.map(reports, fn(x) {
      let assert Ok(val) = int.parse(x)
      val
    })
  })
}

/// Count the number of safe reports.
fn safe_reports(reports: List(Report)) -> Int {
  use num_safe_reports, report <- list.fold(reports, 0)

  case is_safe(report) {
    True -> num_safe_reports + 1
    False -> num_safe_reports
  }
}

/// Returns whether the report is safe. A report is safe if it is either incrementing
/// or decrementing the entire list, and by max of 2 for each integer.
fn is_safe(report: Report) -> Bool {
  report
  |> list.window_by_2
  |> is_safe_windows(#(False, False))
}

fn is_safe_windows(pairs: List(#(Int, Int)), state: #(Bool, Bool)) -> Bool {
  case pairs, state {
    // Base states:
    // Incremented and decremented should never be true at the same time.
    _, #(True, True) -> False

    // Empty list. If we didn't match on the above case, it means we reached the
    // end of the list successfully, and the report is safe.
    [], _ -> True

    // Calculate new state and recurse
    [pair, ..rest], #(incremented, decremented) -> {
      let #(a, b) = pair

      // Return early if difference is not within range
      let diff = int.absolute_value(a - b)
      use <- bool.guard(diff < lower_limit || diff > upper_limit, False)

      case int.compare(a, b) {
        // Incrementing case
        order.Lt -> is_safe_windows(rest, #(True, decremented))

        // Decrementing case
        order.Gt -> is_safe_windows(rest, #(incremented, True))

        // Equal
        order.Eq -> is_safe_windows(rest, #(incremented, decremented))
      }
    }
  }
}
