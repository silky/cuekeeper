/* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. */

open model

one sig base, a, b extends Rev {} { valid[this] }
one sig result extends Rev {}

pred example_merge {
  Node = Rev.my_nodes
  all r : Rev | #r.child_nodes > 1
  merge [base, a, b, result]
  not equal[base, result]
  not equal[base, a]
  not equal[base, b]
  not equal[a, b]
  some r : Rev | some r.contact
  some r : Rev | some r.context
  some r : Rev | some r.actions
}
run example_merge for exactly 4 Rev, 8 Node

// We can make stricter checks for merges where two of the commits are
// the same.
assert trivial {
  merge [base, a, b, result] => {
    equal [a, base] => equal [b, result]
    equal [b, base] => equal [a, result]
    equal [a, b] => equal [a, result]
  }
}
check trivial for 4 Rev, 6 Node

// Merging two valid heads with a valid base must result in a valid
// result.
assert merge_valid {
  merge [base, a, b, result] => valid [result]
}
check merge_valid for 4 Rev, 6 Node
