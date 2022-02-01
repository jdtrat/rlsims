
test_that("`rl_action-simulate` - greedy works", {
  expect_equal(
    rl_action_simulate("greedy", values = c(10, 5, 4, 5, 3, 5, 4)),
    1
  )
  expect_equal(
    rl_action_simulate("greedy", values = c(5, 10, 4, 5, 3, 5, 4)),
    2
  )
  expect_equal(
    rl_action_simulate("greedy", values = c(5, 4, 10, 5, 3, 5, 4)),
    3
  )
  expect_equal(
    rl_action_simulate("greedy", values = c(5, 4, 5, 10, 3, 5, 4)),
    4
  )
  expect_equal(
    rl_action_simulate("greedy", values = c(5, 4, 5, 3, 10, 5, 4)),
    5
  )
  expect_equal(
    rl_action_simulate("greedy", values = c(5, 4, 5, 3, 5, 10, 4)),
    6
  )
  expect_equal(
    rl_action_simulate("greedy", values = c(5, 4, 5, 3, 5, 4, 10)),
    7
  )
})
