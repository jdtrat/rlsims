

test_that("policy checks work for valid requests", {
  expect_silent(
    rl_policy_check_internal(
      rl_set_policy_internal("softmax", tau = 2)
    )
  )
  expect_silent(
    rl_policy_check_internal(
      rl_set_policy_internal("epsilonGreedy", epsilon = 0.2)
    )
  )
  expect_silent(
    rl_policy_check_internal(
      rl_set_policy_internal("greedy")
    )
  )

  expect_message(
    # Tried to pass a parameter to greedy policy, message ignoring it
    rl_policy_check_internal(
      rl_set_policy_internal("greedy", tau = 2)
    )
  )

  expect_message(
    # Tried to pass a parameter to greedy policy, message ignoring it
    rl_policy_check_internal(
      rl_set_policy_internal("greedy", epsilon = 2)
    )
  )
})

test_that("policy checks work for invalid requests", {
  expect_error(
    # epsilonGreedy requires epsilon parameter, not tau
    rl_policy_check_internal(
      rl_set_policy_internal("epsilonGreedy", tau = 2)
    )
  )
  expect_error(
    # epsilon must be between 0 and 1
    rl_policy_check_internal(
      rl_set_policy_internal("epsilonGreedy", epsilon = 2)
    )
  )
  expect_error(
    # epsilonGreedy policy only accepts one epsilon value
    rl_policy_check_internal(
      rl_set_policy_internal("epsilonGreedy", epsilon = c(0.1, 0.2, 0.3))
    )
  )
  expect_error(
    # epsilon must be numeric
    rl_policy_check_internal(
      rl_set_policy_internal("epsilonGreedy", epsilon = "0.5")
    )
  )

  expect_error(
    # tau must be greater than zero
    rl_policy_check_internal(
      rl_set_policy_internal("softmax", tau = 0)
    )
  )
  expect_error(
    # softmax policy requires a numeric value
    rl_policy_check_internal(
      rl_set_policy_internal("softmax", tau = "zero")
    )
  )
  expect_error(
    # softmax policy only accepts one tau value
    rl_policy_check_internal(
      rl_set_policy_internal("softmax", tau = c(0.1, 1.2, 3))
    )
  )
})
