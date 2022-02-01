test_agent <- rl_define_new_agent(
  model_type = "K Arm Conditioning",
  simulation_code_file = use_agent_template("k_armed_bandit", read = FALSE),
  required_args = c(
    "num_arms", "num_trials", "num_episodes",
    "action_episode", "reinforcement_episode",
    "gamma", "alpha"
  ),
  required_methods = c("arms", "policy")
)

testCond <- test_agent$new(
  model_id = "test description",
  num_trials = 100,
  num_episodes = 3,
  num_arms = 2,
  action_episode = 1,
  reinforcement_episode = 2,
  gamma = 1,
  alpha = 0.5
)


arm_structure <- list(
  left = data.frame(
    probability = 0.5,
    magnitude = 1,
    alternative = 0,
    trial = 1:100
  ),
  right = data.frame(
    probability = 0.7,
    magnitude = 1,
    alternative = 0,
    trial = 1:100
  )
)


test_that("action and reinforcement episode checks work", {
  expect_error(
    test_agent$new(
      model_id = "test description",
      num_trials = 100,
      num_episodes = 3,
      num_arms = 2,
      action_episode = 2,
      reinforcement_episode = 2,
      gamma = 1,
      alpha = 0.5
    )
  )

  expect_error(
    test_agent$new(
      model_id = "test description",
      num_trials = 100,
      num_episodes = 3,
      num_arms = 2,
      action_episode = 2,
      reinforcement_episode = 1,
      gamma = 1,
      alpha = 0.5
    )
  )

  expect_error(
    test_agent$new(
      model_id = "test description",
      num_trials = 100,
      num_episodes = 3,
      num_arms = 2,
      action_episode = 3,
      reinforcement_episode = 1,
      gamma = 1,
      alpha = 0.5
    )
  )

  expect_error(
    test_agent$new(
      model_id = "test description",
      num_trials = 100,
      num_episodes = 3,
      num_arms = 2,
      action_episode = 1,
      reinforcement_episode = 3,
      gamma = 1,
      alpha = 0.5
    )
  )

  expect_silent(
    test_agent$new(
      model_id = "test description",
      num_trials = 100,
      num_episodes = 4,
      num_arms = 2,
      action_episode = 2,
      reinforcement_episode = 3,
      gamma = 1,
      alpha = 0.5
    )
  )
})

test_that("arm structure checking works", {
  more_arms_than_specified <- list(
    one = data.frame(
      probability = 0.5,
      magnitude = 1,
      alternative = 0,
      trial = 1:100
    ),
    two = data.frame(
      probability = 0.7,
      magnitude = 1,
      alternative = 0,
      trial = 1:100
    ),
    three = data.frame(
      probability = 0.3,
      magnitude = 1,
      alternative = 0,
      trial = 1:100
    )
  )

  more_trials_than_specified <- list(
    one = data.frame(
      probability = 0.5,
      magnitude = 1,
      alternative = 0,
      trial = 1:101
    ),
    two = data.frame(
      probability = 0.7,
      magnitude = 1,
      alternative = 0,
      trial = 1:120
    )
  )


  expect_error(
    testCond$clone()$set_arms(more_arms_than_specified)
  )

  expect_error(
    testCond$clone()$set_arms(more_trials_than_specified)
  )
})

test_that("simulation needs both arm and policies", {
  arm_set <- testCond$clone()$
    set_arms(
    list(
      left = data.frame(
        probability = 0.5,
        magnitude = 1,
        alternative = 0,
        trial = 1:100
      ),
      right = data.frame(
        probability = 0.7,
        magnitude = 1,
        alternative = 0,
        trial = 1:100
      )
    )
  )

  policy_set <- testCond$clone()$
    set_policy(
    policy = "softmax",
    tau = 1.2
  )

  both_set <- testCond$clone()$
    set_arms(
    list(
      left = data.frame(
        probability = 0.5,
        magnitude = 1,
        alternative = 0,
        trial = 1:100
      ),
      right = data.frame(
        probability = 0.7,
        magnitude = 1,
        alternative = 0,
        trial = 1:100
      )
    )
  )$
    set_policy(
    policy = "softmax",
    tau = 1.2
  )

  expect_error(
    arm_set$simulate_agent()
  )

  expect_error(
    policy_set$simulate_agent()
  )

  expect_silent(
    both_set$simulate_agent()
  )
})

test_that("get_learned_values works", {

  # Reset both_set
  both_set <- testCond$clone()$
    set_arms(arm_structure)$
    set_policy(
    policy = "softmax",
    tau = 1.2
  )

  expect_null(
    both_set$learned_values
  )

  expect_named(
    both_set$simulate_agent()$learned_values,
    c("trial", "episode", "arm", "q_value")
  )
})


test_that("get_pe_data works", {

  # Reset both_set
  both_set <- testCond$clone()$
    set_arms(arm_structure)$
    set_policy(
    policy = "softmax",
    tau = 1.2
  )

  expect_null(
    both_set$pe_data
  )

  expect_named(
    both_set$simulate_agent()$pe_data,
    c("trial", "episode", "arm", "value")
  )
})
