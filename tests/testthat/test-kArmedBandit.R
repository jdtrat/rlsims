testK <- rl_new_agent(model_type = "kArmedBandit",
                      model_id = "test description",
                      num_trials = 100,
                      num_episodes = 3,
                      num_arms = 2,
                      action_episode = 1,
                      reinforcement_episode = 2,
                      gamma = 1,
                      alpha = 0.5)

arm_structure <- list(
  left = rl_arms_define(
    probability = 0.5,
    magnitude = 1,
    alternative = 0,
    trial = 1:100
  ),
  right = rl_arms_define(
    probability = 0.7,
    magnitude = 1,
    alternative = 0,
    trial = 1:100
  )
)


test_that("action and reinforcement episode checks work", {

  expect_error(
    rl_new_agent(model_type = "kArmedBandit",
                 model_id = "test description",
                 num_trials = 100,
                 num_episodes = 3,
                 num_arms = 2,
                 action_episode = 2,
                 reinforcement_episode = 2,
                 gamma = 1,
                 alpha = 0.5)
  )

  expect_error(
    rl_new_agent(model_type = "kArmedBandit",
                 model_id = "test description",
                 num_trials = 100,
                 num_episodes = 3,
                 num_arms = 2,
                 action_episode = 2,
                 reinforcement_episode = 1,
                 gamma = 1,
                 alpha = 0.5)
  )

  expect_error(
    rl_new_agent(model_type = "kArmedBandit",
                 model_id = "test description",
                 num_trials = 100,
                 num_episodes = 3,
                 num_arms = 2,
                 action_episode = 3,
                 reinforcement_episode = 1,
                 gamma = 1,
                 alpha = 0.5)
  )

  expect_error(
    rl_new_agent(model_type = "kArmedBandit",
                 model_id = "test description",
                 num_trials = 100,
                 num_episodes = 3,
                 num_arms = 2,
                 action_episode = 1,
                 reinforcement_episode = 3,
                 gamma = 1,
                 alpha = 0.5)
  )

  expect_silent(
    rl_new_agent(model_type = "kArmedBandit",
                 model_id = "test description",
                 num_trials = 100,
                 num_episodes = 4,
                 num_arms = 2,
                 action_episode = 2,
                 reinforcement_episode = 3,
                 gamma = 1,
                 alpha = 0.5)
  )

})

test_that("arm structure checking works", {

  more_arms_than_specified <- list(
    one = rl_arms_define(
      probability = 0.5,
      magnitude = 1,
      alternative = 0,
      trial = 1:100
    ),
    two = rl_arms_define(
      probability = 0.7,
      magnitude = 1,
      alternative = 0,
      trial = 1:100
    ),
    three = rl_arms_define(
      probability = 0.3,
      magnitude = 1,
      alternative = 0,
      trial = 1:100
    )
  )

  more_trials_than_specified <- list(
    one = rl_arms_define(
      probability = 0.5,
      magnitude = 1,
      alternative = 0,
      trial = 1:101
    ),
    two = rl_arms_define(
      probability = 0.7,
      magnitude = 1,
      alternative = 0,
      trial = 1:120
    )
  )


  expect_error(
    testK$clone()$set_arms(more_arms_than_specified)
  )

  expect_error(
    testK$clone()$set_arms(more_trials_than_specified)
  )

})

test_that("simulation needs both arm and policies", {

  arm_set <- testK$clone()$
    set_arms(arm_structure)

  policy_set <- testK$clone()$
    set_policy(policy = "softmax",
               tau = 1.2)

  both_set <- testK$clone()$
    set_arms(arm_structure)$
    set_policy(policy = "softmax",
               tau = 1.2)

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
  both_set <- testK$clone()$
    set_arms(arm_structure)$
    set_policy(policy = "softmax",
               tau = 1.2)

  expect_error(
    both_set$get_learned_values()
  )

  expect_named(
    both_set$simulate_agent()$get_learned_values(),
    c("trial", "episode", "arm", "q_value")
  )

})


test_that("get_pe_data works", {

  # Reset both_set
  both_set <- testK$clone()$
    set_arms(arm_structure)$
    set_policy(policy = "softmax",
               tau = 1.2)

  expect_error(
    both_set$get_pe_data()
  )

  expect_named(
    both_set$simulate_agent()$get_pe_data(),
    c("trial", "episode", "arm", "value")
  )

})
