test_agent <- rl_agent_new(model_type = "tdrl",
                           num_trials = 50,
                           num_episodes = 10,
                           num_cues = 2,
                           gamma = 1,
                           alpha = 0.05)

test_that("reward onset and offset error catching works", {
  expect_error(test_agent$set_reward(reward_onset = 8,
                                     reward_offset = 5,
                                     reward_magnitude = 1,
                                     keep_reward_structure = FALSE),
               regexp = "`reward_onset` set to occur after `reward_offset`"
               )
})

test_that("reward length checks work", {

  expect_silent(
    test_agent$set_reward(reward_onset = rep(5, 50),
                          reward_offset = rep(6, 50),
                          reward_magnitude = rep(1, 50),
                          keep_reward_structure = FALSE)
  )

  expect_silent(
    test_agent$set_reward(reward_onset = 5,
                          reward_offset = 6,
                          reward_magnitude = 1,
                          keep_reward_structure = FALSE)
  )

  expect_error(test_agent$set_reward(reward_onset = rep(5, 49),
                                     reward_offset = rep(6, 50),
                                     reward_magnitude = rep(1, 50),
                                     keep_reward_structure = FALSE),
               regexp = "must be of length"
  )

  expect_error(test_agent$set_reward(reward_onset = 5,
                                     reward_offset = 6,
                                     reward_magnitude = rep(2, 50),
                                     keep_reward_structure = FALSE),
               regexp = "must be of length"
  )

})

