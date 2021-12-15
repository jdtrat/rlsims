test_agent <- rl_new_agent(model_type = "tdrl",
                           num_trials = 50,
                           num_episodes = 10,
                           num_cues = 1,
                           gamma = 1,
                           alpha = 0.05)

reward_set <- test_agent$clone()$set_reward(8,8,1)
cue_set <- test_agent$clone()$set_cues(
  list(
      one = data.frame(
        cue = 1,
        onset = 4,
        offset = 8,
        trial = 1:50)
      )
)

both_set <- test_agent$clone()$
  set_reward(8,8,1)$
  set_cues(
    list(
      one = data.frame(
        cue = 1,
        onset = 4,
        offset = 8,
        trial = 1:50)
    )
  )

test_that("reward and cues before simulating", {
  expect_error(test_agent$simulate_agent(),
               regexp = "set the reward and cue")
})

test_that("set cues only before simulating", {

  expect_error(reward_set$simulate_agent(),
               regexp = "set the cue")
})

test_that("set rewards only before simulating", {

  expect_error(cue_set$simulate_agent(),
               regexp = "set the reward")
})

test_that("simulation works", {
  expect_silent(both_set$simulate_agent())
})



