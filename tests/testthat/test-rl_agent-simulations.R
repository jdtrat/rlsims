test_agent <- rl_new_agent(model_type = "tdrlConditioning",
                           model_id = "Testing Example",
                           num_trials = 50,
                           num_episodes = 10,
                           gamma = 1,
                           alpha = 0.05)


reward_set <- test_agent$clone()$
  set_reinforcements(list(
  data.frame(
    onset = 8,
    offset = 8,
    magnitude = 1,
    trial = 1:50
  )
))

cue_set <- test_agent$clone()$set_cues(
  list(
      one = data.frame(
        onset = 4,
        offset = 8,
        magnitude = 1,
        trial = 1:50)
      )
)

both_set <- test_agent$clone()$
  set_reinforcements(list(
    data.frame(
      onset = 8,
      offset = 8,
      magnitude = 1,
      trial = 1:50
    )
  ))$
  set_cues(
    list(
      one = data.frame(
        onset = 4,
        offset = 8,
        magnitude = 1,
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



