test_agent <- rl_new_agent(model_type = "tdrlConditioning",
                           model_id = "Testing Example",
                           num_trials = 50,
                           num_episodes = 10,
                           gamma = 1,
                           alpha = 0.05)

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

simulated <- both_set$clone()$simulate_agent()

test_that("simulation is required", {
  expect_error(both_set$get_pe_data(),
               regexp = "Cannot access prediction error data")
})

test_that("simulation is required", {
  expect_s3_class(simulated$get_pe_data(),
                  class = "data.frame")

  expect_named(simulated$get_pe_data(),
               c("trial", "episode", "value"))
})

test_that("appending trial zero works", {

  expect_equal(
    simulated$get_pe_data(add_trial_zero = TRUE, trial_zero_value = 0)[1:10,],
    data.frame(trial = 0, episode = 1:10, value = 0)
  )

  expect_equal(
    simulated$get_pe_data(add_trial_zero = FALSE, trial_zero_value = 0)[1:10,],
    data.frame(trial = 1, episode = 1:10, value = c(rep(0, 7), 1, 0, 0))
  )

  expect_equal(
    simulated$get_pe_data(add_trial_zero = TRUE, trial_zero_value = 1:10)[1:10,],
    data.frame(trial = 0, episode = 1:10, value = 1:10)
  )

})
