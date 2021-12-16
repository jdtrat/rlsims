test_agent <- rl_new_agent(model_type = "tdrlConditioning",
                           model_id = "Testing Example",
                           num_trials = 50,
                           num_episodes = 10,
                           gamma = 1,
                           alpha = 0.05)

test_that("reward onset and offset error catching works", {
  expect_error(test_agent$
                 set_reinforcements(list(
                   data.frame(
                     onset = 8,
                     offset = 6,
                     magnitude = 1,
                     trial = 1:50
                   )
                 ))
               )
})
