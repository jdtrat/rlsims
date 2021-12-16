test_agent <- rl_new_agent(model_type = "tdrlConditioning",
                           model_id = "Testing Example",
                           num_trials = 50,
                           num_episodes = 10,
                           gamma = 1,
                           alpha = 0.05)

test_that("cue error catching works - invalid trial length", {
  expect_error(
    test_agent$set_cues(
      list(one =
             data.frame(
               onset = 6,
               offset = 8,
               magnitude = 1,
               trial = 1:50
               ),
           two =
             data.frame(
               onset = 7,
               offset = 8,
               magnitude = 1,
               trial = 1:500
             )
           )
      ),
    regexp = "value per trial"
    )
})

test_that("cue error catching works - invalid data frame names", {
  expect_error(
    test_agent$set_cues(
      list(one =
             data.frame(
               onset = 6,
               offset = 8,
               magnitudee = 1,
               trial = 1:50
             ),
           two =
             data.frame(
               onset = 7,
               offset = 8,
               magnitude = 1,
               trial = 1:50
             )
      )
    ),
    regexp = "columns named"
  )
})

test_that("cue error catching works - invalid onset and offset values", {
  expect_error(
    test_agent$set_cues(
      list(one =
             data.frame(
               onset = 60,
               offset = 8,
               magnitude = 1,
               trial = 1:50
             ),
           two =
             data.frame(
               onset = 7,
               offset = 8,
               magnitude = 1,
               trial = 1:50
             )
      )
    )
  )

  expect_error(
    test_agent$set_cues(
      list(one =
             data.frame(
               onset = 6,
               offset = 8,
               magnitude = 1,
               trial = 1:50
             ),
           two =
             data.frame(
               onset = 7,
               offset = 18,
               magnitude = 1,
               trial = 1:50
             )
      )
    )
  )

})




