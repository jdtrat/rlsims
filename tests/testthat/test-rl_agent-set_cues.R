test_agent <- rl_agent_new(model_type = "tdrl",
                           num_trials = 50,
                           num_episodes = 10,
                           num_cues = 2,
                           gamma = 1,
                           alpha = 0.05)

test_that("cue error catching works - invalid trial length", {
  expect_error(
    test_agent$set_cues(
      list(one =
             data.frame(
               cue = 1,
               onset = 6,
               offset = 8,
               trial = 1:50
               ),
           two =
             data.frame(
               cue = 2,
               onset = 7,
               offset = 8,
               trial = 1:500
             )
           )
      ),
    regexp = "values for the correct number of trials"
    )
})

test_that("cue error catching works - invalid data frame names", {
  expect_error(
    test_agent$set_cues(
      list(one =
             data.frame(
               cuee = 1,
               onset = 6,
               offset = 8,
               trial = 1:50
             ),
           two =
             data.frame(
               cue = 2,
               onset = 7,
               offset = 8,
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
               cue = 1,
               onset = 60,
               offset = 8,
               trial = 1:50
             ),
           two =
             data.frame(
               cue = 2,
               onset = 7,
               offset = 8,
               trial = 1:50
             )
      )
    )
  )

  expect_error(
    test_agent$set_cues(
      list(one =
             data.frame(
               cue = 1,
               onset = 6,
               offset = 8,
               trial = 1:50
             ),
           two =
             data.frame(
               cue = 2,
               onset = 7,
               offset = 18,
               trial = 1:50
             )
      )
    )
  )

})




