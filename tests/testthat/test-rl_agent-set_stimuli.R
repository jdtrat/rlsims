test_agent <- rl_define_new_agent(
  model_type = "TD Conditioning",
  simulation_code_file = use_agent_template("td_conditioning", read = FALSE),
  required_args = c("num_stimuli", "num_trials", "num_episodes", "gamma", "alpha"),
  required_methods = c("reinforcements", "stimuli")
)$new(
  model_id = "Testing Example",
  num_stimuli = 2,
  num_trials = 50,
  num_episodes = 10,
  gamma = 1,
  alpha = 0.05
)


test_that("stimulus error catching works - invalid trial length", {
  expect_error(
    test_agent$clone()$set_stimuli(
      list(
        one =
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

test_that("stimulus error catching works - invalid data frame names", {
  expect_error(
    test_agent$clone()$set_stimuli(
      list(
        one =
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

test_that("stimulus error catching works - invalid onset and offset values", {
  expect_error(
    test_agent$clone()$set_stimuli(
      list(
        one =
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
    test_agent$clone()$set_stimuli(
      list(
        one =
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

test_that("stimulus error catching works - invalid number of stimuli", {
  expect_error(
    test_agent$clone()$set_stimuli(
      list(
        one =
          data.frame(
            onset = 60,
            offset = 8,
            magnitude = 1,
            trial = 1:50
          )
      )
    )
  )

  expect_error(
    test_agent$clone()$set_stimuli(
      list(
        one =
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
          ),
        three =
          data.frame(
            onset = 5,
            offset = 8,
            magnitude = 1,
            trial = 1:50
          )
      )
    )
  )
})
