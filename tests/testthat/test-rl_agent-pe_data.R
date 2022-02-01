test_agent <- rl_define_new_agent(
  model_type = "TD Conditioning",
  simulation_code_file = use_agent_template("td_conditioning", read = FALSE),
  required_args = c("num_stimuli", "num_trials", "num_episodes", "gamma", "alpha"),
  required_methods = c("reinforcements", "stimuli")
)$new(
  model_id = "Testing Example",
  num_stimuli = 1,
  num_trials = 50,
  num_episodes = 10,
  gamma = 1,
  alpha = 0.05
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
  set_stimuli(
  list(
    one = data.frame(
      onset = 4,
      offset = 8,
      magnitude = 1,
      trial = 1:50
    )
  )
)

simulated <- both_set$clone()$simulate_agent()

test_that("simulation is required to get pe data", {
  expect_null(both_set$pe_data)
})

test_that("pe data is available after simulation", {
  expect_s3_class(simulated$pe_data,
    class = "data.frame"
  )

  expect_named(
    simulated$pe_data,
    c("trial", "episode", "value")
  )
})
