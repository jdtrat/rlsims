test_agent <- rl_define_new_agent(
  model_type = "TD Conditioning",
  simulation_code_file = use_agent_template("td_conditioning", read = FALSE),
  required_args = c("num_stimuli", "num_trials", "num_episodes", "gamma", "alpha"),
  required_methods = c("reinforcements", "stimuli")
)

testCond <- test_agent$new(
  model_id = "Testing Example",
  num_stimuli = 1,
  num_trials = 50,
  num_episodes = 10,
  gamma = 1,
  alpha = 0.05
)

test_that("reward onset and offset error catching works", {
  expect_error(testCond$
    set_reinforcements(list(
    data.frame(
      onset = 8,
      offset = 6,
      magnitude = 1,
      trial = 1:50
    )
  )))
})
