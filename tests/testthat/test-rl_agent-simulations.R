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

reward_set <- test_agent$clone()$
  set_reinforcements(list(
  data.frame(
    onset = 8,
    offset = 8,
    magnitude = 1,
    trial = 1:50
  )
))

stimulus_set <- test_agent$clone()$set_stimuli(
  list(
    one = data.frame(
      onset = 4,
      offset = 8,
      magnitude = 1,
      trial = 1:50
    )
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

test_that("reward and stimuli before simulating", {
  expect_error(test_agent$simulate_agent(),
    regexp = "set before simulating"
  )
})

test_that("set stimuli only before simulating", {
  expect_error(reward_set$simulate_agent(),
    regexp = "set before simulating"
  )
})

test_that("set rewards only before simulating", {
  expect_error(stimulus_set$simulate_agent(),
    regexp = "set before simulating"
  )
})

test_that("simulation works", {
  expect_silent(both_set$simulate_agent())
})
