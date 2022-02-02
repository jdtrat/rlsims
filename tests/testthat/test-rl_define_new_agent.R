# Hashes for TD Conditioning Agent where there is 10 episodes per trial, 100
# trials. A stimulus is presented at episode three and a reward of magnitude 1
# delivered at episode eight on all trials. Gamma is 1. Alpha is 0.3.
hash_prediction_errors <- "e28a272f41d697e4830fae7d6325a0bd"
hash_state_values <- "1e68340f1e8e5d1ebcf87e9b0e8e275d"

cond_stimuli <- list(
  one = data.frame(
    onset = 3,
    offset = 8,
    magnitude = 1,
    trial = 1:100
  )
)

cond_reinforcers <- list(
  reward = data.frame(
    onset = 8,
    offset = 8,
    magnitude = 1,
    trial = 1:100
  )
)

cond_tdrl <- rl_define_new_agent(
  model_type = "TDRL Cond Conditioning",
  simulation_code_file = use_agent_template("td_conditioning", read = FALSE),
  required_args = c("num_stimuli", "num_trials", "num_episodes", "gamma", "alpha"),
  required_methods = c("stimuli", "reinforcements")
)

condTdrl <- cond_tdrl$new(
  model_id = "TDRL Conditioning - One Cue",
  num_stimuli = 1,
  num_trials = 100,
  num_episodes = 10,
  gamma = 1,
  alpha = 0.3
)$
  set_stimuli(cond_stimuli)$
  set_reinforcements(cond_reinforcers)$
  simulate_agent()

test_that("`TD Conditioning Agent returns correct prediction errors`", {
  expect_equal(
    hash_prediction_errors,
    digest::digest(condTdrl$prediction_errors)
  )
})

test_that("`TD Conditioning Agent returns correct state values`", {
  expect_equal(
    hash_state_values,
    digest::digest(condTdrl$state_values)
  )
})
