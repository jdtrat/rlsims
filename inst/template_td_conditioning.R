
# Assign values for local use -- allows portability of main simulation
# code.
num_stimuli <- {{ num_stimuli }}
num_trials <- {{ num_trials }}
num_episodes <- {{ num_episodes }}

gamma <- {{ gamma }}
alpha <- {{ alpha }}

# Define a three-dimensional array tacking what stimuli are in the environment in
# each episode across all simulated trials.
env_stimuli <- rl_define_stimuli_array(
  stimulus_input = {{{ stimulus_structure }}},
  num_episodes = num_episodes,
  num_trials = num_trials
)

# Define a three-dimensional array tacking what reinforcements are delivered in
# each episode across all simulated trials.
reinforcements <- rl_define_reinforcements_array(
  reinforcement_input = {{{ reinforcement_structure }}},
  num_episodes = num_episodes,
  num_trials = num_trials
)

# Initialize a matrix used in each trial tracking the estimated weights
# associated with each stimulus
stimuli_weights <- array(0, dim = c(num_stimuli, num_episodes))

# Initialize a matrix tracking the state values and reward prediction errors
# occurring in each episode across all trials
state_values <- array(0, dim = c(num_episodes, num_trials))
prediction_errors <- array(0, dim = c(num_episodes, num_trials))

for (tr in 1:num_trials) {
  for (ep in 1:(num_episodes - 1)) {

    # The experienced state value is the sum of reinforcements + discounted sum
    # of the future state values given the environmental stimuli and associated
    # weights
    experienced_state_val <- sum(reinforcements[, ep + 1, tr]) + sum(gamma * stimuli_weights[, ep + 1] * env_stimuli[, ep + 1, tr])
    # The expected value of the current state is the sum of each stimulus
    # representation (presence) times the weights associated with each
    expected_state_val <- sum(stimuli_weights[, ep] * env_stimuli[, ep, tr])

    # Prediction error is the difference between experienced state value and the
    # expected state value
    prediction_errors[ep + 1, tr] <- experienced_state_val - expected_state_val

    # Assign the state value for the current episode/trial as the expected state
    # value before learning
    state_values[ep, tr] <- expected_state_val

    # Update the weighting of stimuli given the previous estimate and prediction error and learnrate
    stimuli_weights[, ep] <- stimuli_weights[, ep] + (alpha * env_stimuli[, ep, tr] * prediction_errors[ep + 1, tr])

  }
}


learned_values <- rbind(
  expand.grid(
    trial = 0,
    episode = seq_len(num_episodes),
    value = 0
  ),
  do.call(
    rbind,
    lapply(seq_len(num_trials), function(.x) {
      data.frame(
        trial = round(.x),
        episode = seq_len(num_episodes),
        value = state_values[, .x]
      )
    })
  )
)

pe_data <- rbind(
  do.call(
    rbind,
    lapply(seq_len(num_trials), function(.x) {
      data.frame(
        trial = round(.x),
        episode = seq_len(num_episodes),
        value = prediction_errors[, .x]
      )
    })
  ),
  data.frame(
    trial = 0,
    episode = seq_len(num_episodes),
    value = 0
  )
)
