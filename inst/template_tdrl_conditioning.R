
# Assign values for local use -- allows portability of main simulation
# code.
num_cues <- {{ num_cues }}
num_trials <- {{ num_trials }}
num_episodes <- {{ num_episodes }}

gamma <- {{ gamma }}
alpha <- {{ alpha }}

# Define a three-dimensional array tacking what cues are present in each episode
# across all simulated trials.
present_cues <- rl_define_cues_array(
  cue_input = {{{ cue_structure }}},
  num_episodes = num_episodes,
  num_trials = num_trials
)

estimated_values <- array(0, dim = c(num_cues, num_episodes, num_trials))

# Define a three-dimensional array tacking what reinforcements are delivered in
# each episode across all simulated trials.
reinforcements <- rl_define_reinforcements_array(
  reinforcement_input = {{{ reinforcement_structure }}},
  num_episodes = num_episodes,
  num_trials = num_trials
)

# Initialize a matrix used in each trial tracking the estimated episode value
# associated with each cue
cue_associated_value <- array(0, dim = c(num_cues, num_episodes))

# Initialize a matrix tracking the reward prediction errors occurring in each
# episode across all trials
prediction_errors <- array(0, dim = c(num_episodes, num_trials))

# Initialize expected cues; this should be 0 initially if no cues have been
# experienced then there is no expectation of any
expected_cues <- rep(0, num_cues)

for (tr in 1:num_trials) {
  for (ep in 1:(num_episodes - 1)) {

    # Actual cues become the ones experienced in a given state (and trial)
    actual_cues <- present_cues[, ep, tr]

    # Define the actual experience as the reinforcements + discounted sum of
    # future values given the actual cues experienced at the current state (and
    # trial)
    actual <- sum(reinforcements[, ep, tr]) + sum(gamma * cue_associated_value[, ep + 1] * actual_cues)
    # Define the expected value of the current state given the current value
    # estimate and the cues expected to be experienced in this state.
    expected <- sum(cue_associated_value[, ep] * expected_cues)

    # Prediction error is the difference between actual experience and
    # expected experience
    pe <- actual - expected

    # Update value estimate of the current state given the expected value
    # (previous estimate and expected cues) times the learning rate and
    # prediction error
    cue_associated_value[, ep] <- cue_associated_value[, ep] + expected_cues * alpha * pe # line 2

    # Save running total of estimated state value for plotting
    estimated_values[, ep, tr] <- cue_associated_value[, ep] # line 3

    # Save running total of the prediction error for plotting
    prediction_errors[ep, tr] <- pe # line 4

    # Update your expectation of the cues given your latest experience
    expected_cues <- actual_cues
  }
}

learned_values <- rbind(
  expand.grid(
    trial = 0,
    episode = seq_len(num_episodes),
    cue = seq_len(num_cues),
    value = 0
  ),
  do.call(
    rbind,
    lapply(seq_len(num_cues), function(c) {
      do.call(
        rbind,
        lapply(
          seq_len(num_trials),
          function(tr) {
            data.frame(
              trial = round(tr),
              episode = seq_len(num_episodes),
              cue = c,
              value = estimated_values[c, , tr]
            )
          }
        )
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
