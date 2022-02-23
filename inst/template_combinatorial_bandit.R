#### K-Armed Bandit

# Set Up Task Structure ---------------------------------------------------

## Define Task Structure Parameters
num_arms <- {{ num_arms }}
num_trials <- {{ num_trials }}
num_episodes <- {{ num_episodes }}

## Define Arm Structure
arm_structure <- {{{ arm_structure }}}
## Define subset of arms
arm_subsets <- {{{ arm_subsets }}}

## Define Action and Reinforcement Episodes
action_episode <- {{ action_episode }}
reinforcement_episode <- {{ reinforcement_episode }}

## Define Agent's Learning Parameters
gamma <- {{ gamma }}
alpha <- {{ alpha }}

## Initialize Arrays for Storing Agents Experience

# Create three dimensional arrays for saving the Q values and prediction
# errors from the entire task
Q_values <- array(0, dim = c(num_arms, num_episodes, num_trials))
prediction_errors <- array(0, dim = c(num_arms, num_episodes, num_trials))

# Create matrices for locally updating Q values
Q_val <- array(0, dim = c(num_arms, num_episodes))
# Create matrices for locally updating prediction errors
pe <- array(0, dim = c(num_arms, num_episodes))

# Create matrices for storing the experienced reinforcements
reinforcements <- array(0, dim = c(num_episodes, num_trials))

# Create a vector for storing the actions taken
actions <- numeric(length = num_trials)


# Simulate ----------------------------------------------------------------

# For each trial
for (tr in 1:num_trials) {
  # For each episode (except the terminal one)
  for (ep in 1:(num_episodes - 1)) {
    # If the episode is when the action is taken, simulate an action with said policy.
    if (ep == action_episode) {

      actions[tr] <- {{{ simulate_function }}}

      # Transform the combinatorial bandit arm number back into the full action space arm number
      actions[tr] <- arm_subsets[tr, actions[tr]]

      # Keep track of the experienced reinforcements
      reinforcements[reinforcement_episode, tr] <- rl_arms_get_outcome(
        arm_definitions = arm_structure,
        action = actions[tr],
        trial = tr
      )
    }
    # Define the actual experience as the reinforcement after choosing a given
    # arm plus the discounted sum of future values given the action taken during the
    # current state (and trial)
    actual <- reinforcements[ep, tr] + (gamma * Q_val[actions[tr], ep + 1])
    # Define the expected value of the current state given the current Q value estimate
    expected <- Q_val[actions[tr], ep]

    # Prediction error for a given action on a specific episode is the
    # difference between actual experience of selecting an arm and expected experience
    pe[actions[tr], ep] <- actual - expected

    # Update Q value estimate state given the expected value (previous estimated
    # Q value) plus the learning rate times prediction error from having taken
    # an action
    Q_val[actions[tr], ep] <- expected + (alpha * pe[actions[tr], ep])

    # Save running total of estimated Q value and prediction error for plotting
    for (ac in 1:num_arms) {
      Q_values[ac, ep, tr] <- Q_val[ac, ep]
      prediction_errors[ac, ep, tr] <- pe[ac, ep]
    }
  }
}


# For each arm, create a data frame with each trial, episode, action, and the
# associated Q Value
learned_values <- do.call(
  rbind,
  lapply(seq_len(num_arms), function(ac) {
    do.call(
      rbind,
      lapply(
        seq_len(num_trials),
        function(tr) {
          data.frame(
            trial = round(tr),
            episode = seq_len(num_episodes),
            arm = ac,
            q_value = Q_values[ac, , tr]
          )
        }
      )
    )
  })
)


pe_data <- do.call(
  rbind,
  lapply(seq_len(num_arms), function(ac) {
    do.call(
      rbind,
      lapply(
        seq_len(num_trials),
        function(tr) {
          data.frame(
            trial = round(tr),
            episode = seq_len(num_episodes),
            arm = ac,
            value = prediction_errors[ac, , tr]
          )
        }
      )
    )
  })
)
