# Utility function for checkign the agent's class
check_agent <- function(.agent) {
  if (!inherits(.agent, "rlAgent")) {
    cli::cli_abort("{.arg agent} must be {.cls rlAgent}, not of class {.cls {class(agent)}}")
  }
}

#' Initialize a Reinforcement Learning Agent
#'
#' @description Create a new `rlAgent` object
#' @param model_type (character) either "tdrl" or "vprl", referencing the
#'   reinforcement learning paradigm to perform the simulation.
#' @param num_trials (numeric) The number of trials to simulate.
#' @param num_episodes (numeric) The number of episodes per trial.
#' @param num_cues (numeric) The number of potential cues interacting with the RL agent.
#' @param gamma (numeric) The temporal discounting factor of the RL agent
#' @param alpha (numeric) The learning rate of the RL agent
#'
#' @export
#' @examples
#'
#' mont6a <- rl_new_agent(model_type = "tdrl",
#'                        num_trials = 500,
#'                        num_episodes = 100,
#'                        num_cues = 2,
#'                        gamma = 1,
#'                        alpha = 0.05)
#'
rl_new_agent <- function(model_type,
                         num_trials, num_episodes, num_cues,
                         gamma, alpha) {

  rl_agent$new(model_type = model_type,
               num_trials = num_trials,
               num_episodes = num_episodes,
               num_cues = num_cues,
               gamma = gamma,
               alpha = alpha)
}

#' Setup an agent's Reward Structure
#'
#' @description Define the onset episode and offset episode of rewards for each
#'   trial
#' @param agent An object of class "rlAgent" created with
#'   \code{\link{rl_new_agent}}.
#' @param reward_onset (Numeric) The episode number a reward presentation
#'   begins. Either a single number where reward is presented at the same
#'   episode in all simulated trial or a numeric vector of length `num_trials`
#'   with reward onset occurring in different episodes.
#' @param reward_offset (Numeric) The episode number a reward presentation ends.
#'   Either a single number where reward is presented at the same episode in all
#'   simulated trial or a numeric vector of length `num_trials` with reward
#'   offset occurring in different episodes.
#' @param reward_magnitude (Numeric) The magnitude of the reward. Either a
#'   single number where reward is presented with the same magnitude across all
#'   simulated trials or a numeric vector of length `num_trials` with reward
#'   magnitude differing across trials.
#' @param keep_reward_structure (Logical) `FALSE` (default) and any existing
#'   reward structure will be replaced when called. `TRUE` and the reward
#'   structure will be modified but will not remove previously defined rewards.
#'
#' @return The agent object with modified reward structure.
#' @export
#'
rl_set_reward <- function(agent, reward_onset, reward_offset, reward_magnitude, keep_reward_structure = FALSE) {

  check_agent(agent)

  agent$set_reward(
    reward_onset = reward_onset,
    reward_offset = reward_offset,
    reward_magnitude = reward_magnitude,
    keep_reward_structure = keep_reward_structure
  )

}

#' Setup an agent's Cue Structure
#'
#' @description Define the onset episode and offset episode of cues for
#'   each trial
#' @param agent An object of class "rlAgent" created with
#'   \code{\link{rl_new_agent}}.
#' @param cue_list A list of length `num_cues` where each element contains a
#'   data frame with columns 'cue', 'onset', 'offset', and 'trial'.
#' @param keep_cue_structure (Logical) `FALSE` (default) and any
#'   existing cue structure will be replaced when called. `TRUE` and the
#'   cue structure will be modified but will not remove previously
#'   defined cues.
#' @return The agent object with modified reward structure.
#' @export
#'
#' @examples
#' cue_data_list <- list(
#'   one = data.frame(
#'     cue = 1,
#'     onset = 60,
#'     offset = 80,
#'     trial = 1:500),
#'   two = data.frame(
#'     cue = 2,
#'     onset = 70,
#'     offset = 80,
#'     trial = 1:500
#'   )
#' )
#'
rl_set_cues <- function(agent, cue_list, keep_cue_structure = FALSE) {

  check_agent(agent)

  agent$set_cues(
    cue_list = cue_list,
    keep_cue_structure = keep_cue_structure
  )

}

#' Get Tidied Reward Prediction Error Data
#'
#' @description Convert the agent's simulated reward prediction errors from a
#'   matrix where each row is an episode and each column is a trial to a
#'   dataframe with columns 'trial', 'episode', and 'value'.
#'
#' @param agent An object of class "rlAgent" created with
#'   \code{\link{rl_new_agent}}.
#' @param add_trial_zero (Logical) `TRUE` by default and trial zero will be
#'   appended to the prediction error data frame with values from
#'   `trial_zero_value`. `FALSE` and output will begin at trial one.
#' @param trial_zero_value (Numeric) Either a single value (default is 0) or a
#'   vector of values to append for trial 0.
#'
#' @return A dataframe with the agent's simulated reward prediction errors
#'   ('value') for each episode across trials.
#' @export
#'
rl_get_tidy_pe_data <- function(agent, add_trial_zero = TRUE, trial_zero_value = 0) {

  check_agent(agent)

  agent$get_tidy_pe_data(
    add_trial_zero = add_trial_zero,
    trial_zero_value = trial_zero_value
    )

}
