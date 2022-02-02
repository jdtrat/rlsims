#' Check Arm Input
#'
#' @param .input The input passed into one of the array definition functions
#'   created with \code{\link{rl_define_arms}}.
#' @param num_arms (numeric) The number of arms in the task.
#' @param num_trials (numeric) The number of trials to simulate.
#'
#' @return An error if the input is invalid, otherwise nothing.
#' @keywords internal
#'
check_arm_input <- function(.input, num_arms, num_trials) {
  if (!inherits(.input, "list")) cli::cli_abort("{.arg arm_input} must be a list of dataframes.")
  all_dataframes <- vapply(.input, inherits, logical(1), "data.frame")
  correct_dataframe_names <- vapply(
    .input,
    function(x) all(names(x) == c("probability", "magnitude", "alternative", "trial")),
    logical(1)
  )
  correct_dataframe_lengths <- vapply(
    .input,
    function(x) nrow(x) <= num_trials,
    logical(1)
  )

  if (!all(all_dataframes) || !all(correct_dataframe_names)) {
    cli::cli_abort("Please make sure each element of {.arg arm_input} contains a {.cls data frame} with columns named 'probability', 'magnitude', 'alternative', and 'trial'.")
  }


  if (!all(correct_dataframe_lengths)) {
    cli::cli_abort("Please make sure each element of {.arg arm_input} contains at most one onset and offset value per trial (no more than {num_trials}).")
  }

  if (!length(.input) == num_arms) {
    cli::cli_abort("More arms supplied than allocated. Update agent object's {.arg num_arms = {num_arms}} or remove an arm.")
  }
}

#' Check Action/Reinforcement Episodes
#'
#' @param num_eps Number of episodes per trial.
#' @param action_ep Episode number the action occurs on
#' @param reinforcement_ep Reinforcement number the action occurs on
#'
#' @return An error if the input is invalid, otherwise nothing.
#' @keywords internal
#'
check_action_reinforcement_episodes <- function(num_eps, action_ep, reinforcement_ep) {
  if (action_ep >= num_eps || reinforcement_ep >= num_eps) {
    cli::cli_abort("{.arg action_episode} and {.arg reinforcement_episode} must be less than the terminal episode {.arg num_episodes} ({num_eps})")
  }

  if (action_ep >= reinforcement_ep) {
    cli::cli_abort("{.arg action_episode} must occur before the {.arg reinforcement_episode}")
  }
}
