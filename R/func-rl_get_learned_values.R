#' Get RL Agent's Simulated Learned Values
#'
#' @description This is a generic function for converting the agent's simulated
#'   learned values into a nicer format, when applicable, for exploration and
#'   visualization.
#'
#' @param agent An object of class "rlAgent" created with
#'   \code{\link{rl_new_agent}}.
#' @param ... Additional arguments to pass into the specific RL Agent for
#'   recovering the learned value data.
#'
#' @return A cleaned version of the agent's learned values. See specific methods
#'   for details.
#'
#' @export
#'
rl_get_learned_values <- function(agent, ...) {

  UseMethod("rl_get_learned_values")

}

#' Get 'tdrlConditioning' Agent's Learned Value Data
#'
#' @description Convert the 'tdrl' agent's simulated reward learned values from a
#'   matrix where each row is an episode and each column is a trial to a
#'   dataframe with columns 'trial', 'episode', and 'value'.
#'
#' @param agent An object of class "rlAgent" created with
#'   \code{\link{rl_new_agent}}.
#' @param add_trial_zero (Logical) `TRUE` by default and trial zero will be
#'   appended to the learned value data frame with values from
#'   `trial_zero_value`. `FALSE` and output will begin at trial one.
#' @param trial_zero_value (Numeric) Either a single value (default is 0) or a
#'   vector of values to append for trial 0.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return A dataframe with the agent's simulated reward learned values
#'   ('value') for each episode across trials.
#' @export
#'
rl_get_learned_values.tdrlConditioning <- function(agent, add_trial_zero = TRUE, trial_zero_value = 0, ...) {

  agent$get_learned_values(
    add_trial_zero = add_trial_zero,
    trial_zero_value = trial_zero_value
  )

}

#' Get 'kArmedBandit' Agent's Learned Value Data
#'
#' @description Convert the agent's estimated Q values array where, for each
#'   action, there is an matrix where each row is an episode and each column is
#'   a trial that contains the q_value, to a dataframe with columns 'trial',
#'   'episode', 'action', and 'q_value'.
#'
#' @param agent An object of class "rlAgent" created with
#'   \code{\link{rl_new_agent}}.
#'
#' @param add_trial_zero (Logical) `TRUE` by default and trial zero will be
#'   appended to the learned value data frame with values from
#'   `trial_zero_value`. `FALSE` and output will begin at trial one.
#' @param trial_zero_value (Numeric) Either a single value (default is 0) or a
#'   vector of values with length `num_arms` times `num_episodes` to append for
#'   trial 0.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return A dataframe with the agent's simulated learned values ('q_value')
#'   from taking an action for each episode across all trials.
#' @export
#'
rl_get_learned_values.kArmedBandit <- function(agent, add_trial_zero = TRUE, trial_zero_value = 0, ...) {

  agent$get_learned_values(
    add_trial_zero = add_trial_zero,
    trial_zero_value = trial_zero_value
  )

}
