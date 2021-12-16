#' Get RL Agent's Simulated Prediction Errors
#'
#' @description This is a generic function for converting the agent's simulated
#'   prediction errors into a nicer format, when applicable, for exploration and
#'   visualization.
#'
#' @param agent An object of class "rlAgent" created with
#'   \code{\link{rl_new_agent}}.
#' @param ... Additional arguments to pass into the specific RL Agent for
#'   recovering the prediction error data.
#'
#' @return A cleaned version of the agent's prediction errors. See specific
#'   methods for details.
#'
#' @export
#'
rl_get_pe_data <- function(agent, ...) {

  UseMethod("rl_get_pe_data")

}


#' Get 'tdrlConditioning' Agent's Prediction Error Data
#'
#' @description Convert the 'tdrl' agent's simulated reward prediction errors from a
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
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return A dataframe with the agent's simulated reward prediction errors
#'   ('value') for each episode across trials.
#' @export
#'
rl_get_pe_data.tdrlConditioning <- function(agent, add_trial_zero = TRUE, trial_zero_value = 0, ...) {

  agent$get_pe_data(
    add_trial_zero = add_trial_zero,
    trial_zero_value = trial_zero_value
  )

}
