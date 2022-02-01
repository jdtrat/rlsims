#' Define the Arm Structure
#'
#' @param arm_input A list of arm definitions where each element contains a data
#'   frame with columns 'probability', 'magnitude', 'alternative', and 'trial'
#'   describing, respectively, the `probability` of receiving a reward
#'   `magnitude` with the `alternative` for each `trial`.
#' @param num_arms (numeric) The number of arms in the task.
#' @param num_trials (numeric) The number of trials to simulate.
#'
#' @details
#'
#' Each "arm" is a data frame with the following parameters:
#'
#'  * `probability`: Either a numeric vector the same length as the number of
#'   simulated trials or a single number between zero and one to define the
#'   probability a reinforcement will be delivered.
#' * `magnitude`: Either a numeric vector the same length as the number of
#'   simulated trials or a single number defining the magnitude of the
#'   reinforcement to be delivered with probability `probability` if this arm is
#'   chosen.
#' * `alternative`: Either a numeric vector the same length as the number of
#'   simulated trials or a single number defining the magnitude of the
#'   reinforcement to be delivered with probability `(1 - probability)` if this
#'   arm is chosen.
#' * `trial`: A numeric vector describing the trial in which the probabilistic
#'   reinforcement structure defined by the arguments `probability`,
#'   `magnitude`, and `alternative` applies.
#'
#' @export
#'
#' @examples
#'
#' # Define an arm that, when selected, leads to a 25% probability of receiving
#' # positive reinforcement with magnitude 1 or zero for the first 75 trials and
#' # a negative reinforcement with magnitude 1 or zero with 75% probability for
#' # the second 75 trials.
#'
#' rl_define_arms(
#'   list(
#'     data.frame(
#'       probability = rep(0.25, 150),
#'       magnitude = c(rep(1, 75), rep(-1, 75)),
#'       alternative = 0,
#'       trial = 1:150
#'     )
#'   ),
#'   num_arms = 1,
#'   num_trials = 150
#' )
rl_define_arms <- function(arm_input, num_arms, num_trials) {
  arm_structure <- rlang::enquo(arm_input)

  check_arm_input(
    .input = arm_input,
    num_arms = num_arms,
    num_trials = num_trials
  )

  arm_structure
}

#' Get Arm's Outcome based on its Probability and Reward Structure
#'
#' @description This function defines the reinforcement delivery for an
#'   individual arm, and is used internaly by RL Bandit Agents. With probability
#'   `prob`, it an arm will yield a reinforcement of `magnitude`; with
#'   probability `1 - prob`, an arm will yield a reinforcement of `alternative`
#'   (default of zero).
#' @param arm_definitions A list of arm definitions where each element contains
#'   a data frame with columns 'probability', 'magnitude', 'alternative', and
#'   'trial' describing, respectively, the `probability` of receiving a reward
#'   `magnitude` with the `alternative` for each `trial`.
#' @param action A numeric scalar representing which action was selected on a
#'   given trial.
#' @param trial The trial in which an action was selected.
#'
#' @return A numeric reinforcement defined by `magnitude` (with probability
#'   `prob`) or `alternative` (with probability `1 - prob`).
#' @export
#'
rl_arms_get_outcome <- function(arm_definitions, action, trial) {
  arm <- arm_definitions[[action]][trial, ]
  if (stats::runif(1, min = 0, max = 1) <= arm$prob) arm$magnitude else arm$alternative
}
