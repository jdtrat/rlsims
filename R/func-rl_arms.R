#' Setup an agent's Arm Structure
#'
#' @description This is a generic function for, when applicable, defining the
#'   arm structure of a reinforcement learning agent.
#' @param agent An RL Agent created with \code{\link{rl_new_agent}}.
#' @param ... Additional arguments to pass into the specific RL Agent for
#'   setting the arm structure.
#' @return The agent object with modified arm structure.
#'
#' @export
#'
rl_set_arms <- function(agent, ...) {

  UseMethod("rl_set_arms")

}

#' Set the Arm Structure for 'kArmedBandit' Agent
#'
#' @param agent An RL Agent created with \code{\link{rl_new_agent}}.
#' @param arm_input A list of arm definitions where each element contains a data
#'   frame with columns 'probability', 'magnitude', 'alternative', and 'trial'
#'   describing, respectively, the `probability` of receiving a reward
#'   `magnitude` with the `alternative` for each `trial`.
#' @param keep_arm_structure (Logical) `FALSE` (default) and any existing arm
#'   structure will be replaced when called. `TRUE` and the arm structure will
#'   be modified but will not remove previously defined rewards.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return The agent object with modified arm structure.
#' @export
#'
#' @examples
#'
#' # Annotated examples of different arm structures:
#'
rl_set_arms.kArmedBandit <- function(agent, arm_input, keep_arm_structure = FALSE, ...) {

  agent$set_arms(
    arm_input = arm_input,
    keep_arm_structure = keep_arm_structure
    )

}

#' Retrieve the List of Arm Definitions
#'
#' @param agent An RL Agent created with \code{\link{rl_new_agent}}.
#'
#' @return If the RL Agent has its arm structure set, this will return the list
#'   of arms defined with \code{\link{rl_set_arms}}; otherwise nothing.
#' @export
#'
#' @examples
#'
#'
rl_arms_get_structure <- function(agent) {
  agent$get_arms()
}

#' Define an RL Arm
#'
#' @description Define the probabilistic-reinforcement structure of a specific
#'   arm. This function is a simple wrapper around `data.frame`. As such, you
#'   can pass in either numeric vectors the same length as the number of
#'   simulated trials or a single number.
#'
#' @param probability Either a numeric vector the same length as the number of
#'   simulated trials or a single number between zero and one to define the
#'   probability a reinforcement will be delivered.
#' @param magnitude Either a numeric vector the same length as the number of
#'   simulated trials or a single number defining the magnitude of the
#'   reinforcement to be delivered with probability `probability` if this arm is
#'   chosen.
#' @param alternative Either a numeric vector the same length as the number of
#'   simulated trials or a single number defining the magnitude of the
#'   reinforcement to be delivered with probability `(1 - probability)` if this
#'   arm is chosen.
#' @param trial A numeric vector describing the trial in which the probabilistic
#'   reinforcement structure defined by the arguments `probability`,
#'   `magnitude`, and `alternative` applies.
#'
#' @return A data frame with columns 'probability', 'magnitude', 'alternative',
#'   and 'trial' describing the reinforcement structure of an individual RL
#'   Agent's arm.
#' @export
#'
#' @examples
#'
#' #' # Define an arm that, when selected, leads to a 25% probability of receiving a
#' # positive reinforcement with magnitude 1 or zero for the first 75 trials and a negative
#' # reinforcement with magnitude 1 or zero with 75% probability for the second 75 trials.
#' rl_arms_define(
#'   probability = rep(0.25, 150),
#'   magnitude = c(rep(1, 75), rep(-1, 75)),
#'   alternative = 0,
#'   trial = 1:150
#' )
#'
rl_arms_define <- function(probability, magnitude, alternative, trial) {
  data.frame(
    probability = probability,
    magnitude = magnitude,
    alternative = alternative,
    trial = trial
  )
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
  arm <- arm_definitions[[action]][trial,]
  if (stats::runif(1, min = 0, max = 1) <= arm$prob) arm$magnitude else arm$alternative
}
