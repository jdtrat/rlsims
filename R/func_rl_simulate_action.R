#' Simulate an RL Agent's Action
#'
#' @description This is a generic function used to simulate an RL agent's action
#'   given a specific decision-making policy.
#' @param policy What policy should a decision be made under? Currently
#'   supported are softmax, greedy, and epsilon-greedy.
#' @param values A numeric vector containing the current value estimates of each
#'   action.
#' @param ... Additional arguments passed to or from specific methods, such as
#'   `tau` when `policy = "softmax"` and `epsilon` when `policy =
#'   "epsilonGreedy"`.
#'
#' @return A number representing which action will be taken given the chosen policy.
#' @export
#'
rl_action_simulate <- function(policy, values, ...) {

  class(policy) <- c("character", policy)

  UseMethod("rl_action_simulate", object = policy)

}

#' Simulate an Action with a 'Softmax' Choice Policy
#'
#' @description This implementation of a 'Softmax' action selection policy
#'   accepts a choice temperature parameter `tau`, which describes an agent's
#'   propensity to explore the action space. The higher the tau (temperature),
#'   the more random the actions; the lower the tau (temperature), the more
#'   exploitative the actions (e.g., lower temperature increases the probability
#'   of taking the action with the highest expected value).
#' @param policy Defines the action selection policy as "softmax"; argument
#'   included in this method to support S3 Generics.
#' @param values A numeric vector containing the current value estimates of each
#'   action.
#' @param tau A choice temperature (greater than zero) parameter defining the
#'   exploration versus exploitation trade-off where higher tau (temperature)
#'   values lead to more uncertain choice distributions (more exploration) and
#'   lower tau (temperature) values lead to more certain choice distributions
#'   (more exploitation).
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return A number representing which action will be taken.
#' @export
#'
#' @examples
#'
#' # The smaller the tau, the less exploration
#' cold <- numeric(100)
#' for (trial in seq_along(cold)) {
#'   cold[trial] <- rl_action_simulate(policy = "softmax",
#'                                     values = c(0.2, 0.25, 0.15, 0.8),
#'                                     tau = 0.2)
#' }
#' # Choice 4 (0.8 is most optimal option) so we see it chosen most
#' sum(cold == 4)
#'
#' hot <- numeric(100)
#' for (trial in seq_along(hot)) {
#'   hot[trial] <- rl_action_simulate(policy = "softmax",
#'                                    values = c(0.2, 0.25, 0.15, 0.8),
#'                                    tau = 5)
#' }
#' # Choice 4 (0.8 is most optimal option) but we see more exploration here
#' sum(hot == 4)
#'
rl_action_simulate.softmax <- function(policy = "softmax", values, tau, ...) {

  rand <- stats::runif(1, min = 0, max = 1)
  probs <- exp(values / tau) / sum(exp(values / tau))

  if (any(vapply(probs, is.na, logical(1)))) cli::cli_abort("{.arg tau} must be greater than {.val {tau}}.")

  cum_prob <- cumsum(probs)
  for (ac in seq_along(probs)) {
    if (cum_prob[ac] > rand) {
      return(ac)
    }
  }

}

#' Simulate an Action with a 'Greedy' Choice Policy
#'
#' @description This implementation of a 'greedy' action selection policy will
#'   mean whichever action has the highest expected value will be taken.
#' @param policy Defines the action selection policy as "greedy"; argument
#'   included in this method to support S3 Generics.
#' @param values A numeric vector containing the current value estimates of each
#'   action.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return A number representing which action will be taken.
#' @export
#'
#' @examples
#'
#' action <- numeric(100)
#' for (trial in seq_along(action)) {
#'  action[trial] <- rl_action_simulate(policy = "greedy",
#'                                     values = c(0.2, 0.25, 0.15, 0.8))
#' }
#'
#' # All of the actions were to choose the highest value option
#' all(action == 4)
#'
rl_action_simulate.greedy <- function(policy = "greedy", values, ...) {
  if (all(diff(values) == 0)) {
    round(stats::runif(1, min = 1, max = length(values)))
  } else {
    which.max(values)
  }
}

#' Simulate an Action with a 'Epsilon-Greedy' Choice Policy
#'
#' @description This implementation of an 'epsilonGreedy' action selection
#'   policy accepts a parameter `epsilon`, which describes an agent's propensity
#'   to explore the action space. The higher the epsilon, the more likely the
#'   agent is to select a random action; the lower epsilon, the more likely the
#'   agent is to select the exploitative action (one with highest expected
#'   value).
#' @param policy Defines the action selection policy as "epsilonGreedy";
#'   argument included in this method to support S3 Generics.
#' @param values A numeric vector containing the current value estimates of each
#'   action.
#' @param epsilon A parameter (between zero and one) modulating the RL agent's
#'   propensity to explore. That is, the higher the epsilon, the less
#'   exploitative choices the RL agent will make.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return A number representing which action will be taken.
#' @export
#'
#' @examples
#'
#' # The lower the epsilon, the less exploration
#' exploit <- numeric(100)
#' for (trial in seq_along(exploit)) {
#'   exploit[trial] <- rl_action_simulate(policy = "epsilonGreedy",
#'                                        values = c(0.2, 0.25, 0.15, 0.8),
#'                                        epsilon = 0.1)
#' }
#' # Choice 4 (0.8 is most optimal option) and we see it is selected the most
#' sum(exploit == 4)
#'
#' # The higher the epsilon, the more exploration
#' explore <- numeric(100)
#' for (trial in seq_along(exploit)) {
#'   explore[trial] <- rl_action_simulate(policy = "epsilonGreedy",
#'                                        values = c(0.2, 0.25, 0.15, 0.8),
#'                                        epsilon = 0.8)
#' }
#' # Choice 4 (0.8 is most optimal option) but we see more exploration here
#' sum(explore == 4)

rl_action_simulate.epsilonGreedy <- function(policy = "epsilonGreedy", values, epsilon, ...) {

  if (epsilon < 0 || epsilon > 1) cli::cli_abort("{.arg epsilon} must be between zero and one.")

  rand <- stats::runif(1, min = 0, max = 1)

  if (rand > epsilon) {
    which(values == max(values))
  } else {
    round(stats::runif(1, min = 1, max = length(values)))
  }

}
