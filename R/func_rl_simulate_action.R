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
#' @examples
rl_simulate_action <- function(policy, values, ...) {

  class(policy) <- c("character", policy)

  UseMethod("rl_simulate_action", object = policy)

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
#'   lower tau (temperature) values lead to more certain chocie distributions
#'   (more exploitation).
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return A number representing which action will be taken.
#' @export
#'
#' @examples
rl_simulate_action.softmax <- function(policy = "softmax", values, tau, ...) {

  rand <- runif(1, min = 0, max = 1)
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
rl_simulate_action.greedy <- function(policy = "greedy", values, ...) {
  which(values == max(values))
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
rl_simulate_action.epsilonGreedy <- function(policy = "epsilonGreedy", values, epsilon, ...) {

  if (epsilon < 0 || epsilon > 1) cli::cli_abort("{.arg epsilon} must be between zero and one.")

  rand <- runif(1, min = 0, max = 1)

  if (rand > epsilon) {
    which(values == max(values))
  } else {
    round(runif(1, min = 1, max = length(values)))
  }

}
