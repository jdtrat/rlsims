
#' Generate Policy List
#'
#' @param policy What policy should a decision be made under? Currently
#'   supported are softmax, greedy, and epsilon-greedy.
#' @param ... Additional arguments passed to or from specific methods, such as
#'   `tau` when `policy = "softmax"` and `epsilon` when `policy =
#'   "epsilonGreedy"`.
#'
#' @return A list with a policy "type" and, if applicable, parameters.
#' @keywords internal
#'
rl_set_policy_internal <- function(policy, ...) {
  params <- list(...)

  # If policy is greedy, no parameters are passed
  # so just list with policy type is returned
  if (policy == "greedy") {
    if (length(params) != 0) {
      cli::cli_alert_danger("A {.val greedy} policy does not take any parameters; ignoring supplied parameters.")
    }
    list(type = "greedy")
  } else {
    policy <- list(
      policy,
      params[[names(params)]]
    )
    # Set and return names
    stats::setNames(policy, c("type", names(params)))
  }
}


#' Check Action-Selection Policy
#'
#' @param x A policy list defined by \code{\link{rl_set_policy_internal}}.
#'
#' @return If the policy is valid, nothing. If the policy definition is invalid, throws an error.
#' @keywords internal
#'
rl_policy_check_internal <- function(x) {
  if (x$type == "softmax") {
    correct_arg <- "tau"
    valid <- length(x$tau) == 1 && is.numeric(x$tau) && x$tau > 0
    message <- "{.arg tau} must be {.cls numeric} and greater than zero."
  } else if (x$type == "epsilonGreedy") {
    correct_arg <- "epsilon"
    valid <- length(x$epsilon) == 1 && is.numeric(x$epsilon) && x$epsilon > 0 && x$epsilon <= 1
    message <- "{.arg epsilon} must {.cls numeric} and between zero and one."
  } else if (x$type == "greedy") {
    correct_arg <- NULL
    valid <- TRUE
  }

  correct_names <- all(names(x) == c("type", correct_arg))

  if (!correct_names && !valid) {
    cli::cli_abort("A {.val {x$type}} policy requires parameter {.val {correct_arg}}. {message}")
  } else if (correct_names && !valid) {
    cli::cli_abort(message)
  }
}

#' Define the Action-Selection Policy for an RL Agent
#'
#' @param policy What policy should a decision be made under? Currently
#'   supported are softmax, greedy, and epsilon-greedy.
#' @param ... Additional arguments passed to or from specific methods, such as
#'   `tau` when `policy = "softmax"` and `epsilon` when `policy =
#'   "epsilonGreedy"`.
#'
#' @return The agent object with modified policy structure.
#' @export
#'
#' @examples
#'
#' # Example arguments include:
#'
#' # (policy = "softmax", tau = 7)
#'
#' # (policy = "epsilonGreedy", epsilon = 0.4)
#'
#' # (policy = "greedy")
rl_define_policy <- function(policy, ...) {
  rl_set_policy_internal(
    policy = policy,
    ... = ...
  )
}
