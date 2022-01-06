#' Simulate the RL Agent
#'
#' @description Simulate the RL Agent
#'
#' @param agent An object of class "rlAgent" created with
#'   \code{\link{rl_new_agent}}.
#' @param ... Additional arguments to pass into the specific RL Agent for
#'   siulating the agent.
#'
#' @return The agent object with the completed simulations.
#'
#' @export
#'
rl_simulate_agent <- function(agent, ...) {

  UseMethod("rl_simulate_agent")

}


#' Simulate the 'tdrlConditioning' Agent
#'
#' @param agent An object of class "rlAgent" created with
#'   \code{\link{rl_new_agent}}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return The agent object with the completed simulations.
#' @export
#'
rl_simulate_agent.tdrlConditioning <- function(agent, ...) {
  agent$simulate_agent()
}

#' Simulate the 'kArmedBandit' Agent
#'
#' @param agent An object of class "rlAgent" created with
#'   \code{\link{rl_new_agent}}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return The agent object with the completed simulations.
#' @export
#'
rl_simulate_agent.kArmedBandit <- function(agent, ...) {
  agent$simulate_agent()
}
