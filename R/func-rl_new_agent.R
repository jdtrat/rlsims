#' Initialize a Reinforcement Learning Agent
#'
#' @description This is a generic function to create a new reinforcement
#'   learning agent.
#'
#' @param model_type What type of RL Agent would you like to create? One of
#'   \code{\link{available_agents}}.
#' @param model_id model_id (character) A model identifier referencing the
#'   reinforcement learning paradigm to perform the simulation.
#' @param ... Additional arguments to pass into the specific RL Agent.
#'   reinforcement learning paradigm to perform the simulation.
#' @export
#'
#' @return A Reinforcement Learning Agent object of class `model_type`.
#'
#' @examples
#'
#' rl_new_agent(model_type = "tdrlConditioning",
#'              model_id = "Example TDRL",
#'              num_trials = 100,
#'              num_episodes = 10,
#'              gamma = 1,
#'              alpha = 0.2)
#'
#'
rl_new_agent <- function(model_type,
                         model_id,
                         ...) {
  class(model_id) <- model_type
  UseMethod("rl_new_agent", model_id)
}

#' Initialize a 'tdrlConditioning' Agent
#'
#' @description Create a new `tdrlConditioning` Agent
#'
#' @param model_type What type of RL Agent would you like to create? One of
#'   \code{\link{available_agents}}.
#' @param model_id model_id (character) A model identifier referencing the
#'   reinforcement learning paradigm to perform the simulation.
#' @param num_trials (numeric) The number of trials to simulate.
#' @param num_episodes (numeric) The number of episodes per trial.
#' @param gamma (numeric) The temporal discounting factor of the RL agent
#' @param alpha (numeric) The learning rate of the RL agent
#' @param ... Additional arguments passed to or from other methods.
#'
#' @export
#'
rl_new_agent.tdrlConditioning <- function(model_type, model_id,
                                          num_trials, num_episodes,
                                          gamma, alpha, ...) {

  agent_tdrl_conditioning$new(model_id = model_id,
                              num_trials = num_trials,
                              num_episodes = num_episodes,
                              gamma = gamma,
                              alpha = alpha)
}

#' Initialize a 'tdrlConditioning' Agent
#'
#' @description Create a new `kArmedBandit` Agent
#'
#' @param model_type What type of RL Agent would you like to create? One of
#'   \code{\link{available_agents}}.
#' @param model_id model_id (character) A model identifier referencing the
#'   reinforcement learning paradigm to perform the simulation.
#' @param num_trials (numeric) The number of trials to simulate.
#' @param num_episodes (numeric) The number of episodes per trial.
#' @param num_arms (numeric) The number of arms (options) the agent can sample
#'   from.
#' @param action_episode (numeric) The episode an action should be taken on.
#' @param reinforcement_episode (numeric) The episode reinforcements hsould
#'   occur on.
#' @param gamma (numeric) The temporal discounting factor of the RL agent
#' @param alpha (numeric) The learning rate of the RL agent
#' @param ... Additional arguments passed to or from other methods.
#'
#' @export
#'
rl_new_agent.kArmedBandit <- function(model_type, model_id,
                                          num_trials, num_episodes, num_arms,
                                          action_episode, reinforcement_episode,
                                          gamma, alpha, ...) {

  agent_k_armed_bandit$new(model_id = model_id,
                           num_trials = num_trials,
                           num_episodes = num_episodes,
                           num_arms = num_arms,
                           action_episode = action_episode,
                           reinforcement_episode = reinforcement_episode,
                           gamma = gamma,
                           alpha = alpha)
}


