#' Initialize a Reinforcement Learning Agent
#'
#' @description Create a new `rlAgent` object
#' @param model_type (character) either "tdrl" or "vprl", referencing the
#'   reinforcement learning paradigm to perform the simulation.
#' @param num_trials (numeric) The number of trials to simulate.
#' @param num_episodes (numeric) The number of episodes per trial.
#' @param num_cues (numeric) The number of potential cues interacting with the RL agent.
#' @param gamma (numeric) The temporal discounting factor of the RL agent
#' @param alpha (numeric) The learning rate of the RL agent
#' @examples
#'
#' mont6a <- rl_agent_new(model_type = "tdrl",
#'                        num_trials = 500,
#'                        num_episodes = 100,
#'                        num_cues = 2,
#'                        gamma = 1,
#'                        alpha = 0.05)
#'
#' @export
#'
rl_agent_new <- function(model_type,
                         num_trials, num_episodes, num_cues,
                         gamma, alpha) {

  rl_agent$new(model_type = model_type,
               num_trials = num_trials,
               num_episodes = num_episodes,
               num_cues = num_cues,
               gamma = gamma,
               alpha = alpha)
}

