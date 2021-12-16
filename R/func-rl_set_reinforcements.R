#' Setup an agent's Reward Structure
#'
#' @description This is a generic function for, when applicable, defining the
#'   reward structure of a reinforcement learning agent.
#' @param agent An RL Agent created with \code{\link{rl_new_agent}}.
#' @param ... Additional arguments to pass into the specific RL Agent for
#'   setting the reward structure.
#' @return The agent object with modified reward structure.
#'
#' @export
#'
rl_set_reinforcements <- function(agent, ...) {

  UseMethod("rl_set_reinforcements")

}

#' Set the Reward Structure for 'tdrlConditioning' Agent
#'
#' @param agent An RL Agent created with \code{\link{rl_new_agent}}.
#' @param reinforcement_input A list containing a data frame with columns 'onset',
#'   'offset', 'magnitude', and 'trial' describing, respectively, the episode
#'   number a reward presentation begins; the episode number the reward
#'   presentation ends; the magnitude of the reward; the trials the rewards
#'   occur on.
#' @param keep_reward_structure (Logical) `FALSE` (default) and any existing
#'   reward structure will be replaced when called. `TRUE` and the reward
#'   structure will be modified but will not remove previously defined rewards.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return The agent object with modified reward structure.
#' @export
#'
#' @examples
#'
#' # Annotated examples of different reward structures:
#'
#' # For all 500 trials, present reward with magnitude 1 only on episode 80.
#' ex1 <- list(
#'   data.frame(
#'     onset = 80,
#'     offset = 80,
#'     magnitude = 1,
#'     trial = 1:500
#'   )
#' )
#'
#' # For the first 250 trials, present a reward with magnitude 1 only on episode
#' # 80. For the second 250 trials, present a reward with magnitude -1 on episode
#' # 80.
#' ex2 <- list(
#'   data.frame(
#'     onset = 80,
#'     offset = 80,
#'     magnitude = c(rep(1, 250), rep(-1, 250)),
#'     trial = 1:500
#'   )
#' )
#'
#' # Present a reward with magnitude 1 only on episode 80 every other trial (even trials). This
#' # means reward is witheld (magnitude 0) on all others (odd trials).
#' ex3 <- list(
#'   data.frame(
#'     onset = 80,
#'     offset = 80,
#'     magnitude = 1,
#'     trial = seq(from = 1, to = 500, 2)
#'   )
#' )
#'
#' # This is an equivalent way to write the above reward structure where a reward
#' # is presented on episode 80 every other trial (even trials) with magnitude 1
#' # and withheld (magnitude 0) on all others (odd trials).
#' ex4 <- list(
#'   data.frame(
#'     onset = 80,
#'     offset = 80,
#'     magnitude = rep(c(1,0), 250),
#'     trial = 1:500
#'   )
#' )
#'
#' # For the first 250 trials, present reward with magnitude 1 only on episode 80;
#' # for the second 250 trials, present reward with magnitude 1 only on episode 70.
#' ex5 <- list(
#'   data.frame(
#'     onset = c(rep(80, 250), rep(70, 250)),
#'     offset = c(rep(80, 250), rep(70, 250)),
#'     magnitude = 1,
#'     trial = 1:500
#'   )
#' )
#'
#'
#'
rl_set_reinforcements.tdrlConditioning <- function(agent, reinforcement_input, keep_reward_structure = FALSE, ...) {

  agent$set_reinforcements(
    reinforcement_input = reinforcement_input,
    keep_reward_structure = keep_reward_structure
  )

}
