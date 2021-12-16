
#' Setup an agent's Cue Structure
#'
#' @description This is a generic function for, when applicable, defining the
#'   cue structure of a reinforcement learning agent.
#' @param agent An object of class "rlAgent" created with
#'   \code{\link{rl_new_agent}}.
#' @param ... Additional arguments to pass into the specific RL Agent for
#'   setting the cue structure.
#'
#' @return The agent object with modified cue structure.
#' @export
#'
rl_set_cues <- function(agent, ...) {

  UseMethod("rl_set_cues")

}

#' Set the Cue Structure for 'tdrlConditioning' Agent
#'
#' @description Define the onset episode and offset episode of cues for each
#'   trial.
#' @param agent An RL Agent created with \code{\link{rl_new_agent}}.
#' @param cue_input A list of of cues where each element contains
#'   a data frame with columns 'onset', 'offset', 'magnitude', and 'trial'
#'   describing, respectively, the episode number a cue presentation begins;
#'   the episode number the cue presentation ends; the magnitude (saliency
#'   of a cue) of the cue; the trials the cues occur.
#' @param keep_cue_structure (Logical) `FALSE` (default) and any existing
#'   cue structure will be replaced when called. `TRUE` and the cue
#'   structure will be modified but will not remove previously defined cues.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return The agent object with modified reward structure.
#' @export
#'
#' @examples
#'
#' # Annotated examples of different cue structures:
#'
#' # For a task where one cue occurs during the first 250 trials with magnitude (1)
#' # and a second cue occurs during the second 250 trials with magnitude (1). Cue
#' # values are set to be zero if not explicitly defined.
#' ex1 <- list(
#'   one = data.frame(
#'     onset = 60,
#'     offset = 80,
#'     magnitude = c(rep(1, 250), rep(0, 250)),
#'     trial = 1:500),
#'   two = data.frame(
#'     onset = 70,
#'     offset = 80,
#'     magnitude = c(rep(0, 250), rep(1, 250)),
#'     trial = 1:500
#'   )
#' )
#'
#' # This is an alternative way to specify the two-cue structure since the cue
#' # values are set to be zero if not explicitly defined.
#' ex2 <- list(
#'   one = data.frame(
#'     onset = 60,
#'     offset = 80,
#'     magnitude = 1,
#'     trial = 1:250),
#'   two = data.frame(
#'     onset = 70,
#'     offset = 80,
#'     magnitude = 1,
#'     trial = 251:500
#'   )
#' )
#'
#'
#'
rl_set_cues.tdrlConditioning <- function(agent, cue_input, keep_cue_structure = FALSE, ...) {

  agent$set_cues(
    cue_input = cue_input,
    keep_cue_structure = keep_cue_structure
  )

}
