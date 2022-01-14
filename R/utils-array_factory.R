#' Have Input Checks for Setting Array Inputs
#'
#' @param .input The input passed into one of the array definition functions
#'   created with \code{\link{rl_define_array_factory}}.
#' @param type Either "cue" or "reinforcement" to tailor error messages
#'   according to the array setting input.
#' @param num_episodes (numeric) The number of episodes per trial.
#' @param num_trials (numeric) The number of trials to simulate.
#'
#' @return An error if the input is invalid, otherwise nothing.
#' @keywords internal
#'
check_array_list_input <- function(.input, type, num_episodes, num_trials) {
  arg_input <- switch(type,
                      cue = "cue_input",
                      reinforcement = "reinforcement_input")

  if (!inherits(.input, "list")) cli::cli_abort("{.arg {arg_input}} must be a list of dataframes")
  all_dataframes <- vapply(.input, inherits, logical(1), "data.frame")
  correct_dataframe_names <- vapply(.input,
                                    function(x) all(names(x) == c("onset", "offset", "magnitude", "trial")),
                                    logical(1))
  correct_dataframe_lengths <- vapply(.input,
                                      function(x) nrow(x) <= num_trials,
                                      logical(1))
  onset_offset_within_bounds <- vapply(.input,
                                       function(x) all(x$onset <= x$offset) && all(x$offset <= num_episodes),
                                       logical(1))

  if (!all(all_dataframes) || !all(correct_dataframe_names)) {
    cli::cli_abort("Please make sure each element of {.arg {arg_input}} contains a {.cls data frame} with columns named 'onset', 'offset', 'magnitude', and 'trial'.")
  }


  if (!all(correct_dataframe_lengths)) {
    cli::cli_abort("Please make sure each element of {.arg {arg_input}} contains at most one onset and offset value per trial (no more than {num_trials}).")
  }

  if (!all(onset_offset_within_bounds)) {
    cli::cli_abort("Please make sure each element of {.arg {arg_input}} contains cue onset values less than cue offset values and that both are less than the number of episodes per trial ({num_episodes})")
  }
}




#' Factory for Defining Array Setting Functions
#'
#' @param type What type of array will be set? 'reinforcement' or 'cues'.
#'
#' @return A function to be exported for defining either array or cues.
#'
#' @keywords internal
#' @examples
#'
#' \dontrun{
#' rl_define_array_factory("reinforcement")
#' }
#'
#'
rl_define_array_factory <- function(type) {

  fn_text <- whisker::whisker.render('
  function({{type}}_input, num_episodes, num_trials) {

    check_array_list_input(
      .input = {{type}}_input,
      type = "{{type}}",
      num_trials = num_trials,
      num_episodes = num_episodes
    )

    {{type}}_data <- do.call(rbind,
                         lapply(seq_along({{type}}_input), function(x) {
                           cbind({{type}}_number = x, {{type}}_input[[x]])
                         })
    )

    {{type}} <- zeros(dims = c(length({{type}}_input), num_episodes, num_trials))

    for (row in 1:nrow({{type}}_data)) {
      data <- {{type}}_data[row,]
      if (data$onset == data$offset) {
        {{type}}[data${{type}}_number, data$onset, data$trial] <- data$magnitude
      } else if (data$onset < data$offset) {
        {{type}}[data${{type}}_number, data$onset:data$offset, data$trial] <- data$magnitude
      }
    }
    {{type}}
  }', data = list(type = type))

  return(
    eval(parse(text = fn_text))
  )

}

#' Define Cue Array for Tracking Cues in RL Agent's Environment
#'
#' @param cue_input A list of of cues where each element contains a data frame
#'   with columns 'onset', 'offset', 'magnitude', and 'trial' describing,
#'   respectively, the episode number a cue presentation begins; the episode
#'   number the cue presentation ends; the magnitude (saliency of a cue) of the
#'   cue; the trials the cues occur.
#' @param num_episodes (numeric) The number of episodes per trial.
#' @param num_trials (numeric) The number of trials to simulate.
#'
#' @return A three-dimensional array tacking what cues are present in each
#'   episode across all simulated trials.
#' @export
#'
#' @examples
#'
#' rl_define_cues_array(
#'   list(
#'     one = data.frame(
#'       onset = 3,
#'       offset = 8,
#'       magnitude = 1,
#'       trial = 1:100
#'     )
#'   ),
#'   num_episodes = 10,
#'   num_trials = 100
#' )
#'
rl_define_cues_array <- rl_define_array_factory("cue")

#' Define Reinforcements Array for Tracking Cues in RL Agent's Environment
#'
#' @param reinforcement_input  A list of reinforcements where each element
#'   contains a data frame with columns 'onset', 'offset', 'magnitude', and
#'   'trial' describing, respectively, the episode number a reward presentation
#'   begins; the episode number the reward presentation ends; the magnitude of
#'   the reward; the trials the rewards occur on.
#' @param num_episodes (numeric) The number of episodes per trial.
#' @param num_trials (numeric) The number of trials to simulate.
#'
#' @return A three-dimensional array tacking what reinforcements are delivered
#'   in each episode across all simulated trials.
#' @export
#'
#' @examples
#'
#' rl_define_reinforcements_array(
#'   list(
#'     one = data.frame(
#'       onset = 8,
#'       offset = 8,
#'       magnitude = 1,
#'       trial = 1:100
#'     )
#'   ),
#'   num_episodes = 10,
#'   num_trials = 100
#' )
#'
rl_define_reinforcements_array <- rl_define_array_factory("reinforcement")
