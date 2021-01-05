
#' @keywords internal
#' @noRd
one_iter <- function(num_iters, num_trials, num_states) {
  setup_iter(num_states = num_states)

  purrr::walk(seq(1, num_trials), ~ sample_icons(.x))
  purrr::walk(seq(1, num_trials), ~ make_decisions(.x))
  purrr::walk(seq(1, num_trials), ~ determine_rewards(.x))
  purrr::walk(seq(1, num_trials), ~ determine_percent_correct(.x))

  prpt_performance(k = num_iters)

}


#' Simulate PRPT task
#'
#' @param num_iters Number of iterations
#' @param num_trials Number of trials
#' @param num_states Number of states -- should equal 4
#'
#' @return A list of the random earnings and correctness.
#' @export
#'
simulatePRPT <- function(num_iters = 1000, num_trials = 150, num_states = 4) {

  setup_PRPT(num_iters = num_iters,
             num_trials = num_trials)

  purrr::walk(.x = seq(1, num_iters), ~ one_iter(num_iters = .x,
                                                 num_trials = num_trials,
                                                 num_states = num_states))

  return(list(
    random_earnings = prpt$rand_earnings,
    random_correctness = prpt$rand_correctness
  ))

}
