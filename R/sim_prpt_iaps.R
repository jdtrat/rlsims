
#' @keywords internal
#' @noRd
one_iter <- function(num_iters, num_trials, num_states, task_type = task_type) {
  setup_iter(num_states = num_states, task_type = task_type)

  purrr::walk(seq(1, num_trials), ~ sample_icons(.x))
  purrr::walk(seq(1, num_trials), ~ make_decisions(.x))
  purrr::walk(seq(1, num_trials), ~ determine_rewards(.x))
  purrr::walk(seq(1, num_trials), ~ determine_percent_correct(.x))

  prpt_iaps_performance(k = num_iters)

}


#' Simulate Random Choices on the PRPT or IAPS task
#'
#' @param num_iters Number of iterations
#' @param num_trials Number of trials
#' @param num_states Number of states -- should equal 4
#' @param task_type Is simulation for IAPS or PRPT task? If former, task_type = "iaps"; latter and task_type = "prpt"
#'
#' @return A list of the random earnings and correctness.
#' @export
#'
simulate_prpt_iaps <- function(num_iters = 1000, num_trials = 150, num_states = 4, task_type) {

  setup_prpt_iaps(num_iters = num_iters,
             num_trials = num_trials)

  purrr::walk(.x = seq(1, num_iters), ~ one_iter(num_iters = .x,
                                                 num_trials = num_trials,
                                                 num_states = num_states,
                                                 task_type = task_type))

  return(list(
    random_earnings = prpt_iaps$rand_earnings,
    random_correctness = prpt_iaps$rand_correctness
  ))

}
