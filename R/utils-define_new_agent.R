#' Define a New RL Agent
#' @description For users who wish to supply their own simulation code, calling
#'   this function streamlines the creation of RL Agent Objects allowing
#'   individuals to benefit from a consistent API for RL simulations and take
#'   advantage of internal safeguards.
#'
#' @param model_type What type of model is the agent? E.g., "kArmedBandit" or
#'   "tdrlConditioning".
#' @param simulation_code_file A file path containing templated simulation code.
#'   See the examples for details.
#' @param required_args A character vector indicating required arguments of the
#'   RL Agent. For example, `c("num_trials", "num_episodes", "gamma", "alpha")`.
#' @param required_methods Which methods should the agent support? Setting
#'   reinforcements? Stimuli? Arms? See the examples for more details.
#' @param return_agent Logical: `TRUE` (default) and the actual agent object
#'   will be returned and can be instantiated. `FALSE` and a string containing
#'   the code to generate the R6 Agent will be returned.
#'
#' @return If `return_agent = TRUE` (default), an R6 object of class
#'   `model_type` for simulating RL tasks with the file supplied by the
#'   `simulation_code_file` argument. Otherwise, a character string with the
#'   code to define the R6 object.
#' @export
#'
#' @examples
#'
#'
#'
#' # Define a temporal-difference conditioning agent using the built in algorithm
#' td_cond_agent <- rl_define_new_agent(
#'   model_type = "TD Conditioning",
#'   # Use built in model specification "td_conditioning", setting read = FALSE to
#'   # simply specify the path
#'   simulation_code_file = use_agent_template("td_conditioning", read = FALSE),
#'   # What values must be set for the agent/simulation to work?
#'   required_args = c("num_stimuli", "num_trials", "num_episodes", "gamma", "alpha"),
#'   # Only need to specify reinforcements and stimuli structure
#'   required_methods = c("reinforcements", "stimuli"),
#'   return_agent = TRUE # return the actual RL Agent object
#' )
#'
#' # Initialize a new conditioning agent as tdCond
#' tdCond <- td_cond_agent$new(
#'   model_id = "Classical Conditioning via TDRL",
#'   num_stimuli = 1,
#'   num_trials = 100,
#'   num_episodes = 10,
#'   gamma = 1,
#'   alpha = 0.3
#'   )
#'
#' tdCond$
#'   set_stimuli(
#'     list(
#'       one = data.frame(
#'         onset = 3,
#'         offset = 8,
#'         magnitude = 1,
#'         trial = 1:100
#'       )
#'     )
#'   )$
#'   set_reinforcements(
#'     list(
#'       data.frame(
#'         onset = 8,
#'         offset = 8,
#'         magnitude = 1,
#'         trial = 1:100
#'       )
#'     )
#'   )$
#'   simulate_agent()
#'
#'
#'
#'
#' # Define a temporal-difference conditioning agent using the built in algorithm
#' td_cond_agent <- rl_define_new_agent(
#'   model_type = "TD Conditioning",
#'   # Use built in model specification "td_conditioning", setting read = FALSE to
#'   # simply specify the path
#'   simulation_code_file = use_agent_template("td_conditioning", read = FALSE),
#'   # What values must be set for the agent/simulation to work?
#'   required_args = c("num_stimuli", "num_trials", "num_episodes", "gamma", "alpha"),
#'   # Only need to specify reinforcements and stimuli structure
#'   required_methods = c("reinforcements", "stimuli"),
#'   return_agent = TRUE # return the actual RL Agent object
#' )
#'
#' # Initialize a new conditioning agent as tdCond
#' tdCond <- td_cond_agent$new(
#'   model_id = "Classical Conditioning via TDRL",
#'   num_stimuli = 1,
#'   num_trials = 100,
#'   num_episodes = 10,
#'   gamma = 1,
#'   alpha = 0.3
#' )
#'
#' tdCond$
#'   set_stimuli(
#'     list(
#'       one = data.frame(
#'         onset = 3,
#'         offset = 8,
#'         magnitude = 1,
#'         trial = 1:100
#'       )
#'     )
#'   )$
#'   set_reinforcements(
#'     list(
#'       data.frame(
#'         onset = 8,
#'         offset = 8,
#'         magnitude = 1,
#'         trial = 1:100
#'       )
#'     )
#'   )$
#'   simulate_agent()
#'
#'
#'
#'
#' # Define a temporal-difference conditioning agent using the built in algorithm
#' k_armed_agent <- rl_define_new_agent(
#'   model_type = "K-Armed Bandit",
#'   # Use built in model specification "k_armed_bandit", setting read = FALSE to
#'   # simply specify the path
#'   simulation_code_file = use_agent_template("k_armed_bandit", read = FALSE),
#'   # What values must be set for the agent/simulation to work?
#'   required_args = c(
#'     "num_arms", "num_trials", "num_episodes",
#'     "action_episode", "reinforcement_episode",
#'     "gamma", "alpha"
#'   ),
#'   required_methods = c("arms", "policy"), # must specify the arms and policy structure
#'   return_agent = TRUE # return the actual RL Agent object
#' )
#'
#' # Initialize a k-Armed Bandit agent as 'twoArms'
#' twoArms <- k_armed_agent$new(
#'   model_id = "Two Armed Bandit Example",
#'   num_trials = 100,
#'   num_episodes = 4,
#'   num_arms = 2,
#'   action_episode = 2,
#'   reinforcement_episode = 3,
#'   gamma = 1,
#'   alpha = 0.3
#' )
#'
#' # Set the arm structure, action-selection policy, and simulate
#' twoArms$
#'   set_arms(
#'     list(
#'       left = data.frame(
#'         probability = 0.1,
#'         magnitude = 1,
#'         alternative = 0,
#'         trial = 1:100
#'       ),
#'       right = data.frame(
#'         probability = 0.8,
#'         magnitude = 1,
#'         alternative = 0,
#'         trial = 1:100
#'       )
#'     )
#'   )$
#'   set_policy(
#'     policy = "softmax",
#'     tau = 0.5
#'   )$
#'   simulate_agent()
#'
#'
rl_define_new_agent <- function(model_type,
                                simulation_code_file, required_args,
                                required_methods = c(
                                  "reinforcements", "stimuli",
                                  "arms", "policy"
                                ),
                                return_agent = TRUE) {

  # Parse simulation code script
  code_temp <- parse(file = simulation_code_file)

  # Find the indices for functions in the .R file
  function_indexes <- do.call(
    c,
    Filter(
      is.numeric,
      lapply(seq_along(code_temp), function(x) {
        if (as.character(code_temp[[x]][[3]][[1]]) == "function") x else FALSE
      })
    )
  )

  # Assign any functions in the .R file as `utility functions` to be registered
  # for R6 object
  utility_functions <-
    do.call(
      c,
      lapply(function_indexes, function(i) {
        as.expression(
          paste0(code_temp[[i]][[2]], " = ", as.expression(code_temp[[i]][[3]]))
        )
      })
    )

  # Find indices for any objects in the .R file
  object_indexes <- do.call(
    c,
    Filter(
      is.numeric,
      lapply(seq_along(code_temp), function(x) {
        if (as.character(code_temp[[x]][[1]]) == "<-") x else FALSE
      })
    )
  )

  # Find the name for each object (not function)
  object_names <- vapply(object_indexes[!object_indexes %in% function_indexes], function(i) as.character(code_temp[[i]][[2]]), character(1))

  # And initialize as NULL for public methods of R6 object
  public_fields <- c(
    as.expression("model_id = NULL"),
    do.call(
      c,
      lapply(seq_along(object_names), function(i) {
        as.expression(
          paste0(object_names[i], " = NULL")
        )
      })
    ),
    as.expression("stimulus_structure = NULL"),
    as.expression("simulation_code = NULL"),
    as.expression("num_reinforcements = NULL")
  )

  # Make sure required args are able to be initialized
  init_args <- paste("self$", required_args, " <- ", required_args, "\n", sep = "", collapse = "")
  # Register the objects to assign to self after simulations (exclude required_args since those shouldn't change)
  simulation_code_assignments <- paste0("self$", object_names[which(!object_names %in% required_args)], " <- ", object_names[which(!object_names %in% required_args)], "\n", sep = "", collapse = "")

  agent_code <- whisker::whisker.render(
    use_agent_template("define_new_agent"),
    data = list(
      model_type = model_type,
      public_fields = public_fields,
      required_args = required_args,
      required_args_assignment = init_args,
      simulation_file = simulation_code_file,
      simulation_code_assignments = simulation_code_assignments,
      assign_simulation_code_data = paste(required_args, " = ", "self$", required_args, "\n", sep = "", colapse = ""),
      utility_functions = c(utility_functions, ""), # for some reason, using c with empty string adds extra comma needed to separate function in R6 object.
      include_reinforcement_method = ifelse(grep("reinf", required_methods), TRUE, FALSE),
      include_stimuli_method = ifelse(grep("stimul", required_methods), TRUE, FALSE),
      include_arm_method = ifelse(grep("arm", required_methods), TRUE, FALSE),
      include_policy_method = ifelse(grep("pol", required_methods), TRUE, FALSE)
    )
  )

  if (return_agent) {
    eval(
      parse(
        text = agent_code
      )
    )
  } else {
    agent_code
  }
}
