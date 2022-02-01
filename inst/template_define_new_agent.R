

#' R6 Class representing a new TDRL agent for Conditioning Simulations
#'
#' Called by \code{\link{rl_new_agent}}.
#'
#' @keywords internal
R6::R6Class(
  "{{model_type}}",
  public = list(

    {{{ public_fields }}},
    {{ #include_policy_method }}
      policy = NULL,
    {{ /include_policy_method }}

    initialize = function(model_id, {{{ required_args }}}) {
      self$model_id <- model_id
      {{{ required_args_assignment }}}

      {{ #include_arm_method }}
        check_action_reinforcement_episodes(
          num_eps = num_episodes,
          action_ep = action_episode,
          reinforcement_ep = reinforcement_episode
        )
      {{ /include_arm_method }}

    },

    #' @description Printing method for object of class 'rlAgent'.
    #' @param ... NA; printing function
    print = function(...) {
      cli::cli({
        cli::cli_div(id = "parent", theme = list(.rlAgentEl = list(`margin-left` = 2)))
        cli::cli_text("<{.emph {crayon::underline('{{model_type}}')} Simulation Agent}>")
        cli::cli_text("ID: {.val {self$model_id}}")
        cli::cli_end(id = "parent")
      })
    },
    {{ #include_reinforcement_method }}
    #' @description Define the onset episode and offset episode of reinforcementss for
    #'   each trial
    #' @param reinforcement_input A list of reinforcements where each element
    #'   contains a data frame with columns 'onset', 'offset', 'magnitude', and
    #'   'trial' describing, respectively, the episode number a reinforcements
    #'   presentation begins; the episode number the reinforcements presentation ends;
    #'   the magnitude of the reinforcements; the trials the reinforcementss occur on.
    set_reinforcements = function(reinforcement_input) {

      check_array_list_input(
        .input = reinforcement_input,
        type = "reinforcement",
        num_episodes = self$num_episodes,
        num_trials = self$num_trials
      )

      self$reinforcements <- rlang::enquo(reinforcement_input)

      private$reinforcement_structure_set <- TRUE
      invisible(self)
    },
    {{ /include_reinforcement_method }}
    {{ #include_stimuli_method }}
    #' @description Define the onset episode and offset episode of stimuli for each
    #'   trial.
    #' @param stimulus_input A list of stimuli where each element contains a data frame
    #'   with columns 'onset', 'offset', 'magnitude', and 'trial' describing,
    #'   respectively, the episode number a stimulus presentation begins; the episode
    #'   number the stimulus presentation ends; the magnitude (saliency of a stimulus) of
    #'   the stimulus; the trials the stimuli occur.
    set_stimuli = function(stimulus_input) {

      check_array_list_input(
        .input = stimulus_input,
        type = "stimulus",
        num_episodes = self$num_episodes,
        num_trials = self$num_trials,
        num_stimuli = self$num_stimuli
      )

      self$stimulus_structure <- rlang::enquo(stimulus_input)

      private$stimulus_structure_set <- TRUE
      invisible(self)
    },
    {{ /include_stimuli_method }}
    {{ #include_arm_method }}
      #' @description Define the arm structure for an agent
      #' @param arm_input A list of arm definitions where each element
      #'   contains a data frame with columns 'probability', 'magnitude',
      #'   'alternative', and 'trial' describing, respectively, the `probability`
      #'   of receiving a reward `magnitude` with the `alternative` for each
      #'   `trial`.
      set_arms = function(arm_input) {

        check_arm_input(.input = arm_input,
                        num_arms = self$num_arms,
                        num_trials = self$num_trials)

        self$arm_structure <- rlang::enquo(arm_input)

        private$arm_structure_set <- TRUE
        invisible(self)
      },
    {{ /include_arm_method }}
    {{ # include_policy_method }}
      #' @description Set an Agent's Action-Selection Policy
      #' @param policy What policy should a decision be made under? Currently
      #'   supported are softmax, greedy, and epsilon-greedy.
      #' @param ... Additional arguments passed to or from specific methods, such as
      #'   `tau` when `policy = "softmax"` and `epsilon` when `policy =
      #'   "epsilonGreedy"`.
      set_policy = function(policy, ...) {

        self$policy <- rl_set_policy_internal(policy = policy,
                                              ... = ...)

        private$policy_set <- TRUE
        invisible(self)
      },
    {{ /include_policy_method }}
    #' @description Simulate the RL Agent
    simulate_agent = function() {

      if (!private$ready_to_simulate()) {
        cli::cli_abort("Please make sure all the required structures (e.g., reinforcements, arms, etc.) are set before simulating.")
      }

      self$simulation_code <- whisker::whisker.render(
        readLines( "{{ simulation_file }}" ),
        data = list(
          {{ #include_reinforcement_method }}
          reinforcement_structure = rlang::expr_text(rlang::eval_tidy(self$reinforcements)),
          {{ /include_reinforcement_method }}
          {{ #include_stimuli_method }}
          stimulus_structure = rlang::expr_text(rlang::eval_tidy(self$stimulus_structure)),
          {{ /include_stimuli_method }}
          {{ #include_arm_method }}
          arm_structure = rlang::expr_text(rlang::eval_tidy(self$arm_structure)),
          simulate_function = private$rl_action_simulate_function(),
          {{ /include_arm_method }}
          {{ assign_simulation_code_data }}
        )
      )

      eval(parse(text = self$simulation_code))

      {{{ simulation_code_assignments }}}

      # Log that simulation has occurred
      private$simulated <- TRUE

      invisible(self)
    },
    {{{ utility_functions }}}
    #' @description Retrieve the code needed to simulate the agent.
    get_simulation_code = function() {
      self$simulation_code
    }
  ),
  private = list(
    {{ #include_arm_method }}
    # Indicator for whether the arm structure was set (e.g., was the method `set_arms` called?)
    arm_structure_set = FALSE,
    {{ /include_arm_method }}
    {{ #include_reinforcement_method }}
    # Indicator for whether the reinforcements structure was set (e.g., was the method `set_reinforcements` called?)
    reinforcement_structure_set = FALSE,
    {{ /include_reinforcement_method }}
    {{ #include_stimuli_method }}
    # Indicator for whether the stimulus structure was set (e.g., was the method `set_stimuli` called?)
    stimulus_structure_set = FALSE,
    {{ /include_stimuli_method }}
    {{ # include_policy_method }}
    # Indicator for whether the policy was set (e.g., was the method `set_policy` called?)
    policy_set = FALSE,
    # Return the parsed, but unevaluated function to simulate an action given the policy
    rl_action_simulate_function = function() {
      if (self$policy$type == "greedy") {
        parse(text = 'rl_action_simulate(policy = "greedy", values = Q_val[, ep])')
      } else {
        parse(text = paste0('rl_action_simulate(policy = "', self$policy$type, '", values = Q_val[, ep], ', names(self$policy[2]), ' = ', self$policy[[2]], ')'))
      }
    },
    {{ /include_policy_method }}
    # Indicator for whether simulations have been done with this agent.
    simulated = FALSE,
    ready_to_simulate = function() {

      # Get all vars from private environment as list
      private_vars <- eapply(env = private, function(x) x)
      # Check if all the ones that need to be set to simulate are
      all(vapply(X = private_vars[endsWith(names(private_vars), "_set")],
                 FUN =  "[",
                 FUN.VALUE = logical(1))
      )
    }
  )
)



