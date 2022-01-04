#' R6 Class representing a K Armed Bandit Agent
#'
#' Called by \code{\link{rl_new_agent}}.
#'
#' @keywords internal
agent_k_armed_bandit <- R6::R6Class(
  "kArmedBandit",
  public = list(
    #' @field model_id (character) A model identifier referencing the
    #'   reinforcement learning paradigm to perform the simulation.
    model_id = NULL,
    #' @field num_trials (numeric) The number of trials to simulate.
    num_trials = NULL,
    #' @field num_episodes (numeric) The number of episodes per trial.
    num_episodes = NULL,
    #' @field num_arms (numeric) The number of arms (options) the agent can sample from.
    num_arms = NULL,
    #' @field gamma (numeric) The temporal discounting factor of the RL agent
    gamma = NULL,
    #' @field alpha (numeric) The learning rate of the RL agent
    alpha = NULL,
    #' @field action_episode (numeric) The episode number in which the agent
    #'   takes an action.
    action_episode = NULL,
    #' @field reinforcement_episode (numeric) The episode number in which the
    #'   agent receives the reinforcement after taking an action.
    reinforcement_episode = NULL,
    #' @field policy (list) A list containing the information relevant to the
    #'   agent's decision policy, such as 'softmax' or 'epsilon-greedy' that
    #'   guides choice behavior
    policy = NULL,
    #' @field actions A numeric vector containing the index of which arm was
    #'   selected on a given trial.
    actions = NULL,
    #' @field arm_structure A list of arm definitions where each element
    #'   contains a data frame with columns 'probability', 'magnitude',
    #'   'alternative', and 'trial' describing, respectively, the `probability`
    #'   of receiving a reward `magnitude` with the `alternative` for each
    #'   `trial`.
    arm_structure = NULL,
    #' @field Q_values A three dimensional array tracking the estimated Q-value
    #'   for selecting each arm across all episodes and trials
    Q_values = NULL,
    #' @field prediction_errors A three dimensional array tracking the
    #'   prediction errors associated with choosing each arm across all episodes
    #'   and trials
    prediction_errors = NULL,
    #' @field reinforcements A matrix tracking the experienced reinforcements
    #'   associated with choosing an arm across episodes within a given trial
    reinforcements = NULL,

    #' @description Create a new `kArmedBandit` object
    #'
    #' @param model_id model_id (character) A model identifier referencing the
    #'   reinforcement learning paradigm to perform the simulation.
    #' @param num_trials (numeric) The number of trials to simulate.
    #' @param num_episodes (numeric) The number of episodes per trial.
    #' @param num_arms (numeric) The number of arms (options) the agent can
    #'   sample from.
    #' @param action_episode (numeric) The episode an action should be taken on.
    #' @param reinforcement_episode (numeric) The episode reinforcements hsould
    #'   occur on.
    #' @param gamma (numeric) The temporal discounting factor of the RL agent
    #' @param alpha (numeric) The learning rate of the RL agent
    initialize = function(model_id,
                          num_trials, num_episodes, num_arms,
                          action_episode, reinforcement_episode,
                          gamma, alpha) {

      self$model_id <- model_id

      self$num_trials <- num_trials
      self$num_episodes <- num_episodes
      self$num_arms <- num_arms

      self$gamma <- gamma
      self$alpha <- alpha

      self$action_episode <- action_episode
      self$reinforcement_episode <- reinforcement_episode

    },

    #' @description Printing method for object of class 'rlAgent'.
    #' @param ... NA; printing function
    print = function(...) {
      set_message <- paste0(cli::symbol$tick, " set")
      not_set_message <- paste0(cli::symbol$cross, " not set")
      cli::cli({
        cli::cli_div(id = "parent", theme = list(.rlAgentEl = list(`margin-left` = 2),
                                                 .paramSpecifics = list(`margin-left` = 3)))
        cli::cli_text("<{.emph {crayon::underline('kArmedBandit')} Simulation Agent}>")
        cli::cli_text("ID: {.val {self$model_id}}")
        cli::cli_par(id = "sim-parameters", class = "rlAgentEl")
        if (private$simulated) cli::cli_text(cli::style_bold(crayon::green("{cli::symbol$tick} Simulations Complete")))
        cli::cli_text("{.val {self$num_arms}}-Armed Bandit")
        cli::cli_text('{.val {self$num_trials}} Trials')
        cli::cli_text('{.val {self$num_episodes}} Episodes per Trial')
        cli::cli_par(id = "episode-subset", class = "paramSpecifics")
        cli::cli_ul("Action Episode: {.val {self$action_episode}}")
        cli::cli_ul("Reinforcement Episode: {.val {self$reinforcement_episode}}")
        cli::cli_end(id = "episode-subset")
        cli::cli_text('Temporal Discounting Factor of {.val {self$gamma}}')
        cli::cli_text('Learnrate of {.val {self$alpha}}')
        if (!private$arm_structure_set) {
          cli::cli_text("Arm Structure: {crayon::underline(cli::make_ansi_style('pink')(cli::style_bold({not_set_message})))}")
        } else if (private$arm_structure_set) {
          cli::cli_text("Arm Structure: {crayon::underline(cli::make_ansi_style('cyan')(cli::style_bold({set_message})))}")
        }
        if (!private$policy_set) {
          cli::cli_text("Action-Selection Policy: {crayon::underline(cli::make_ansi_style('pink')(cli::style_bold({not_set_message})))}")
        } else if (private$policy_set) {
          cli::cli_text("Action-Selection Policy: {crayon::underline(cli::make_ansi_style('cyan')(cli::style_bold({set_message})))}")
          cli::cli_par(id = "policy-details", class = "paramSpecifics")
          cli::cli_ul("Policy Type: {.val {self$policy$type}}")
          if (self$policy$type == "greedy") {
            cli::cli_ul("Parameter: {.val Not Applicable}")
          } else {
            cli::cli_ul("Parameter: {.arg {names(self$policy[2])}} = {.val {self$policy[[2]]}}")
          }
          cli::cli_end(id = "policy-details")
        }
        cli::cli_end(id = "model-type")
        cli::cli_end(id = "parent")
      })
    },
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

    },
    #' @description Define the arm structure for an agent
    #' @param arm_input A list of arm definitions where each element
    #'   contains a data frame with columns 'probability', 'magnitude',
    #'   'alternative', and 'trial' describing, respectively, the `probability`
    #'   of receiving a reward `magnitude` with the `alternative` for each
    #'   `trial`.
    #' @param keep_arm_structure (Logical) `FALSE` (default) and any existing
    #'   arm structure will be replaced when called. `TRUE` and the arm
    #'   structure will be modified but will not remove previously defined
    #'   rewards.
    #'
    set_arms = function(arm_input, keep_arm_structure = FALSE) {

      private$check_arm_input(.input = arm_input)

      self$num_arms <- length(arm_input)

      if (!keep_arm_structure) {
        self$arm_structure <- arm_input
      } else if (keep_arm_structure & private$arm_structure_set) {
        cli::cli_alert_info("Arm structure already exists and {.arg keep_arm_structure = FALSE},
                            so it will not be overwritten.")
      }

      private$arm_structure_set <- TRUE
      invisible(self)
    },
    #' @description Retrieve the list of arm definitions
    #' @return A list of data frames
    get_arms = function() {
      if (private$arm_structure_set) {
        self$arms
      } else {
        cli::cli_alert_danger("Arm structure not set.")
      }
    },
    #' @description Simulate the RL Agent
    simulate_agent = function() {

      if (!private$arm_structure_set) {
        cli::cli_abort("Please set the arm structures before simulating.")
      }

      # Assign values for local use -- allows portability of main simulation
      # code.
      num_arms <- self$num_arms
      num_trials <- self$num_trials
      num_episodes <- self$num_episodes

      action_episode <- self$action_episode
      reinforcement_episode <- self$reinforcement_episode

      gamma <- self$gamma
      alpha <- self$alpha

      simulate_func <- private$rl_action_simulate_function()

      # Create three dimensional arrays for saving the Q values and prediction
      # errors from the entire task
      Q_values <- zeros(dims = c(num_arms, num_episodes, num_trials))
      prediction_errors <- zeros(dims = c(num_arms, num_episodes, num_trials))

      # Create matrices for locally updating Q values
      Q_val <- zeros(dims = c(num_arms, num_episodes))
      # Create matrices for locally updating prediction errors
      pe <- zeros(dims = c(num_arms, num_episodes))

      # Create matrices for storing the experienced reinforcements
      reinforcements <- zeros(dims = c(num_episodes, num_trials))

      # Create a vector for storing the actions taken
      actions <- numeric(length = num_trials)

      # For each trial
      for (tr in 1:num_trials) {
        # For each episode (except the terminal one)
        for (ep in 1:(num_episodes - 1)) {
          # If the episode is when the action is taken, simulate an action with
          # said policy. This is the only part of the `simulate_agent` code that
          # is not directly applicable on its own. We have implemented some fun
          # parsing of unevaluated expressions to simulate the correct action
          # given the policy.
          #
          # As an example of simulating with a softmax decision policy, you
          # could replace 'eval(simulate_func)' with: actions[tr] <- rl_action_simulate(policy =
          # "softmax", values = Q_val[, ep], tau = 8)
          if (ep == action_episode) {
            actions[tr] <- eval(simulate_func)

            reinforcements[reinforcement_episode, tr] <- rl_arms_get_outcome(arm_definitions = self$arm_structure,
                                                                             action = actions[tr],
                                                                             trial = tr)
          }
          # Define the actual experience as the reinforcement after choosing a given
          # arm plus the discounted sum of future values given the action taken during the
          # current state (and trial)
          actual <-  reinforcements[ep, tr] + (gamma * Q_val[actions[tr], ep + 1])
          # Define the expected value of the current state given the current Q value estimate
          expected <- Q_val[actions[tr], ep]

          # Prediction error for a given action on a specific episode is the
          # difference between actual experience of selecting an arm and expected experience
          pe[actions[tr], ep] <- actual - expected

          # Update Q value estimate state given the expected value (previous estimated
          # Q value) plus the learning rate times prediction error from having taken
          # an action
          Q_val[actions[tr],ep] <- expected + (alpha * pe[actions[tr], ep])

          # Save running total of estimated Q value and prediction error for plotting
          for (ac in 1:num_arms) {
            Q_values[ac, ep, tr] <- Q_val[ac, ep]
            prediction_errors[ac, ep, tr] <- pe[ac, ep]
          }
        }

      }

      # Save prediction errors, Q_values, actions, and reinforcements
      self$prediction_errors <- prediction_errors
      self$Q_values <- Q_values
      self$actions <- actions
      self$reinforcements <- reinforcements

      # Log that simulation has occurred
      private$simulated <- TRUE

      invisible(self)

    },
    #' @description Convert the agent's estimated Q values array where, for each
    #'   action, there is an matrix where each row is an episode and each column
    #'   is a trial that contains the q_value, to a dataframe with columns
    #'   'trial', 'episode', 'action', and 'q_value'.
    #'
    #' @param add_trial_zero (Logical) `TRUE` by default and trial zero will be
    #'   appended to the prediction error data frame with values from
    #'   `trial_zero_value`. `FALSE` and output will begin at trial one.
    #' @param trial_zero_value (Numeric) Either a single value (default is 0) or
    #'   a vector of values with length `num_arms` times `num_episodes` to
    #'   append for trial 0.
    #' @return A dataframe with the agent's simulated learned values ('q_value')
    #'   from taking an action for each episode across all trials.
    get_learned_values = function(add_trial_zero = TRUE, trial_zero_value = 0) {

      if (!private$simulated) {
        cli::cli_abort("Cannot access learned values data. Please double check the simulations occurred.")
      }
        # Essentially a nested for loop but with lapply
        # saying for (ac in seq_len(num_actions)) {
        # for (tr in seq_len(num_trials)) {
        # create data frame with trial, episode, action and the associated q value
        # }
        # }
        out <- do.call(rbind,
                       lapply(seq_len(self$num_arms), function(ac) {
                         do.call(rbind,
                                 lapply(seq_len(self$num_trials),
                                        function(tr) {
                                          data.frame(
                                            trial = round(tr),
                                            episode = seq_len(self$num_episodes),
                                            arm = ac,
                                            q_value = self$Q_values[ac,,tr]
                                          )
                                        }
                                 )
                         )
                       }
                       )
        )

        if (add_trial_zero) {
          out <- rbind(
            expand.grid(trial = 0,
                        episode = seq_len(self$num_episodes),
                        arm = seq_len(self$num_arms),
                        q_value = trial_zero_value
            ),
            out
          )
        }

        # Order the output by trial
        out <- out[order(out$trial),,drop = FALSE]
        rownames(out) <- NULL
        return(out)
    },
    #' @description Convert the agent's prediction error array where, for each
    #'   action, there is an matrix with each row corresponding to an episode
    #'   and each column a trial with the experienced prediction error, to a
    #'   dataframe with columns 'trial', 'episode', 'action', and 'value'.
    #' @param add_trial_zero (Logical) `TRUE` by default and trial zero will be
    #'   appended to the prediction error data frame with values from
    #'   `trial_zero_value`. `FALSE` and output will begin at trial one.
    #' @param trial_zero_value (Numeric) Either a single value (default is 0) or
    #'   a vector of values with length `num_arms` times `num_episodes` to
    #'   append for trial 0.
    #' @return A dataframe with the agent's simulated prediction errors from
    #'   taking an action for each episode across all trials.
    get_pe_data = function(add_trial_zero = TRUE, trial_zero_value = 0) {

      if (!private$simulated) {
        cli::cli_abort("Cannot access prediction error data. Please double check the simulations occurred.")
      }


      if (!private$simulated) {
        cli::cli_abort("Cannot access learned values data. Please double check the simulations occurred.")
      }
      # Essentially a nested for loop but with lapply
      # saying for (ac in seq_len(num_actions)) {
      # for (tr in seq_len(num_trials)) {
      # create data frame with trial, episode, action and the associated q value
      # }
      # }
      out <- do.call(rbind,
                     lapply(seq_len(self$num_arms), function(ac) {
                       do.call(rbind,
                               lapply(seq_len(self$num_trials),
                                      function(tr) {
                                        data.frame(
                                          trial = round(tr),
                                          episode = seq_len(self$num_episodes),
                                          arm = ac,
                                          value = self$prediction_errors[ac,,tr]
                                        )
                                      }
                               )
                       )
                     }
                     )
      )

      if (add_trial_zero) {
        out <- rbind(
          expand.grid(trial = 0,
                      episode = seq_len(self$num_episodes),
                      arm = seq_len(self$num_arms),
                      value = trial_zero_value
          ),
          out
        )
      }

      # Order the output by trial
      out <- out[order(out$trial),,drop = FALSE]
      rownames(out) <- NULL
      return(out)
    }
  ),
  private = list(
    check_arm_input = function(.input, type) {

      if (!inherits(.input, "list")) cli::cli_abort("{.arg {arm_input}} must be a list of dataframes. Try creating them with {.fn define_arm}.")
      all_dataframes <- vapply(.input, inherits, logical(1), "data.frame")
      correct_dataframe_names <- vapply(.input,
                                        function(x) all(names(x) == c("probability", "magnitude", "alternative", "trial")),
                                        logical(1))
      correct_dataframe_lengths <- vapply(.input,
                                          function(x) nrow(x) <= self$num_trials,
                                          logical(1))

      if (!all(all_dataframes) || !all(correct_dataframe_names)) {
        cli::cli_abort("Please make sure each element of {.arg {arg_input}} contains a {.cls data frame} with columns named 'onset', 'offset', 'magnitude', and 'trial'.")
      }


      if (!all(correct_dataframe_lengths)) {
        cli::cli_abort("Please make sure each element of {.arg {arg_input}} contains at most one onset and offset value per trial (no more than {self$num_trials}).")
      }

    },
    # Return the parsed, but unevaluated function to simulate an action given the policy
    rl_action_simulate_function = function() {
        if (self$policy$type == "greedy") {
          parse(text = 'rl_action_simulate(policy = "greedy", values = Q_val[, ep])')
        } else {
          parse(text = paste0('rl_action_simulate(policy = "', self$policy$type, '", values = Q_val[, ep], ', names(self$policy[2]), ' = ', self$policy[[2]], ')'))
        }
    },
    # Indicator for whether the reward structure was set (e.g., was the method `set_reward` called?)
    arm_structure_set = FALSE,
    # Indicator for whether the agent's action-selection policy was set (e.g.,
    # was the method `set_policy` called?)
    policy_set = FALSE,
    # Indicator for whether simulations have been done with this agent.
    simulated = FALSE
  )
)
