
#' R6 Class representing a new TDRL agent for Conditioning Simulations
#'
#' Called by \code{\link{rl_new_agent}}.
#'
#' @keywords internal
agent_tdrl_conditioning <- R6::R6Class(
  "tdrlConditioning",
  public = list(
    #' @field model_id (character) A model identifier referencing the
    #'   reinforcement learning paradigm to perform the simulation.
    model_id = NULL,
    #' @field num_trials (numeric) The number of trials to simulate.
    num_trials = NULL,
    #' @field num_episodes (numeric) The number of episodes per trial.
    num_episodes = NULL,
    #' @field gamma (numeric) The temporal discounting factor of the RL agent
    gamma = NULL,
    #' @field alpha (numeric) The learning rate of the RL agent
    alpha = NULL,
    #' @field num_cues (numeric) The number of potential cues interacting with the RL agent.
    num_cues = NULL,
    #' @field present_cues A three-dimensional array tacking what cues are
    #'   present in each episode across all simulated trials
    present_cues = NULL,
    #' @field estimated_value A three dimensional array tracking the estimated
    #' values of each episode across all trials
    estimated_value = NULL,
    #' @field num_reinforcements (numeric) The number of potential reinforcements
    #'   experienced by the RL agent.
    num_reinforcements = NULL,
    #' @field reinforcements A matrix tracking experienced rewards in each episode
    #'   across all trials
    reinforcements = NULL,
    #' @field RPE A matrix tracking the reward prediction errors occuring in
    #   each episode across all trials
    RPE = NULL,

    #' @description Create a new `tdrlConditioning` object
    #' @param model_id model_id (character) A model identifier referencing the
    #'   reinforcement learning paradigm to perform the simulation.
    #' @param num_trials (numeric) The number of trials to simulate.
    #' @param num_episodes (numeric) The number of episodes per trial.
    #' @param gamma (numeric) The temporal discounting factor of the RL agent
    #' @param alpha (numeric) The learning rate of the RL agent
    initialize = function(model_id,
                          num_trials, num_episodes,
                          gamma, alpha) {

      self$model_id <- model_id
      self$num_trials <- num_trials
      self$num_episodes <- num_episodes
      self$gamma <- gamma
      self$alpha <- alpha

    },

    #' @description Printing method for object of class 'rlAgent'.
    #' @param ... NA; printing function
    print = function(...) {
      set_message <- paste0(cli::symbol$tick, " set")
      not_set_message <- paste0(cli::symbol$cross, " not set")
      cli::cli({
        cli::cli_div(id = "parent", theme = list(.rlAgentEl = list(`margin-left` = 2)))
        cli::cli_text("<{.emph {crayon::underline('tdrlConditioning')} Simulation Agent}>")
        cli::cli_text("ID: {.val {self$model_id}}")
        cli::cli_par(id = "sim-parameters", class = "rlAgentEl")
        if (private$simulated) cli::cli_text(cli::style_bold(crayon::green("{cli::symbol$tick} Simulations Complete")))
        cli::cli_text('{self$num_trials} {.field Trials}')
        cli::cli_text('{self$num_episodes} {.field Episodes per Trial}')
        cli::cli_text('{.field Temporal Discounting Factor} of {self$gamma}')
        cli::cli_text('{.field Learnrate} of {self$alpha}')
        if (!private$reward_structure_set) {
          cli::cli_text("{.field Reward Structure:} {crayon::underline(cli::make_ansi_style('pink')(cli::style_bold({not_set_message})))}")
        } else if (private$reward_structure_set) {
          cli::cli_text("{.field Reward Structure:} {crayon::underline(cli::make_ansi_style('cyan')(cli::style_bold({set_message})))}")
        }
        if (!private$cue_structure_set) {
          cli::cli_text("{.field Cue Structure:} {crayon::underline(cli::make_ansi_style('pink')(cli::style_bold({not_set_message})))}")
        } else if (private$cue_structure_set) {
          cli::cli_text("{.field Cue Structure:} {crayon::underline(cli::make_ansi_style('cyan')(cli::style_bold({set_message})))}")
        }
        cli::cli_end(id = "model-type")
        cli::cli_end(id = "parent")
      })
    },
    #' @description Define the onset episode and offset episode of rewards for
    #'   each trial
    #' @param reinforcement_input A list of reinforcements where each element
    #'   contains a data frame with columns 'onset', 'offset', 'magnitude', and
    #'   'trial' describing, respectively, the episode number a reward
    #'   presentation begins; the episode number the reward presentation ends;
    #'   the magnitude of the reward; the trials the rewards occur on.
    #' @param keep_reward_structure (Logical) `FALSE` (default) and any existing
    #'   reward structure will be replaced when called. `TRUE` and the reward
    #'   structure will be modified but will not remove previously defined
    #'   rewards.
    #'
    set_reinforcements = function(reinforcement_input, keep_reward_structure = FALSE) {

      private$check_input(.input = reinforcement_input, type = "reinforcement")

      self$num_reinforcements <- length(reinforcement_input)

      if (!keep_reward_structure) {
        self$reinforcements <- zeros(dims = c(self$num_reinforcements, self$num_episodes, self$num_trials))
      }

      reinforcement_data <- do.call(rbind,
                                    lapply(seq_along(reinforcement_input), function(x) {
                                      cbind(reinforcement_number = x, reinforcement_input[[x]])
                                    })
      )

      for (row in 1:nrow(reinforcement_data)) {
        data <- reinforcement_data[row,]
        if (data$onset == data$offset) {
          self$reinforcements[data$reinforcement_number, data$onset, data$trial] <- data$magnitude
        } else if (data$onset < data$offset) {
          self$reinforcements[data$reinforcement_number, data$onset:data$offset, data$trial] <- data$magnitude
        }
      }

      private$reward_structure_set <- TRUE
      invisible(self)
    },
    #' @description Define the onset episode and offset episode of cues for each
    #'   trial.
    #' @param cue_input A list of cues where each element contains a data frame
    #'   with columns 'onset', 'offset', 'magnitude', and 'trial' describing,
    #'   respectively, the episode number a cue presentation begins; the episode
    #'   number the cue presentation ends; the magnitude (saliency of a cue) of
    #'   the cue; the trials the cues occur.
    #' @param keep_cue_structure (Logical) `FALSE` (default) and any existing
    #'   cue structure will be replaced when called. `TRUE` and the cue
    #'   structure will be modified but will not remove previously defined cues.
    #'
    set_cues = function(cue_input, keep_cue_structure = FALSE) {

      private$check_input(.input = cue_input, type = "cue")

      self$num_cues <- length(cue_input)

      cue_data <- do.call(rbind,
                          lapply(seq_along(cue_input), function(x) {
                            cbind(cue_number = x, cue_input[[x]])
                          })
      )

      if (!keep_cue_structure) {
        self$present_cues <- zeros(dims = c(self$num_cues, self$num_episodes, self$num_trials))
      }


      for (row in 1:nrow(cue_data)) {
        data <- cue_data[row,]
        if (data$onset == data$offset) {
          self$present_cues[data$cue_number, data$onset, data$trial] <- data$magnitude
        } else if (data$onset < data$offset) {
          self$present_cues[data$cue_number, data$onset:data$offset, data$trial] <- data$magnitude
        }
      }

      private$cue_structure_set <- TRUE
      invisible(self)

    },
    #' @description Simulate the RL Agent
    simulate_agent = function() {

      if (!private$reward_structure_set & !private$cue_structure_set) {
        cli::cli_abort("Please set the reward and cue structures before simulating.")
      } else if (!private$reward_structure_set) {
        cli::cli_abort("Please set the reward structure before simulating.")
      } else if (!private$cue_structure_set) {
        cli::cli_abort("Please set the cue structure before simulating.")
      }

      # Assign values for local use -- allows portability of main simulation
      # code.
      num_cues <- self$num_cues
      num_trials <- self$num_trials
      num_episodes <- self$num_episodes

      gamma <- self$gamma
      alpha <- self$alpha

      present_cues <- self$present_cues
      estimated_value <- zeros(dims = c(num_cues, num_episodes, num_trials))
      reinforcements <- self$reinforcements

      # Initialize a matrix used in each trial tracking the estimated episode value associated
      # with each cue
      cue_associated_value <- zeros(dims = c(num_cues, num_episodes))

      # Initialize a matrix tracking the reward prediction errors occurring in each episode
      # across all trials
      RPE <- zeros(dims = c(num_episodes, num_trials))

      # Initialize expected cues; this should be 0 initially if no cues have been
      # experienced then there is no expectation of any
      expected_cues <- rep(0, num_cues)

      for (tr in 1:num_trials) {
        for (ep in 1:(num_episodes - 1)) {

          # Actual cues become the ones experienced in a given state (and trial)
          actual_cues <- present_cues[,ep, tr]

          # Define the actual experience as the reinforcements + discounted sum of future values
          # given the actual cues experienced at the current state (and trial)
          actual <-  sum(reinforcements[,ep, tr]) + sum(gamma * cue_associated_value[,ep + 1] * actual_cues)
          # Define the expected value of the current state given the current value estimate
          # and the cues expected to be experienced in this state.
          expected <- sum(cue_associated_value[,ep] * expected_cues)

          # Prediction error is the difference between actual experience and
          # expected experience
          pe <- actual - expected

          # Update value estimate of the current state given the expected value
          # (previous estimate and expected cues) times the learning rate and
          # prediction error
          cue_associated_value[,ep] <- cue_associated_value[,ep] + expected_cues * alpha * pe # line 2

          # Save running total of estimated state value for plotting
          estimated_value[, ep, tr] <- cue_associated_value[,ep] # line 3

          # Save running total of the prediction error for plotting
          RPE[ep, tr] <- pe # line 4

          # Update your expectation of the cues given your latest experience
          expected_cues <- actual_cues

        }
      }

      # Save RPE and estimated value (present_cues was not changed, so it
      # remains the same as set with `set_cues`, and reward was not changed, so
      # it remains the same as set with `set_reward`)
      self$RPE <- RPE
      self$estimated_value <- estimated_value

      # Log that simulation has occurred
      private$simulated <- TRUE

      invisible(self)

    },
    #' @description Convert the agent's estimated values array where, for each
    #'   cue, there is a matrix where each row is an episode and each column
    #'   is a trial that contains the estimated value, to a dataframe with columns
    #'   'trial', 'episode', 'cue', and 'value'.
    #'
    #' @param add_trial_zero (Logical) `TRUE` by default and trial zero will be
    #'   appended to the prediction error data frame with values from
    #'   `trial_zero_value`. `FALSE` and output will begin at trial one.
    #' @param trial_zero_value (Numeric) Either a single value (default is 0) or
    #'   a vector of values with length `num_arms` times `num_episodes` to
    #'   append for trial 0.
    #' @return A dataframe with the agent's simulated learned values ('value')
    #'   a given cue corresponds to for each episode across all trials.
    get_learned_values = function(add_trial_zero = TRUE, trial_zero_value = 0) {

      if (!private$simulated) {
        cli::cli_abort("Cannot access learned values data. Please double check the simulations occurred.")
      }
      # Essentially a nested for loop but with lapply
      # saying for (ac in seq_len(num_cues)) {
      # for (tr in seq_len(num_trials)) {
      # create data frame with trial, episode, cue and the associated value
      # }
      # }
      out <- do.call(rbind,
                     lapply(seq_len(self$num_cues), function(c) {
                       do.call(rbind,
                               lapply(seq_len(self$num_trials),
                                      function(tr) {
                                        data.frame(
                                          trial = round(tr),
                                          episode = seq_len(self$num_episodes),
                                          cue = c,
                                          value = self$estimated_value[c,,tr]
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
                      cue = seq_len(self$num_cues),
                      value = trial_zero_value
          ),
          out
        )
      }

      # Order the output by trial
      out <- out[order(out$trial),,drop = FALSE]
      rownames(out) <- NULL
      return(out)
    },
    #' @description Convert the agent's simulated reward prediction errors from
    #'   a matrix where each row is an episode and each column is a trial to a
    #'   dataframe with columns 'trial', 'episode', and 'value'.
    #' @param add_trial_zero (Logical) `TRUE` by default and trial zero will be
    #'   appended to the prediction error data frame with values from
    #'   `trial_zero_value`. `FALSE` and output will begin at trial one.
    #' @param trial_zero_value (Numeric) Either a single value (default is 0) or
    #'   a vector of values to append for trial 0.
    #' @return A dataframe with the agent's simulated reward prediction errors
    #'   ('value') for each episode across trials.
    get_pe_data = function(add_trial_zero = TRUE, trial_zero_value = 0) {

      if (!private$simulated) {
        cli::cli_abort("Cannot access prediction error data. Please double check the simulations occurred.")
      }

      out <- do.call(
        rbind,
        lapply(seq_len(self$num_trials), function(.x) {
          data.frame(trial = round(.x),
                     episode = seq_len(self$num_episodes),
                     value = self$RPE[,.x]
          )
        })
      )

      if (add_trial_zero) {
        rbind(
          data.frame(trial = 0,
                     episode = seq_len(self$num_episodes),
                     value = trial_zero_value
          ),
          out
        )
      } else if (!add_trial_zero) {
        out
      }
    }
  ),
  private = list(
    check_input = function(.input, type) {
      arg_input <- switch(type,
                          cue = "cue_input",
                          reinforcement = "reinforcement_input")

      if (!inherits(.input, "list")) cli::cli_abort("{.arg {arg_input}} must be a list of dataframes")
      all_dataframes <- vapply(.input, inherits, logical(1), "data.frame")
      correct_dataframe_names <- vapply(.input,
                                        function(x) all(names(x) == c("onset", "offset", "magnitude", "trial")),
                                        logical(1))
      correct_dataframe_lengths <- vapply(.input,
                                          function(x) nrow(x) <= self$num_trials,
                                          logical(1))
      onset_offset_within_bounds <- vapply(.input,
                                           function(x) all(x$onset <= x$offset) && all(x$offset <= self$num_episodes),
                                           logical(1))

      if (!all(all_dataframes) || !all(correct_dataframe_names)) {
        cli::cli_abort("Please make sure each element of {.arg {arg_input}} contains a {.cls data frame} with columns named 'onset', 'offset', 'magnitude', and 'trial'.")
      }


      if (!all(correct_dataframe_lengths)) {
        cli::cli_abort("Please make sure each element of {.arg {arg_input}} contains at most one onset and offset value per trial (no more than {self$num_trials}).")
      }

      if (!all(onset_offset_within_bounds)) {

        cli::cli_abort("Please make sure each element of {.arg {arg_input}} contains cue onset values less than cue offset values and that both are less than the number of episodes per trial ({self$num_episodes})")
      }


    },
    # Indicator for whether the reward structure was set (e.g., was the method `set_reward` called?)
    reward_structure_set = FALSE,
    # Indicator for whether the cue structure was set (e.g., was the method `set_cues` called?)
    cue_structure_set = FALSE,
    # Indicator for whether simulations have been done with this agent.
    simulated = FALSE
  )
)
