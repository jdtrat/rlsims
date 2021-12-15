
#' R6 Class representing a new Reinforcement Learning Agent
#'
#' Called by \code{\link{rl_new_agent}}.
#'
#' @keywords internal
rl_agent <- R6::R6Class(
  "rlAgent",
  public = list(
    #' @field model_type (character) either "tdrl" or "vprl", referencing the
    #'   reinforcement learning paradigm to perform the simulation.
    model_type = NULL,
    #' @field num_trials (numeric) The number of trials to simulate.
    num_trials = NULL,
    #' @field num_episodes (numeric) The number of episodes per trial.
    num_episodes = NULL,
    #' @field num_cues (numeric) The number of potential cues interacting with the RL agent.
    num_cues = NULL,
    #' @field gamma (numeric) The temporal discounting factor of the RL agent
    gamma = NULL,
    #' @field alpha (numeric) The learning rate of the RL agent
    alpha = NULL,
    #' @field present_cues A three-dimensional array tacking what cues are
    #'   present in each episode across all simulated trials
    present_cues = NULL,
    #' @field estimated_value A three dimensional array tracking the estimated
    #' values of each episode across all trials
    estimated_value = NULL,
    #' @field reward A matrix tracking experienced rewards in each episode
    #'   across all trials
    reward = NULL,
    #' @field RPE A matrix tracking the reward prediction errors occuring in
    #   each episode across all trials
    RPE = NULL,

    #' @description Create a new `rlAgent` object
    #' @param model_type (character) either "tdrl" or "vprl", referencing the
    #'   reinforcement learning paradigm to perform the simulation.
    #' @param num_trials (numeric) The number of trials to simulate.
    #' @param num_episodes (numeric) The number of episodes per trial.
    #' @param num_cues (numeric) The number of potential cues interacting with
    #'   the RL agent.
    #' @param gamma (numeric) The temporal discounting factor of the RL agent
    #' @param alpha (numeric) The learning rate of the RL agent
    initialize = function(model_type,
                          num_trials, num_episodes, num_cues,
                          gamma, alpha) {

      self$model_type <- model_type
      self$num_trials <- num_trials
      self$num_episodes <- num_episodes
      self$num_cues <- num_cues
      self$gamma <- gamma
      self$alpha <- alpha

      self$present_cues <- zeros(dims = c(num_cues, num_episodes, num_trials))
      self$estimated_value <-  zeros(dims = c(num_cues, num_episodes, num_trials))
      self$reward <- zeros(dims = c(num_episodes, num_trials))

    },

    #' @description Printing method for object of class 'rlAgent'.
    #' @param ... NA; printing function
    print = function(...) {
      set_message <- paste0(cli::symbol$tick, " set")
      not_set_message <- paste0(cli::symbol$cross, " not set")
      cli::cli({
        cli::cli_div(id = "parent", theme = list(.rlAgentEl = list(`margin-left` = 2)))
        cli::cli_text("<{.emph {toupper(self$model_type)} Simulation Agent}>")
        cli::cli_par(id = "sim-parameters", class = "rlAgentEl")
        if (private$simulated) cli::cli_text(cli::style_bold(crayon::green("{cli::symbol$tick} Simulations Complete")))
        cli::cli_text('{self$num_trials} {.field Trials}')
        cli::cli_text('{self$num_episodes} {.field Episodes per Trial}')
        cli::cli_text('{self$num_cues} {.field Environmental Cues}')
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
    #' @param reward_onset (Numeric) The episode number a reward presentation
    #'   begins. Either a single number where reward is presented at the same
    #'   episode in all simulated trial or a numeric vector of length
    #'   `num_trials` with reward onset occurring in different episodes.
    #' @param reward_offset (Numeric) The episode number a reward presentation
    #'   ends. Either a single number where reward is presented at the same
    #'   episode in all simulated trial or a numeric vector of length
    #'   `num_trials` with reward offset occurring in different episodes.
    #' @param reward_magnitude (Numeric) The magnitude of the reward. Either a
    #'   single number where reward is presented with the same magnitude across
    #'   all simulated trials or a numeric vector of length `num_trials` with
    #'   reward magnitude differing across trials.
    #' @param keep_reward_structure (Logical) `FALSE` (default) and any
    #'   existing reward structure will be replaced when called. `TRUE` and the
    #'   reward structure will be modified but will not remove previously
    #'   defined rewards.
    #'
    set_reward = function(reward_onset, reward_offset, reward_magnitude, keep_reward_structure = FALSE) {

      reward_lengths <- lengths(list(reward_onset, reward_offset, reward_magnitude))

      if (!all(reward_lengths == self$num_trials)) {

        if (all(reward_lengths == 1)) {
        reward_onset <- rep(reward_onset, length.out = self$num_trials)
        reward_offset <- rep(reward_offset, length.out = self$num_trials)
        reward_magnitude <- rep(reward_magnitude, length.out = self$num_trials)
        } else {
          args <- names(as.list(match.call()))[-1]
          cli::cli_abort("{.arg {args}} must be of length {.emph {crayon::underline(1)}} or {.emph {crayon::underline(self$num_trials)}}")
        }

      }

      if (!all(reward_onset <= reward_offset)) {
        cli::cli_abort("{.arg reward_onset} set to occur after {.arg reward_offset}.")
      }

      if (!keep_reward_structure) {
        self$reward <- zeros(dims = c(self$num_episodes, self$num_trials))
      }

      for (tr in seq_len(self$num_trials)) {
        if (reward_onset[tr] == reward_offset[tr]) {
          self$reward[reward_onset,tr] <- reward_magnitude[tr]
        } else if (reward_onset[tr] < reward_offset[tr]) {
          self$reward[reward_onset[tr]:reward_offset[tr], tr] <- reward_magnitude[tr]
        }
      }

      private$reward_structure_set <- TRUE
      invisible(self)
    },
    #' @description Define the onset episode and offset episode of cues for
    #'   each trial
    #' @param cue_list A list of length `num_cues` where each element contains a
    #'   data frame with columns 'cue', 'onset', 'offset', and 'trial'.
    #' @param keep_cue_structure (Logical) `FALSE` (default) and any
    #'   existing cue structure will be replaced when called. `TRUE` and the
    #'   cue structure will be modified but will not remove previously
    #'   defined cues.
    #' @examples
    #' cue_data_list <- list(
    #'   one = data.frame(
    #'     cue = 1,
    #'     onset = 60,
    #'     offset = 80,
    #'     trial = 1:500),
    #'   two = data.frame(
    #'     cue = 2,
    #'     onset = 70,
    #'     offset = 80,
    #'     trial = 1:500
    #'   )
    #' )
    #'
    set_cues = function(cue_list, keep_cue_structure = FALSE) {

        private$check_cue_list(.cue_list = cue_list)

        cue_data <- do.call(rbind, cue_list)

        if (!keep_cue_structure) {
          self$present_cues <- zeros(dims = c(self$num_cues, self$num_episodes, self$num_trials))
        }

        for (row in 1:nrow(cue_data)) {
          data <- cue_data[row,]
          self$present_cues[data$cue, data$onset:data$offset, data$trial] <- 1
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
      num_trials <- self$num_trials
      num_episodes <- self$num_episodes

      gamma <- self$gamma
      alpha <- self$alpha

      present_cues <- self$present_cues
      estimated_value <- self$estimated_value
      reward <- self$reward

      # Initialize a matrix used in each trial tracking the estimated episode value associated
      # with each cue
      cue_associated_value <- zeros(dims = c(self$num_cues, self$num_episodes))

      # Initialize a matrix tracking the reward prediction errors occurring in each episode
      # across all trials
      RPE <- zeros(dims = c(num_episodes, num_trials))

      # Initialize expected cues; this should be 0 initially if no cues have been
      # experienced then there is no expectation of any
      expected_cues <- rep(0, self$num_cues)

      for (tr in 1:num_trials) {
        for (ep in 1:(num_episodes - 1)) {

          # Actual cues become the ones experienced in a given state (and trial)
          actual_cues <- present_cues[,ep, tr]

          # Define the actual experience as the reward + discounted sum of future values
          # given the actual cues experienced at the current state (and trial)
          actual <-  reward[ep, tr] + sum(gamma * cue_associated_value[,ep + 1] * actual_cues)
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
    get_tidy_pe_data = function(add_trial_zero = TRUE, trial_zero_value = 0) {

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
      check_cue_list = function(.cue_list) {
        if (length(.cue_list) != self$num_cues) {
          cli::cli_abort("{.arg cue_list} must be length {crayon::underline(self$num_cues)}")
        }

        all_dataframes <- vapply(.cue_list, inherits, logical(1), "data.frame")

        correct_dataframe_names <- vapply(.cue_list,
                                          function(x) all(names(x) == c("cue", "onset", "offset", "trial")),
                                          logical(1))

        correct_dataframe_lengths <- vapply(.cue_list,
                                          function(x) nrow(x) == self$num_trials,
                                          logical(1))

        onset_offset_within_bounds <- vapply(.cue_list,
                                             function(x) all(x$onset <= x$offset) && all(x$offset < self$num_episodes),
                                             logical(1))

        if (!all(all_dataframes) || !all(correct_dataframe_names)) {
          cli::cli_abort("Please make sure each element of {.arg cue_list} contains a {.cls data frame} with columns named 'cue', 'onset', 'offset', and 'trial'.")
        }


        if (!all(correct_dataframe_lengths)) {
          cli::cli_abort("Please make sure each element of {.arg cue_list} contains onset and offset values for the correct number of trials ({self$num_trials}).")
        }

        if (!all(onset_offset_within_bounds)) {

          cli::cli_abort("Please make sure each element of {.arg cue_list} contains cue onset values less than cue offset values and that both are less than the number of episodes per trial ({self$num_episodes})")
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
