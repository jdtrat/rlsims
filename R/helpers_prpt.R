prpt <- new.env(parent = emptyenv())

#' Set up overall PRPT task structure and
#'
#' @keywords internal
#' @noRd
#' @param num_iters Number of iterations
#' @param num_trials Number of trials
#'
setup_PRPT <- function(num_iters, num_trials) {

  # set up number of iterations
  prpt$num_iters <- num_iters
  # initialize number of trials
  prpt$num_trials <- num_trials
  # Initialize vectors for total earnings accrued on each simulation
  # 100 by 1 vector
  prpt$rand_earnings <- matrix(nrow = prpt$num_iters,
                               ncol = 1)
  # Percent correct for each iteration -- overall percent correct
  prpt$rand_correctness <- matrix(nrow = prpt$num_iters,
                                  ncol = 1)

  # Running average of the percent correct
  # list of ones and zeros for whether they were correct on the trial
  prpt$rand_avg_correctness <- matrix(nrow = prpt$num_iters,
                                      ncol = prpt$num_trials)
}

#' @keywords internal
#' @noRd
setup_reward_vec <- function() {
  # make a vector of 100 elements
  prpt$reward_vector <- matrix(nrow = 6, ncol = 100)

  # filling in the reward vector matrix which has the
  # probabilities of getting a reward.
  # for instance reward_vector[1,] is 75 0s and 25 ones, corresponding to
  # 25% rpobability of getting reward
  prpt$reward_vector[1,] <- c(rep(0,75),rep(1,25))
  prpt$reward_vector[2,] <- c(rep(0,50),rep(1,50))
  prpt$reward_vector[3,] <- c(rep(0,25),rep(1,75))
  prpt$reward_vector[4,] <- c(rep(0,75),rep(1,25))
  prpt$reward_vector[5,] <- c(rep(0,50),rep(1,50))
  prpt$reward_vector[6,] <- c(rep(0,25),rep(1,75))

}

#' @keywords internal
#' @noRd
setup_reward_struc <- function() {
  # what values of rewards are possible for each action
  # column 1 is phase 1 and 2 and column 2 is phase 3
  prpt$reward_structure <- matrix(nrow = 6, ncol = 2)

  #Reward structure is the magnitude of the rewards
  #column 1 is for phase 1 and 2 values
  #column 2 is for phase 3 values
  prpt$reward_structure[1,] <- c(1,2.5)
  prpt$reward_structure[2,] <- c(1,1.5)
  prpt$reward_structure[3,] <- c(1,0.5)
  prpt$reward_structure[4,] <- c(-1,-1.25)
  prpt$reward_structure[5,] <- c(-1,-0.75)
  prpt$reward_structure[6,] <- c(-1,-0.25)

}

#' @keywords internal
#' @noRd
setup_iter <- function(num_states) {

  prpt$num_states <- num_states #initialize number of states

  #reward occuring at each state
  #number of rows is the amount of states by number of trials
  prpt$reward_episode <- matrix(nrow = prpt$num_states, ncol = prpt$num_trials)

  # initialize choice vector which stores the two options (fractal)
  # images on each trial -- 1 to 6.
  prpt$choice <- matrix(nrow = prpt$num_trials, ncol = 2)

  # decision is number 1 - 6 corresponding to icon group.
  # this is the intitialization of the decision matrix
  prpt$decision <- matrix(nrow = 1, ncol = prpt$num_trials)

  #initialize percent corrrect
  prpt$percent_correct <- matrix(nrow = 1, ncol = prpt$num_trials)

  setup_reward_vec()

  setup_reward_struc()

  # start to define which choices will be on each trial
  # this only corresponds to phase 2
  # 50 trials, you pull 25 positive
  # and 25 negatives
  choice_vector <- c(rep(0,25), rep(1,25))

  # sample without replacement and say which order of trials
  # will you have positive or negative trials (icons) in phase 2
  prpt$choice_vector_shuffled <- sample(choice_vector, 50)

}

#' @keywords internal
#' @noRd
sample_icons <- function(i) {

    # Phase One choices
    if (i < 25 || i == 25) {
      # sample 2 icons corresponding to the group images to show on a trial
      prpt$choice[i,] <- sample(c(1,2,3),2)}

    # Phase Two choices
    if (i > 25 && i <=75) {
      # sample 2 icons corresponding to the group images to show on a trial
      # if it's a 1 it is a positive icon, if it's a 0 it's a negative icon
      if (prpt$choice_vector_shuffled[i-25] == 0)
        prpt$choice[i,] <- sample(c(1,2,3),2)
      if (prpt$choice_vector_shuffled[i-25] == 1)
        prpt$choice[i,] <- sample(c(4,5,6),2)}

    # Phase 3 choices
    if (i > 75) {
      # sample 2 icons corresponding to the group images to show on a trial
      # both positive and negative
      prpt$choice[i,] <- sample(c(1,2,3,4,5,6),2)}
}


#' @keywords internal
#' @noRd
make_decisions <- function(i) {
  # Create variable ('decision_vector') with 50/50 chance of choosing choice
  # 1 or choice 2 and shuffle it and draw the first element
  prpt$decision_vector <- c(rep(prpt$choice[i,1],50), rep(prpt$choice[i,2],50)) # For softmax these 50s should become the calculated softmax probabilities.
  prpt$decision_vector_shuffled <- sample(prpt$decision_vector,100)
  prpt$decision[1,i] <- prpt$decision_vector_shuffled[1] # save decision to the decision matrix

}

#' @keywords internal
#' @noRd
determine_rewards <- function(i) {

  # Determine whether reward is obtained or not. If the first element of
  # 'Reward_vector_shuffled'=1, then reward is obtained. The reward
  # magnitude depends on the trial # (different reward magnitudes by task
  # Phase).
  prpt$reward_vector_trial <- prpt$reward_vector[prpt$decision[1,i],] # select the row that has the definitions of whether or not a trial was rewarded
  prpt$reward_vector_shuffled <- sample(prpt$reward_vector_trial,100) # sample that without replacement. If first element is 1, get rewarded. Else no reward.

  #in phase 1 and 2 add reward if appropriate
  if (i < 76) {
    if (prpt$reward_vector_shuffled[1] == 1) {
      prpt$reward_episode[3,i] <- prpt$reward_structure[prpt$decision[1,i],1]}
    else {
      prpt$reward_episode[3,i] <- 0 }
  }

  # in phase 3 add reward if appropriate (second column phase 3 values)
  if (i > 75) {
    if (prpt$reward_vector_shuffled[1] == 1) {
      prpt$reward_episode[3,i] <- prpt$reward_structure[prpt$decision[1,i],2]}
    else {
      prpt$reward_episode[3,i] = 0 }
  }
}

#' @keywords internal
#' @noRd
determine_percent_correct <- function(i) {
  # Determine -- for Phases 1/2 -- whether the choice made by the agent was
  # the "Correct" choice, i.e. the agent chose the option with the highest
  # expected value.
  if (i < 76) {
    if (prpt$decision[1,i] < 4) {
      if (((prpt$choice[i,1] == 1) && (prpt$choice[i,2] == 2)) || ((prpt$choice[i,1] == 2) && (prpt$choice[i,2] == 1))) {
        if (prpt$decision[1,i] == 1) { prpt$percent_correct[1,i] <- 0 }
        else { prpt$percent_correct[1,i] <- 1 } }

      else if (((prpt$choice[i,1] == 1) && (prpt$choice[i,2] == 3)) || ((prpt$choice[i,1] == 3) && (prpt$choice[i,2] == 1))) {
        if (prpt$decision[1,i] == 1) { prpt$percent_correct[1,i] <- 0 }
        else { prpt$percent_correct[1,i] <- 1 } }

      else if (((prpt$choice[i,1] == 2) && (prpt$choice[i,2] == 3)) || ((prpt$choice[i,1] == 3) && (prpt$choice[i,2] == 2))) {
        if (prpt$decision[1,i] == 2) { prpt$percent_correct[1,i] <- 0 }
        else { prpt$percent_correct[1,i] <- 1 } } }

    else if (prpt$decision[1,i] > 3) {
      if (((prpt$choice[i,1] == 4) && (prpt$choice[i,2] == 5)) || ((prpt$choice[i,1] == 5) && (prpt$choice[i,2] == 4))) {
        if (prpt$decision[1,i] == 4) {
          prpt$percent_correct[1,i] <- 1 }
        else {
          prpt$percent_correct[1,i] <- 0 } }

      else if (((prpt$choice[i,1] == 4) && (prpt$choice[i,2] == 6)) || ((prpt$choice[i,1] == 6) && (prpt$choice[i,2] == 4))) {
        if (prpt$decision[1,i] == 4) {
          prpt$percent_correct[1,i] <- 1 }
        else {
          prpt$percent_correct[1,i] <- 0 } }

      else if (((prpt$choice[i,1] == 5) && (prpt$choice[i,2] == 6)) || ((prpt$choice[i,1] == 6) && (prpt$choice[i,2] == 5))) {
        if (prpt$decision[1,i] == 5) {
          prpt$percent_correct[1,i] <- 1 }
        else {
          prpt$percent_correct[1,i] <- 0 } } }
  }

  # Determine -- for Phases 3 -- whether the choice made by the agent was
  # the "Correct" choice, i.e. the agent chose the option with the highest
  # expected value.
  if (i > 75) {
    if (((prpt$choice[i,1] == 1) && (prpt$choice[i,2] == 2)) || ((prpt$choice[i,1] == 2) && (prpt$choice[i,2] == 1))) {
      if (prpt$decision[1,i] == 2) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 1) && (prpt$choice[i,2] == 3)) || ((prpt$choice[i,1] == 3) && (prpt$choice[i,2] == 1))) {
      if (prpt$decision[1,i] == 1) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 1) && (prpt$choice[i,2] == 4)) || ((prpt$choice[i,1] == 4) && (prpt$choice[i,2] == 1))) {
      if (prpt$decision[1,i] == 1) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 1) && (prpt$choice[i,2] == 5)) || ((prpt$choice[i,1] == 5) && (prpt$choice[i,2] == 1))) {
      if (prpt$decision[1,i] == 1) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 1) && (prpt$choice[i,2] == 6)) || ((prpt$choice[i,1] == 6) && (prpt$choice[i,2] == 1))) {
      if (prpt$decision[1,i] == 1) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 2) && (prpt$choice[i,2] == 3)) || ((prpt$choice[i,1] == 3) && (prpt$choice[i,2] == 2))) {
      if (prpt$decision[1,i] == 2) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 2) && (prpt$choice[i,2] == 4)) || ((prpt$choice[i,1] == 4) && (prpt$choice[i,2] == 2))) {
      if (prpt$decision[1,i] == 2) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 2) && (prpt$choice[i,2] == 5)) || ((prpt$choice[i,1] == 5) && (prpt$choice[i,2] == 2))) {
      if (prpt$decision[1,i] == 2) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 2) && (prpt$choice[i,2] == 6)) || ((prpt$choice[i,1] == 6) && (prpt$choice[i,2] == 2))) {
      if (prpt$decision[1,i] == 2) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 3) && (prpt$choice[i,2] == 4)) || ((prpt$choice[i,1] == 4) && (prpt$choice[i,2] == 3))) {
      if (prpt$decision[1,i] == 3) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 3) && (prpt$choice[i,2] == 5)) || ((prpt$choice[i,1] == 5) && (prpt$choice[i,2] == 3))) {
      if (prpt$decision[1,i] == 3) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 3) && (prpt$choice[i,2] == 6)) || ((prpt$choice[i,1] == 6) && (prpt$choice[i,2] == 3))) {
      if (prpt$decision[1,i] == 3) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 4) && (prpt$choice[i,2] == 5)) || ((prpt$choice[i,1] == 5) && (prpt$choice[i,2] == 4))) {
      if (prpt$decision[1,i] == 4) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 4) && (prpt$choice[i,2] == 6)) || ((prpt$choice[i,1] == 6) && (prpt$choice[i,2] == 4))) {
      if (prpt$decision[1,i] == 6) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

    else if (((prpt$choice[i,1] == 5) && (prpt$choice[i,2] == 6)) || ((prpt$choice[i,1] == 6) && (prpt$choice[i,2] == 5))) {
      if (prpt$decision[1,i] == 6) {
        prpt$percent_correct[1,i] = 1 }
      else {
        prpt$percent_correct[1,i] = 0 } }

  }
}

#' @keywords internal
#' @noRd
prpt_performance <- function(k) {
  # Calculate and store the total earnings and the overall percent correct for
  # each simulation
  prpt$rand_earnings[k] <- sum(prpt$reward_episode[3,])
  prpt$rand_correctness[k] <- (sum(prpt$percent_correct)/150)
  prpt$rand_avg_correctness[k,] <- prpt$percent_correct

}

