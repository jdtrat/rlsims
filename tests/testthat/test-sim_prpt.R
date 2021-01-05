#' Run the original PRPT simulation
#'
#' Developed by L. Paul Sands III in Matlab, translated to R. Modified slightly
#' for testing purposes by Jonathan D. Trattner to return random earnings and
#' correctness in the same form as the package version.
#'
#' @return A list of the random earnings and correctness.
#' @keywords internal
#'
original_PRPT <- function() {

  numiters = 1000 #number of iterations
  numtrials = 150 #number of trials

  #Initialize vectors for total earnings accrued on each simulation
  # 100 by 1 vector
  Random_earnings = matrix(nrow = numiters, ncol = 1)
  #Percent correct for each iteration -- overall percent correct
  Random_correctness = matrix(nrow = numiters, ncol = 1)
  #Running average of the percent correct
  #list of ones and zeros for whether they were correct on the trial
  Random_correctness_run = matrix(nrow = numiters, ncol = numtrials)


  for (k in 1:numiters) {

    nEpisode = 4 #initialize number of states
    #reward occuring at each state
    #number of rows is the amount of states by number of trials
    reward_episode = matrix(nrow = nEpisode, ncol = numtrials)
    # initialize choice vector which stores the two options (fractal)
    # images on each trial -- 1 to 6.
    choice = matrix(nrow = numtrials, ncol = 2)
    # decision is number 1 - 6 corresponding to icon group.
    # this is the intitialization of the decision matrix
    decision = matrix(nrow = 1, ncol = numtrials)
    #initialize percent corrrect
    percent_correct = matrix(nrow = 1, ncol = numtrials)

    # make a vector of 100 elements
    Reward_vector = matrix(nrow = 6, ncol = 100)
    # what values of rewards are possible for each action
    # column 1 is phase 1 and 2 and column 2 is phase 3
    Reward_structure = matrix(nrow = 6, ncol = 2)
    # filling in the reward vector matrix which has the
    # probabilities of getting a reward.
    # for instance reward_vector[1,] is 75 0s and 25 ones, corresponding to
    # 25% rpobability of getting reward
    Reward_vector[1,] <- c(rep(0,75),rep(1,25))
    Reward_vector[2,] <- c(rep(0,50),rep(1,50))
    Reward_vector[3,] <- c(rep(0,25),rep(1,75))
    Reward_vector[4,] <- c(rep(0,75),rep(1,25))
    Reward_vector[5,] <- c(rep(0,50),rep(1,50))
    Reward_vector[6,] <- c(rep(0,25),rep(1,75))
    #Reward structure is the magnitude of the rewards
    #column 1 is for phase 1 and 2 values
    #column 2 is for phase 3 values
    Reward_structure[1,] <- c(1,2.5)
    Reward_structure[2,] <- c(1,1.5)
    Reward_structure[3,] <- c(1,0.5)
    Reward_structure[4,] <- c(-1,-1.25)
    Reward_structure[5,] <- c(-1,-0.75)
    Reward_structure[6,] <- c(-1,-0.25)

    # start to define which choices will be on each trial
    # this only corresponds to phase 2
    # 50 trials, you pull 25 positive
    # and 25 negatives
    #
    Choice_vector = c(rep(0,25), rep(1,25))
    # sample without replacement and say which order of trials
    # will you have positive or negative trials (icons) in phase 2
    Choice_vector_shuffled = sample(Choice_vector,50)

    #for each trial,
    for (i in 1:numtrials) {

      # Phase One choices
      if (i < 25 || i == 25) {
        # sample 2 icons corresponding to the group images to show on a trial
        choice[i,] <- sample(c(1,2,3),2)}

      # Phase Two choices
      if (i > 25 && i <=75) {
        # sample 2 icons corresponding to the group images to show on a trial
        # if it's a 1 it is a positive icon, if it's a 0 it's a negative icon
        if (Choice_vector_shuffled[i-25] == 0)
          choice[i,] <- sample(c(1,2,3),2)
        if (Choice_vector_shuffled[i-25] == 1)
          choice[i,] <- sample(c(4,5,6),2)}

      # Phase 3 choices
      if (i > 75) {
        # sample 2 icons corresponding to the group images to show on a trial
        # both positive and negative
        choice[i,] <- sample(c(1,2,3,4,5,6),2)}
    }


    for (i in 1:numtrials) {

      # Create variable ('Decision_vector') with 50/50 chance of choosing choice
      # 1 or choice 2 and shuffle it and draw the first element
      Decision_vector <- c(rep(choice[i,1],50), rep(choice[i,2],50)) # For softmax these 50s should become the calculated softmax probabilities.
      Decision_vector_shuffled <- sample(Decision_vector,100)
      decision[1,i] <- Decision_vector_shuffled[1] # save decision to the decision matrix


      # Determine whether reward is obtained or not. If the first element of
      # 'Reward_vector_shuffled'=1, then reward is obtained. The reward
      # magnitude depends on the trial # (different reward magnitudes by task
      # Phase).
      Reward_vector_trial <- Reward_vector[decision[1,i],] # select the row that has the definitions of whether or not a trial was rewarded
      Reward_vector_shuffled <- sample(Reward_vector_trial,100) # sample that without replacement. If first element is 1, get rewarded. Else no reward.

      #in phase 1 and 2 add reward if appropriate
      if (i < 76) {
        if (Reward_vector_shuffled[1] == 1) {
          reward_episode[3,i] <- Reward_structure[decision[1,i],1]}
        else {
          reward_episode[3,i] <- 0 }
      }

      # in phase 3 add reward if appropriate (second column phase 3 values)
      if (i > 75) {
        if (Reward_vector_shuffled[1] == 1) {
          reward_episode[3,i] <- Reward_structure[decision[1,i],2]}
        else {
          reward_episode[3,i] = 0 }
      }

      # Determine -- for Phases 1/2 -- whether the choice made by the agent was
      # the "Correct" choice, i.e. the agent chose the option with the highest
      # expected value.
      if (i < 76) {
        if (decision[1,i] < 4) {
          if (((choice[i,1] == 1) && (choice[i,2] == 2)) || ((choice[i,1] == 2) && (choice[i,2] == 1))) {
            if (decision[1,i] == 1) { percent_correct[1,i] <- 0 }
            else { percent_correct[1,i] <- 1 } }

          else if (((choice[i,1] == 1) && (choice[i,2] == 3)) || ((choice[i,1] == 3) && (choice[i,2] == 1))) {
            if (decision[1,i] == 1) { percent_correct[1,i] <- 0 }
            else { percent_correct[1,i] <- 1 } }

          else if (((choice[i,1] == 2) && (choice[i,2] == 3)) || ((choice[i,1] == 3) && (choice[i,2] == 2))) {
            if (decision[1,i] == 2) { percent_correct[1,i] <- 0 }
            else { percent_correct[1,i] <- 1 } } }

        else if (decision[1,i] > 3) {
          if (((choice[i,1] == 4) && (choice[i,2] == 5)) || ((choice[i,1] == 5) && (choice[i,2] == 4))) {
            if (decision[1,i] == 4) {
              percent_correct[1,i] <- 1 }
            else {
              percent_correct[1,i] <- 0 } }

          else if (((choice[i,1] == 4) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 4))) {
            if (decision[1,i] == 4) {
              percent_correct[1,i] <- 1 }
            else {
              percent_correct[1,i] <- 0 } }

          else if (((choice[i,1] == 5) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 5))) {
            if (decision[1,i] == 5) {
              percent_correct[1,i] <- 1 }
            else {
              percent_correct[1,i] <- 0 } } }
      }

      # Determine -- for Phases 3 -- whether the choice made by the agent was
      # the "Correct" choice, i.e. the agent chose the option with the highest
      # expected value.
      if (i > 75) {
        if (((choice[i,1] == 1) && (choice[i,2] == 2)) || ((choice[i,1] == 2) && (choice[i,2] == 1))) {
          if (decision[1,i] == 2) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 1) && (choice[i,2] == 3)) || ((choice[i,1] == 3) && (choice[i,2] == 1))) {
          if (decision[1,i] == 1) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 1) && (choice[i,2] == 4)) || ((choice[i,1] == 4) && (choice[i,2] == 1))) {
          if (decision[1,i] == 1) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 1) && (choice[i,2] == 5)) || ((choice[i,1] == 5) && (choice[i,2] == 1))) {
          if (decision[1,i] == 1) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 1) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 1))) {
          if (decision[1,i] == 1) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 2) && (choice[i,2] == 3)) || ((choice[i,1] == 3) && (choice[i,2] == 2))) {
          if (decision[1,i] == 2) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 2) && (choice[i,2] == 4)) || ((choice[i,1] == 4) && (choice[i,2] == 2))) {
          if (decision[1,i] == 2) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 2) && (choice[i,2] == 5)) || ((choice[i,1] == 5) && (choice[i,2] == 2))) {
          if (decision[1,i] == 2) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 2) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 2))) {
          if (decision[1,i] == 2) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 3) && (choice[i,2] == 4)) || ((choice[i,1] == 4) && (choice[i,2] == 3))) {
          if (decision[1,i] == 3) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 3) && (choice[i,2] == 5)) || ((choice[i,1] == 5) && (choice[i,2] == 3))) {
          if (decision[1,i] == 3) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 3) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 3))) {
          if (decision[1,i] == 3) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 4) && (choice[i,2] == 5)) || ((choice[i,1] == 5) && (choice[i,2] == 4))) {
          if (decision[1,i] == 4) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 4) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 4))) {
          if (decision[1,i] == 6) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

        else if (((choice[i,1] == 5) && (choice[i,2] == 6)) || ((choice[i,1] == 6) && (choice[i,2] == 5))) {
          if (decision[1,i] == 6) {
            percent_correct[1,i] = 1 }
          else {
            percent_correct[1,i] = 0 } }

      }
    }


    # Calculate and store the total earnings and the overall percent correct for
    # each simulation
    Random_earnings[k] <- sum(reward_episode[3,])
    Random_correctness[k] <- (sum(percent_correct)/150)
    Random_correctness_run[k,] <- percent_correct

  }
  #
  # hist(Random_earnings)
  # hist(Random_correctness)

  return(
    list(random_earnings = Random_earnings,
         random_correctness = Random_correctness)
  )

}


test_that("prpt simulation matches original", {

  # 3rd edition of testthat to use waldo for comparison
  local_edition(3)

  set.seed(18)
  matlab_based <- original_PRPT()

  set.seed(18)
  r_package <- simulatePRPT()

  expect_equal(matlab_based, r_package)

})
