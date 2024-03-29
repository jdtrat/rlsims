% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-array_factory.R
\name{rl_define_reinforcements_array}
\alias{rl_define_reinforcements_array}
\title{Define Reinforcements Array for Tracking Cues in RL Agent's Environment}
\usage{
rl_define_reinforcements_array(reinforcement_input, num_episodes, num_trials)
}
\arguments{
\item{reinforcement_input}{A list of reinforcements where each element
contains a data frame with columns 'onset', 'offset', 'magnitude', and
'trial' describing, respectively, the episode number a reward presentation
begins; the episode number the reward presentation ends; the magnitude of
the reward; the trials the rewards occur on.}

\item{num_episodes}{(numeric) The number of episodes per trial.}

\item{num_trials}{(numeric) The number of trials to simulate.}
}
\value{
A three-dimensional array tacking what reinforcements are delivered
in each episode across all simulated trials.
}
\description{
Define Reinforcements Array for Tracking Cues in RL Agent's Environment
}
\examples{

rl_define_reinforcements_array(
  list(
    one = data.frame(
      onset = 8,
      offset = 8,
      magnitude = 1,
      trial = 1:100
    )
  ),
  num_episodes = 10,
  num_trials = 100
)
}
