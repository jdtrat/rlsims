% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_prpt_iaps.R
\name{simulate_prpt_iaps}
\alias{simulate_prpt_iaps}
\title{Simulate Random Choices on the PRPT or IAPS task}
\usage{
simulate_prpt_iaps(
  num_iters = 1000,
  num_trials = 150,
  num_states = 4,
  task_type
)
}
\arguments{
\item{num_iters}{Number of iterations}

\item{num_trials}{Number of trials}

\item{num_states}{Number of states -- should equal 4}

\item{task_type}{Is simulation for IAPS or PRPT task? If former, task_type = "iaps"; latter and task_type = "prpt"}
}
\value{
A list of the random earnings and correctness.
}
\description{
Simulate Random Choices on the PRPT or IAPS task
}
