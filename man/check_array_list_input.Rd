% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-array_factory.R
\name{check_array_list_input}
\alias{check_array_list_input}
\title{Have Input Checks for Setting Array Inputs}
\usage{
check_array_list_input(.input, type, num_episodes, num_trials, num_stimuli)
}
\arguments{
\item{.input}{The input passed into one of the array definition functions
created with \code{\link{rl_define_array_factory}}.}

\item{type}{Either "stimulus" or "reinforcement" to tailor error messages
according to the array setting input.}

\item{num_episodes}{(numeric) The number of episodes per trial.}

\item{num_trials}{(numeric) The number of trials to simulate.}
}
\value{
An error if the input is invalid, otherwise nothing.
}
\description{
Have Input Checks for Setting Array Inputs
}
\keyword{internal}
