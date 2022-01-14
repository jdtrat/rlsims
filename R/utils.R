#' Create a zero matrix.
#'
#' @description Inspired by MatLab's zeros function to easily create a
#'   multi-dimensional array populated with zeros.
#' @param dims A numeric vector representing the dimensions of the array.
#'
#' @return A multi-dimensional array of zeros.
#' @export
#'
#' @examples
#'
#' # Create a 10-row by 2-column matrix of zeros
#' zeros(dims = c(10, 2))
#'
#' # Create a 3-d array with 10-rows and 2-columns
#' zeros(dims = c(3, 10, 2))
#'
zeros <- function(dims) {
  array(0,dim = dims)
}

# Utility function for checkign the agent's class
check_agent <- function(.agent) {

  .class <- class(.agent)

  if (!all(.class %in% c(available_agents_internal()$agent_class, "R6"))) {
    cli::cli_abort("{.arg agent} must be {.cls {available_agents_internal()$agent_class}}, not of class {.cls {.class}}")
  }

}



# Return all R6 objects beginning with "agent_"
reg_ls_agents <- function() {
  paste0(grep("^agent\\_", ls("package:rlsims"), value = TRUE))
}

# Get the name and class for the R6 objects
get_class <- function(x) {
  data.frame(
    agent_name = x,
    agent_class = get(x = x)$classname
  )
}

#' List Available RL Agents
#'
#' @return A data frame of available RL agent names and their class types. This assumes
#'   the convention that all RL agents are prefaced with "agent_", e.g.,
#'   "agent_tdrl_conditioning".
#'
available_agents_internal <- function() {
  do.call(
    "get_class",
    list(reg_ls_agents())
  )
}

#' List Available RL Agents
#'
#' @return A character vector of available RL agent names and their class types.
#' @export
#'
#' @examples
#'
#' if (interactive()) available_agents()
#'
available_agents <- function() {
  available_agents_internal()$agent_class
}


#' Use an RL Agent Template from rlsims Package
#'
#' @param name Name of the agent template to use.
#'
#' @return Results of [base::readLines] on the RL Agent template.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' use_agent_template("tdrl_conditioning")
#' }
use_agent_template <- function(name) {
  readLines(
    system.file(
      paste0("template_", name, ".R"),
      package = "rlsims"
    ),
    encoding = "UTF-8"
  )
}
