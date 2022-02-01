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
zeros <- function(dims) {
  array(0, dim = dims)
}

#' Use an RL Agent Template from rlsims Package
#'
#' @description This function is for accessing RL Agent templates that ship with
#'   the rlsims package and is exported for pedagogical purposes.
#'
#' @param name Name of the agent template to use.
#' @param read Logical: Should the RL Agent template be read? Default is `TRUE`.
#'
#' @return If `read = TRUE` (default), the results of [base::readLines] on the
#'   RL Agent template; otherwise the template file path.
#' @export
#'
#' @examples
#' \dontrun{
#' use_agent_template("td_conditioning")
#' }
use_agent_template <- function(name, read = TRUE) {
  file <- system.file(
    paste0("template_", name, ".R"),
    package = "rlsims"
  )

  if (read) {
    readLines(file, encoding = "UTF-8")
  } else {
    file
  }
}
