#' Create a zero matrix.
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
