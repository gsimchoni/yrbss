#' Check the validity of a variable name
#'
#' Helper method to check the validity of a variable name.
#'
#' @param variable a string representing the name of the desired binary variable
#'
#' @return an ERROR if the variable is NULL or not in the yrbss_questions_binary
#' list of questions
#'
#' @examples
#' checkVariableValidity("qn8")
#'
#' @export
checkVariableValidity <- function(variable) {
  if (is.null(variable)) {
    stop("A variable name must be specified")
  }
  if (!variable %in% yrbss_questions_binary$variable) {
    stop("Invalid variable name")
  }
}