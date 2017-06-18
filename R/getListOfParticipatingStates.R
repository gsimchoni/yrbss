#' YRBSS Data - Binary Version - List of Participating States
#'
#' Helper method to extract the list of participating US states in the
#' \code{\link{yrbss_states_data_binary}} data.
#'
#' @return vector of participating US states abbreviations
#'
#' @examples
#' getListOfParticipatingStates()
#'
#' @export
getListOfParticipatingStates <- function() {
  sort(levels(yrbss_states_data_binary$variables$sitecode))
}