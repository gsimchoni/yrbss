#' Check the validity of a location
#'
#' Helper method to check the validity of a location.
#'
#' @param location a string representing the desired location
#'
#' @return an ERROR if the location is not in the yrbss_data_binary dataset,
#' the location itself if it is valid, and "US" if it is NULL.
#'
#' @examples
#' checkLocationValidity("CA")
#'
#' @export
checkLocationValidity <- function(location) {
  if (is.null(location)) {
    warning("You have not selected a location, you're getting results
            for the entire US")
    return("US")
  } else if (!location %in%
             c(levels(yrbss_states_data_binary$variables$sitecode), "US")) {
    stop("Invalid location or State not participating")
  }
  return(location)
}