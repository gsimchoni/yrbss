#' Check the validity of a year
#'
#' Helper method to check the validity of a year
#'
#' @param year a four-digit number representing the desired year
#'
#' @return an ERROR if the year is not in the yrbss_data_binary dataset,
#' the year itself if it is valid, and 2015 if it is NULL.
#'
#' @examples
#' checkLocationValidity("CA")
#'
#' @export
checkYearValidity <- function(year) {
  if (is.null(year)) {
    warning("You have not selected a year, you're getting results
            for the year 2015.")
    return(2015)
  } else if (!year %in% seq(1991, 2015, 2)) {
    stop("Invalid year.")
  }
  return(year)
}