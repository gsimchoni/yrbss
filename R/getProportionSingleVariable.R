#' Get a Single Variable "Yes" Proportion
#'
#' Retrieves the proportion and confidence interval for a single variable in the
#' YRBSS binary version, for a given location and a given year, as well as
#' additional filters
#'
#' @param .variable a string representing the name of the desired binary variable
#' @param .location a string representing the desired location, defaults to "US"
#' @param .year a int of the desired year (currently: 1991 to 2015)
#' @param .filters a vector of strings representing desired filters (see details)
#' @param .level the desired confidence interval level, defaults to 0.95
#'
#' @return a list containing:
#' \item{prop}{the proportion of "Yes" for the desired variables within given filters}
#' \item{ciLB}{Confidence Interval lower bound}
#' \item{ciUB}{Confidence Interval upper bound}
#' \item{n}{Sample size}
#'
#' @details 
#' This function is a wrapper of the \code{survey} package \code{svyciprop}
#' function to adequately return a proportion for a single binary variable with
#' confidence interval, from the YRBSS survey which contains complex weighted
#' sampling. You should NOT report proportions based on the raw data in
#' \code{\link{yrbss_data_binary}} or \code{\link{yrbss_states_data_binary}}.
#' 
#' @seealso 
#' \code{\link{yrbss_data_binary}}, \code{\link{yrbss_states_data_binary}},
#' \code{\link{getListOfParticipatingStates}}, \code{\link{yrbss_questions_binary}}
#' 
#' @note 
#' This function does basic input validity check-ups, e.g. if the \code{.year}
#' parameter entered makes sense or if the \code{.location} is valid. It does NOT
#' however checks if for a given \code{.variable}, for a given \code{.year} and
#' possibly more filters there is enough (or at all) data to report a proportion.
#' If that isn't the case you should get a list of \code{NA}s + a Warning.
#' 
#' @examples
#' getProportionSingleVariable("qn8", "US", 2015)
#'
#' @export
getProportionSingleVariable <- function(.variable = NULL, .location = NULL,
                                        .year = NULL, .filters = NULL,
                                        .level = 0.95) {
  suppressMessages(require("survey"))
  
  checkVariableValidity(.variable)
  
  .location <- checkLocationValidity(.location)
  
  .year <- checkYearValidity(.year)

  formula <- as.formula(paste0("~I(", .variable," == 1)"))

  filtersExp <- paste(c(.filters, paste("year ==", .year)), collapse = " & ")
  
  data <- yrbss_data_binary
  
  if (.location != "US") {
    data <- yrbss_states_data_binary
    filtersExp <- paste(c(filtersExp, paste0("sitecode == \'", .location, "\'")),
                        collapse = " & ")
  } 

  data <- subset(data, eval(parse(text = filtersExp)))
  
  n <- sum(!is.na(data$variables[, .variable]))
  
  if (n >= 100) {
    res <- survey::svyciprop(formula, data, level = .level, na.rm = TRUE)
    prop <- unname(res[1])
    ci <- unname(attr(res, "ci"))
  } else {
    warning("Sample size < 100, outputting NA")
    prop <- NA
    ci <- NA
  }
  
  return(list(prop = prop, ciLB = ci[1], ciUB = ci[2], n = n))
}
