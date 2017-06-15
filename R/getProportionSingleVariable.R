#' Get a Single Variable "Yes" Proportion
#'
#' Retrieves the proportion and confidence interval for a single variable in the
#' binary version, for given location and year as well as additional filters
#'
#' @param variable a string representing the name of the desired binary variable (if not specified "label" must be specified)
#' @param location a string representing the desired location, defaults to "US"
#' @param .year a int of the desired year (currently: 1991 to 2015)
#' @param label optionally instead of a "variable" argument one could specify the variable label
#' @param filters a vector of strings representing desired filters (see details)
#' @param .level the desired confidence interval level, defaults to 0.95
#'
#' @return a list containing:
#' \item{prop}{the proportion of "Yes" for the desired variables within given filters}
#' \item{ciLB}{Confidence Interval lower bound}
#' \item{ciUB}{Confidence Interval upper bound}
#'
#' @examples
#' getProportionSingleVariable("qn8", "US", 2015)
#'
#' @export
getProportionSingleVariable <- function(variable = NULL, location = "US", .year = NULL,
                                        label = NULL, filters = NULL, .level = 0.95) {
  if(is.null(.year)) {
    warning("You have not selected a year, you're getting proportion for the year 2015.")
    .year <- 2015
  }

  if (is.null(variable)) {
    if (is.null(label)) {
      stop("Error: you did not specify neither a variable name nor label")
    } else {
      variable <- getVarNameFromLabel(label)
    }
  }

  filtersExp <- paste(c(filters, paste("year ==", .year)), collapse = " & ")

  formula <- as.formula(paste0("~I(", variable," == 1)"))

  res <- survey::svyciprop(formula,
                           subset(yrbss_data_binary, eval(parse(text = filtersExp))),
                           level = .level,
                           na.rm = TRUE)
  prop <- unname(res[1])
  ci <- unname(attr(res, "ci"))
  return(list(prop = prop, ciLB = ci[1], ciUB = ci[2]))
}
