getVariableLabels <- function(binaryVersion = TRUE) {
  varLabels <- if (binaryVersion) {
    suppressMessages(readr::read_csv("data/yrbss_binary_questions.csv"))
  } else {
    suppressMessages(readr::read_csv("data/yrbss_questions.csv"))
  }
  l <- varLabels$label
  names(l) <- varLabels$variable
  return(as.list(l))
}
