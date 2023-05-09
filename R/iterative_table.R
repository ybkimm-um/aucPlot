#' iterative_table
#'
#' Intakes individual AUC data over time and aggregates data into the average,
#' standard deviation, and sample size by time interval.
#'
#' @param iterative_data matrix of individual AUC values
#'
#' @return table with time, average, standard deviation,
#' and sample size by time interval
#'
#' @export
iterative_table <- function(iterative_data) {
  rowmeans <- rowMeans(iterative_data, na.rm = TRUE)
  rowsds <- rowSds(iterative_data, na.rm = TRUE)
  num <- rowSums(!is.na(iterative_data))
  time <- as.double(rownames(iterative_data))
  table <- cbind(time, rowmeans, rowsds, num)
  return (table)
}
