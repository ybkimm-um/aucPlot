#' iterative_table
#'
#' Intakes individual AUC data over time and calculates the average,
#' standard deviation, and sample size by time interval.
#'
#' @param iterative_data matrix of individual AUC values
#'
#' @return table with time, average, standard deviation, and sample size by time interval
#'
#' @export
iterative_table = function(iterative_data) {
  rowmeans = rowMeans(matrix1, na.rm = TRUE)
  rowsds = rowSds(matrix1, na.rm = TRUE)
  num = rowSums(!is.na(matrix1))
  time = as.double(rownames(matrix1))
  table = cbind(time, rowmeans, rowsds, num)
  return (table)
}
