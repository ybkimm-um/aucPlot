#' cumulative_table
#'
#' Intakes individual AUC data over time and calculates the cumulative average,
#' standard deviation, and sample size.
#'
#' @param iterative_data matrix of individual, non-cumulative AUC data
#'
#' @return table with time, cumulative average, standard deviaiton, and sample size
#'
#' @export
cumulative_table = function(iterative_data) {
  n = nrow(iterative_data)
  c = ncol(iterative_data)
  for (p in 2:n) {
    for (q in 1:c) {
      iterative_data[p, q] = iterative_data[p, q] + iterative_data[p-1, q]
    }
  }
  rowmeans = rowMeans(iterative_data, na.rm = TRUE)
  rowsds = rowSds(iterative_data, na.rm = TRUE)
  num = rowSums(!is.na(iterative_data))
  time = as.double(row.names(iterative_data))

  table = cbind(time, rowmeans, rowsds, num)
  return (table)
}
