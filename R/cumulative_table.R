#' cumulative_table
#'
#' Intakes individual AUC data over time and calculates the cumulative average,
#' standard deviation, and sample size.
#' @param iterative_data matrix of individual, non-cumulative AUC data
#' @return table with time, cumulative average, standard deviaiton, and sample size
#' @export
cumulative_table = function(iterative_data) {
  n = nrow(x)
  c = ncol(x)
  for (p in 2:n) {
    for (q in 1:c) {
      x[p, q] = x[p, q] + x[p-1, q]
    }
  }
  rowmeans = rowMeans(x, na.rm = TRUE)
  rowsds = rowSds(x, na.rm = TRUE)
  num = rowSums(!is.na(x))
  time = as.double(row.names(x))

  table = cbind(time, rowmeans, rowsds, num)
  return (table)
}
