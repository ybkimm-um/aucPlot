#' iterative_auc
#'
#' Imtakes raw long-format longitudinal data and calculates iterative AUC values
#'
#' @param data_long long format longitudinal data
#'
#' @return matrix of individual AUC per time interval
#'
#' @export
iterative_auc = function(data_long) {
  n = length(x$time)
  missing = is.na(x$pain)
  x$pain[missing] = 0 # sets all missing values to 0 for purpose of cumulative sum calculations
  patientdata = matrix(0, nrow = length(unique(x$time)) - 1, ncol = length(unique(x$patient)))

  rowcount = 1
  colcount = 0

  for (t in 1:n-1) {
    value = 0.5 * (x$pain[t] + x$pain[t+1]) * (x$time[t+1] - x$time[t])
    patientdata[t %% length(unique(x$time) + 1), colcount] = value
    if (t %% length(unique(x$time) + 1) == 0) {
      rowcount = 1
      colcount = colcount + 1
    }
  }
  row.names(patientdata) =  tail(unique(x$time), -1)
  colnames(patientdata) = paste(unique(x$patient), "_AUC", sep = "") # REQUIRES THAT DATA BE ORDERED IN INCREASING PATIENTID
  x$pain[missing] = NA
  return (patientdata)
}
