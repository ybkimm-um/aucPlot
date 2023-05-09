#' iterative_auc
#'
#' Intakes long-format longitudinal data and calculates iterative AUC values
#' by individual patient
#'
#' @param data_long long format longitudinal data
#'
#' @return matrix of individual AUC per time interval
#'
#' @export
iterative_auc <- function(data_long) {
  n <- length(data_long$time)
  missing <- is.na(data_long$pain)
  data_long$pain[missing] <- 0
  # sets all missing values to 0 for purpose of cumulative sum calculations
  patientdata <- matrix(0, nrow = length(unique(data_long$time)) - 1,
                       ncol = length(unique(data_long$patient)))

  rowcount <- 1
  colcount <- 0

  for (t in 1:n-1) {
    value <- (0.5 * (data_long$pain[t] + data_long$pain[t+1])*
               (data_long$time[t+1] - data_long$time[t]))
    patientdata[t %% length(unique(data_long$time) + 1), colcount] <- value
    if (t %% length(unique(data_long$time) + 1) == 0) {
      rowcount <- 1
      colcount <- colcount + 1
    }
  }
  row.names(patientdata) <-  tail(unique(data_long$time), -1)
  colnames(patientdata) <- paste(unique(data_long$patient), "_AUC", sep = "")
    # REQUIRES THAT DATA BE ORDERED IN INCREASING PATIENTID
  data_long$pain[missing] <- NA
  return (patientdata)
}
