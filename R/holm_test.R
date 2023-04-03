#' holm_test
#'
#' Runs the Holm-Bonferoni method and extracts the first time of no statistical
#' significance
#'
#' @param x table resulting from merged_results
#' @param conf confidence level
#'
#' @return time value of no statistical significance
#'
#' @export
holm_test = function(x, conf) {
  alpha = 1 - conf
  n = nrow(x)
  adjusteddata = x %>% arrange(pval) %>% mutate(rank = row_number(), hb = alpha / (n - rank + 1)) %>% mutate(sig = "N")

  sigrun = 1
  rowcount = 1

  while (sigrun != 0) {
    adjusteddata[rowcount, "sig"] = ifelse(adjusteddata[rowcount, "pval"] < adjusteddata[rowcount, "hb"], "Y", "N")
    if (adjusteddata[rowcount, "sig"] == "N") {
      sigrun = 0
    }
    if (rowcount == n) {
      sigrun = 0
    }
    rowcount = rowcount + 1
  }

  insigcount = count(adjusteddata$sig == "N")

  if (insigcount > 0) {
    insigdata = adjusteddata %>% select(time, sig) %>% filter(sig == "N")
    timesig = min(insigdata$time)
  }
  else {
    timesig = NA
  }
  return (timesig)
}
