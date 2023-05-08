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
holm_test <- function(x, conf) {

  adjusted_pval <- NULL
  sig <- NULL

  adjusteddata = x %>% mutate(adjusted_pval = p.adjust(x$pval,method = "holm"),
                              sig = ifelse(adjusted_pval <= 1-conf, "Y", "N"))

  insig_table = adjusteddata %>% filter(sig == "N")

  insig_time = min(insig_table$time)

  return (insig_time)
}
