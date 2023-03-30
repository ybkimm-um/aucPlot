#' merged_results
#'
#' Aggregates data from 2 individual tables and calculates the mean difference,
#' pooled variance, t-test statistic, and p-value
#'
#' @param group1 aggregated data from group1
#' @param group2 aggregated data from group2
#'
#' @return merged table with extra metrics for both groups
#'
#' @export
merged_results = function(group1, group2) {
  xy = merge(group1, group2, by = "time", suffixes = c("_a", "_b"))
  dfxy = as.data.frame(xy)

  results = dfxy %>% mutate(meandiff = rowmeans_a - rowmeans_b,
                            pooledvar = ( ( (((num_a-1)*(rowsds_a^2)) +   ((num_b-1)*(rowsds_b^2))) / (num_a+num_b-2) ) * ((1/num_b) + (1/num_b)) ),
                            tstat = abs(meandiff) /sqrt(pooledvar), pval = 2 * pt(-abs(tstat), (num_a+num_b-2)) )
  return (results)
}
