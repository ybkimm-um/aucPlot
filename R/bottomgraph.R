#' bottomgraph
#'
#' Creates plot which shows rolling difference in cumulative AUC values over time
#'
#' @param merged_table table from merged_results
#' @param conf confidence level
#' @param group1name string for name of group 1
#' @param group2name string for name of group 2
#' @param timeunit unit of time used as a string
#' @param timesig point of no significance from holm_test
#' @param rawdata raw long-format, longitudinal data
#'
#' @return graph of pain over time by group
#'
#' @export
bottomgraph = function(merged_table, conf, group1name, group2name, timeunit, timesig, rawdata) {

  num_a = NULL
  num_b = NULL
  pooledvar = NULL
  meandiff = NULL
  int = NULL
  lowint = NULL
  upint = NULL

  conf = conf + ((1 - conf) / 2) # adjusts calculation for 2 tailed
  graphdata = merged_table %>% mutate(int = ifelse(num_a == 0 & num_b == 0, 0, qt(conf, (num_a + num_b - 2)) * sqrt(pooledvar)),
                           lowint = meandiff - int, upint = meandiff + int)
  graphdata = rbind(c(0:0), graphdata)
  graphdata[1, "time"] = rawdata[1, "time"]

  yscale_upper = max(abs(graphdata$meandiff)) + max(graphdata$int)
  yscale_lower = -max(abs(graphdata$meandiff)) - max(graphdata$int)

  annotate1 = paste("Favors Group ", group1name, sep = "")
  annotate2 = paste("Favors Group ", group2name, sep ="")
  axisannotate = paste("Time (", timeunit, ")", sep = "")

  graph = ggplot(data = graphdata) + geom_ribbon(mapping = aes(x = time, ymin = lowint, ymax = upint), fill = "palegreen", alpha = 0.3) +
    geom_line(mapping = aes(x = time, y = meandiff), color = "green4") +
    scale_y_continuous(limits = c(yscale_lower, yscale_upper)) +
    scale_x_continuous(breaks = graphdata$time) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ylab("Difference in Cumulative Mean Pain") +
    xlab(axisannotate) +
    annotate("text", x = min(graphdata$time), y = -max(abs(graphdata$meandiff)), hjust = 0, label = annotate1, color = "firebrick3") +
    annotate("text", x = min(graphdata$time), y = max(abs(graphdata$meandiff)), hjust = 0, label = annotate2, color = "blue3") +
    theme_classic() + theme(panel.border = element_rect(colour = "black",fill= NA, size = 1)) +
    geom_vline(xintercept = ifelse(is.na(timesig), 0, timesig), linetype = "dashed", color = "black")

  return(graph)
}
