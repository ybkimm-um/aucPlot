#' topgraph
#'
#' Creates plot which visualizes pain scores longitudinally over time by group
#'
#' @param data_long raw, long-format longitudinal data
#' @param conf confidence level
#'
#' @return graph of pain over time by group
#'
#' @export
topgraph <- function(data_long, conf) {

  upint <- NULL
  lowint <- NULL
  intlength <- NULL
  group <- NULL
  pain <- NULL
  sds <- NULL
  n <- NULL
  avgpain <- NULL

  conf <- conf + ((1 - conf) / 2) # adjusts calculation to be 2 tailed
  data <- data_long %>% group_by(group, time) %>%
    summarize(avgpain = mean(pain, na.rm = TRUE), sds = sd(pain, na.rm = TRUE),
              n = sum(!is.na(pain)), intlength = qt(conf,n-1) * (sds/sqrt(n)),
              lowint = avgpain - intlength, upint = avgpain + intlength)

  graph <- ggplot(data = data) +
    geom_ribbon(mapping <- aes(x = time,
                               ymin = lowint, ymax = upint, fill = group)) +
    geom_line(mapping = aes(x = time, y = avgpain, color = group)) +
    scale_color_manual(values=alpha(c("firebrick3", "blue3")))+
    scale_fill_manual(values=alpha(c("lightcoral", "lightskyblue"), 0.5))+
    scale_x_continuous(breaks = data_long$time) +
    ylab("Difference in Mean Pain Score") +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.title.x = element_blank())
  return (graph)
}
