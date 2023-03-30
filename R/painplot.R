#' painplot
#'
#' Takes in raw inputs and outputs 2 visuals
#'
#' @param raw_data raw long-format, longitudinal data
#' @param conf confidence level
#' @param group1name string for name of group 1
#' @param group2name string for name of group 2
#' @param timeunit unit of time used as a string
#' @param completecase option to exclude subjects without complete data
#'
#' @return 2 visuals
#'
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line scale_color_manual scale_fill_manual
#' scale_x_continuous scale_y_continuous geom_hline ylab xlab theme_classic theme element_rect
#' geom_vline element_blank annotate alpha
#'
#' @importFrom egg ggarrange
#'
#' @importFrom dplyr group_by summarize mutate filter select %>% arrange count row_number
#'
#' @importFrom stats ave complete.cases pt qnorm qt sd time
#'
#' @importFrom matrixStats rowSds
#'
#' @importFrom utils globalVariables tail
#'
#' @export
painplot = function(raw_data, conf = 0.95, group1name, group2name, timeunit = "hours",
                    completecase = FALSE) {

  iterative_auc = function(x) {
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
  cumulative_table = function(x) {
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
  merged_results = function(x, y) {
    xy = merge(x, y, by = "time", suffixes = c("_a", "_b"))
    dfxy = as.data.frame(xy)

    results = dfxy %>% mutate(meandiff = rowmeans_a - rowmeans_b,
                              pooledvar = ( ( (((num_a-1)*(rowsds_a^2)) +   ((num_b-1)*(rowsds_b^2))) / (num_a+num_b-2) ) * ((1/num_b) + (1/num_b)) ),
                              tstat = abs(meandiff) /sqrt(pooledvar), pval = 2 * pt(-abs(tstat), (num_a+num_b-2)) )
    return (results)
  }
  iterative_table = function(x) {
    rowmeans = rowMeans(x, na.rm = TRUE)
    rowsds = rowSds(x, na.rm = TRUE)
    num = rowSums(!is.na(x))
    time = as.double(rownames(x))
    table = cbind(time, rowmeans, rowsds, num)

    return (table)
  }
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
  topgraph = function(x, conf) {
    conf = conf + ((1 - conf) / 2) # adjusts calculation to be 2 tailed
    data = x %>% group_by(group, time) %>% summarize(avgpain = mean(pain, na.rm = TRUE),
                                                     sds = sd(pain, na.rm = TRUE), n = sum(!is.na(pain)), intlength = qnorm(conf) * (sds/sqrt(n)), # make 1.96 an input based on desired conf int
                                                     lowint = avgpain - intlength, upint = avgpain + intlength)

    graph = ggplot(data = data) + geom_ribbon(mapping = aes(x = time, ymin = lowint, ymax = upint, fill = group)) +
      geom_line(mapping = aes(x = time, y = avgpain, color = group)) +
      scale_color_manual(values=alpha(c("firebrick3", "blue3")))+
      scale_fill_manual(values=alpha(c("lightcoral", "lightskyblue"), 0.5))+
      scale_x_continuous(breaks = x$time) +
      ylab("Difference in Mean Pain Score") +
      theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.title.x = element_blank())
    return (graph)
  }
  bottomgraph = function(x, conf, group1name, group2name, timeunit, timesig, rawdata) {
    conf = conf + ((1 - conf) / 2) # adjusts calculation for 2 tailed
    graphdata = x %>% mutate(int = ifelse(num_a == 0 & num_b == 0, 0, qt(conf, (num_a + num_b - 2)) * sqrt(pooledvar)),
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

  #standardize column names to lower case
  names(x) = tolower(names(x))

  # complete cases option
  if (completecase == FALSE) {
  }
  if (completecase == TRUE) {
    x = x[ave(complete.cases(x), x$patient, FUN = all), ]
  }
  # filters data and creates individual datasets per group
  group1 = x %>% filter(group == group1name)
  group2 = x %>% filter(group == group2name)

  # creates individual tables for each group of cumulative averages
  group1step2 = cumulative_table(iterative_auc(group1))
  group2step2 = cumulative_table(iterative_auc(group2))

  # creates complete table of averages + tstat calculations for combined data of groups
  completetable = merged_results(group1step2, group2step2)

  #creates table of iterative changes in cumulative average for groups
  iterativechanges1 = iterative_table(iterative_auc(group1))
  iterativechanges2 = iterative_table(iterative_auc(group2))

  # holm-bonferroni adjusted p-value table
  adjustedtable = merged_results(iterativechanges1, iterativechanges2)

  # extract first point of no significance
  timesig = holm_test(adjustedtable, conf)

  #top plot
  topplot = topgraph(x, conf)

  #creating bottom plot
  bottomplot = bottomgraph(completetable, conf, group1name, group2name, timeunit, timesig, x)
  # create combined plot

  finalplot = ggarrange(topplot, bottomplot)

  return (finalplot)
}
