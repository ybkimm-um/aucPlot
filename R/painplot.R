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
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line scale_color_manual
#' scale_fill_manual scale_x_continuous scale_y_continuous geom_hline ylab xlab
#' theme_classic theme element_rect geom_vline element_blank annotate alpha
#'
#' @importFrom egg ggarrange
#'
#' @importFrom dplyr group_by summarize mutate filter select %>%
#' arrange count row_number
#'
#' @importFrom stats ave complete.cases pt qnorm qt sd time
#'
#' @importFrom matrixStats rowSds
#'
#' @importFrom utils globalVariables tail
#'
#' @export
painplot <- function(raw_data, conf = 0.95, group1name, group2name,
                    timeunit = "hours", completecase = FALSE) {

  group <- NULL

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
  cumulative_table <- function(iterative_data) {
    n <- nrow(iterative_data)
    c <- ncol(iterative_data)
    for (p in 2:n) {
      for (q in 1:c) {
        iterative_data[p, q] <- iterative_data[p, q] + iterative_data[p-1, q]
      }
    }
    rowmeans <- rowMeans(iterative_data, na.rm = TRUE)
    rowsds <- rowSds(iterative_data, na.rm = TRUE)
    num <- rowSums(!is.na(iterative_data))
    time <- as.double(row.names(iterative_data))

    table <- cbind(time, rowmeans, rowsds, num)
    return (table)
  }
  merged_results <- function(group1, group2) {

    rowmeans_a <- NULL
    rowmeans_b <- NULL
    num_a <- NULL
    rowsds_a <- NULL
    num_b <- NULL
    rowsds_b <- NULL
    meandiff <- NULL
    pooledvar <- NULL
    tstat <- NULL

    xy <- merge(group1, group2, by = "time", suffixes = c("_a", "_b"))
    dfxy <- as.data.frame(xy)

    results <- dfxy %>%
      mutate(meandiff = rowmeans_a - rowmeans_b,
             pooledvar = (((((num_a-1)*(rowsds_a^2))+((num_b-1)*(rowsds_b^2)))
                           / (num_a+num_b-2) ) * ((1/num_b) + (1/num_b)) ),
             tstat = abs(meandiff) /sqrt(pooledvar),
             pval = 2 * pt(-abs(tstat), (num_a+num_b-2)) )
    return (results)
  }
  iterative_table <- function(iterative_data) {
    rowmeans <- rowMeans(iterative_data, na.rm = TRUE)
    rowsds <- rowSds(iterative_data, na.rm = TRUE)
    num <- rowSums(!is.na(iterative_data))
    time <- as.double(rownames(iterative_data))
    table <- cbind(time, rowmeans, rowsds, num)
    return (table)
  }
  holm_test <- function(x, conf) {

    pval <- NULL
    sig <- NULL

    alpha <- 1 - conf
    n <- nrow(x)
    adjusteddata <- x %>% arrange(pval) %>%
      mutate(rank = row_number(), hb = alpha / (n - rank + 1)) %>%
      mutate(sig = "N")

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
      rowcount <- rowcount + 1
    }

    insigcount = sum(adjusteddata$sig == "N")

    if (insigcount > 0) {
      insigdata <- adjusteddata %>% select(time, sig) %>% filter(sig == "N")
      timesig <- min(insigdata$time)
    }
    else {
      timesig <- NA
    }
    return (timesig)
  }
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
      summarize(avgpain = mean(pain, na.rm = TRUE),
                sds = sd(pain, na.rm = TRUE), n = sum(!is.na(pain)),
                intlength = qnorm(conf) * (sds/sqrt(n)),
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
  bottomgraph <- function(merged_table, conf, group1name, group2name, timeunit,
                          timesig, data_long) {

    num_a <- NULL
    num_b <- NULL
    pooledvar <- NULL
    meandiff <- NULL
    int <- NULL
    lowint <- NULL
    upint <- NULL

    conf <- conf + ((1 - conf) / 2) # adjusts calculation for 2 tailed
    graphdata <- merged_table %>%
      mutate(int <- ifelse(num_a == 0 & num_b == 0, 0,
                           qt(conf, (num_a + num_b - 2)) * sqrt(pooledvar)),
             lowint <- meandiff - int, upint <- meandiff + int)
    graphdata <- rbind(c(0:0), graphdata)
    graphdata[1, "time"] <- data_long[1, "time"]

    yscale_upper <- max(abs(graphdata$meandiff)) + max(graphdata$int)
    yscale_lower <- -max(abs(graphdata$meandiff)) - max(graphdata$int)

    annotate1 <- paste("Favors Group ", group1name, sep <- "")
    annotate2 <- paste("Favors Group ", group2name, sep <-"")
    axisannotate <- paste("Time (", timeunit, ")", sep <- "")

    graph <- ggplot(data = graphdata) +
      geom_ribbon(mapping = aes(x = time, ymin = lowint, ymax = upint),
                  fill = "palegreen", alpha = 0.3) +
      geom_line(mapping = aes(x = time, y = meandiff), color = "green4") +
      scale_y_continuous(limits = c(yscale_lower, yscale_upper)) +
      scale_x_continuous(breaks = graphdata$time) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      ylab("Difference in Cumulative Mean Pain") +
      xlab(axisannotate) +
      annotate("text", x = min(graphdata$time),
               y = -max(abs(graphdata$meandiff)), hjust = 0,
               label = annotate1, color = "firebrick3") +
      annotate("text", x = min(graphdata$time),
               y = max(abs(graphdata$meandiff)), hjust = 0,
               label = annotate2, color = "blue3") +
      theme_classic() +
      theme(panel.border = element_rect(colour = "black",fill= NA, size = 1)) +
      geom_vline(xintercept = ifelse(is.na(timesig), 0, timesig),
                 linetype = "dashed", color = "black")

    return(graph)
  }

  #standardize column names to lower case
  names(raw_data) <- tolower(names(raw_data))

  # complete cases option
  if (completecase == FALSE) {
  }
  if (completecase == TRUE) {
    raw_data <- raw_data[ave(complete.cases(raw_data),
                            raw_data$patient, FUN = all), ]
  }
  # filters data and creates individual datasets per group
  group1 <- raw_data %>% filter(group == group1name)
  group2 <- raw_data %>% filter(group == group2name)

  # creates individual tables for each group of cumulative averages
  group1step2 <- cumulative_table(iterative_auc(group1))
  group2step2 <- cumulative_table(iterative_auc(group2))

  # creates complete table of averages +
  #tstat calculations for combined data of groups
  completetable <- merged_results(group1step2, group2step2)

  #creates table of iterative changes in cumulative average for groups
  iterativechanges1 <- iterative_table(iterative_auc(group1))
  iterativechanges2 <- iterative_table(iterative_auc(group2))

  # holm-bonferroni adjusted p-value table
  adjustedtable <- merged_results(iterativechanges1, iterativechanges2)

  # extract first point of no significance
  timesig <- holm_test(adjustedtable, conf)

  #top plot
  topplot <- topgraph(raw_data, conf)

  #creating bottom plot
  bottomplot <- bottomgraph(completetable, conf, group1name, group2name,
                           timeunit,timesig, raw_data)
  # create combined plot

  finalplot <- ggarrange(topplot, bottomplot)

  return (finalplot)
}
