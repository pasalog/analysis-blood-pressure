man <- 0
woman <- 1
datasetFile <- "data.csv"
dataSet <- read.csv(datasetFile, header = TRUE, ";")
men.sysbp <- dataSet[which(dataSet$GENDER == man),]['SYSBP'][, 1]
men.diasbp <- dataSet[which(dataSet$GENDER == man),]['DIASBP'][, 1]
women.sysbp <- dataSet[which(dataSet$GENDER == man),]['SYSBP'][, 1]
women.diasbp <- dataSet[which(dataSet$GENDER == man),]['DIASBP'][, 1]


men.sysbp.sorted <- sort(men.sysbp)
men.sysbp.mean <- mean(men.sysbp)
men.sysbp.most.frequent.value <- Mode(men.sysbp)
men.sysbp.most.frequent.value.count <- length(dataSet[which(dataSet$SYSBP == men.sysbp.most.frequent.value),]['SYSBP'][, 1])
# for (man in men.sysbp) {
#   tmp <- length(dataSet[which(dataSet$SYSBP == man),]['SYSBP'][, 1])
#   tmp
#   if (tmp > men.sysbp.max.frequency) men.sysbp.max.frequency <- tmp
# }
men.sysbp.median <- median(men.sysbp)
men.sysbp.max <- max(men.sysbp)
men.sysbp.min <- min(men.sysbp)
men.sysbp.sum <- sum(men.sysbp)
men.sysbp.std <- sd(men.sysbp, na.rm = FALSE)
men.sysbp.fiveNumbersSumy <- quantile(men.sysbp)
men.sysbp.lowerQuartile <- men.sysbp.fiveNumbersSumy[2]
men.sysbp.upperQuartile <- men.sysbp.fiveNumbersSumy[4]
men.sysbp.interQuartile <- men.sysbp.upperQuartile - men.sysbp.lowerQuartile
men.sysbp.variance <- men.sysbp.std^2
men.sysbp.range <- men.sysbp.max - men.sysbp.min
men.sysbp.range.over.std <- men.sysbp.range / men.sysbp.std
men.sysbp.largest.zscore <- (men.sysbp.max - men.sysbp.mean) / men.sysbp.std
men.sysbp.smallest.zscore <- (men.sysbp.min - men.sysbp.mean) / men.sysbp.std
stem(men.sysbp)
hist(men.sysbp, breaks = 5, col = "blue", density = 20, main = "5 Subinterval Histogram", labels = FALSE)
hist(men.sysbp, breaks = 10, col = "green", density = 15, main = "10 Subinterval Histogram", labels = FALSE)

#dotplot(men.sysbp, by = men.sysbp.mean, dot.col = "fucking")
ggplot(data.frame(men.sysbp), aes(x = men.sysbp)) +
  geom_dotplot(binaxis = 'x', stackdir = 'up', binwidth = length(men.sysbp) / men.sysbp.most.frequent.value.count - 2, method = "dotdensity") +
  scale_y_continuous(name = NULL, breaks = NULL)

# ggplot(data.frame(men.sysbp), aes(x = men.sysbp, y = men.sysbp.mean)) + stat_boxplot(mapping = NULL,
#                                                                                      data = data.frame(men.sysbp),
#                                                                                      geom = "boxplot",
#                                                                                      position = "dodge2",
#                                                                                      outlier.color = "RED",
#                                                                                      outlier.fill = NULL,
#                                                                                      outlier.shape = 19,
#                                                                                      outlier.size = 1.5,
#                                                                                      outlier.stroke = 0.5,
#                                                                                      outlier.alpha = NULL,
#                                                                                      notch = FALSE,
#                                                                                      notchwidth = 0.5,
#                                                                                      varwidth = FALSE,
#                                                                                      na.rm = FALSE,
#                                                                                      orientation = NA,
#                                                                                      show.legend = NA,
#                                                                                      inherit.aes = TRUE)

# MEN _ DIASBP

men.diasbp <- dataSet[which(dataSet$GENDER == man),]['DIASBP'][, 1]
men.diasbp.sorted <- sort(men.diasbp)
men.diasbp.mean <- mean(men.diasbp)
men.diasbp.median <- median(men.diasbp)
men.diasbp.max <- max(men.diasbp)
men.diasbp.min <- min(men.diasbp)
men.diasbp.sum <- sum(men.diasbp)
men.diasbp.std <- sd(men.diasbp, na.rm = FALSE)
men.diasbp.variance <- men.diasbp.std^2
men.diasbp.range <- men.diasbp.max - men.diasbp.min
men.diasbp.range.over.std <- men.diasbp.range / men.diasbp.std

# WOMEN _ SYSBP

women.sysbp <- dataSet[which(dataSet$GENDER == woman),]['SYSBP'][, 1]
women.sysbp.sorted <- sort(women.sysbp)
women.sysbp.mean <- mean(women.sysbp)
women.sysbp.median <- median(women.sysbp)
women.sysbp.max <- max(women.sysbp)
women.sysbp.min <- min(women.sysbp)
women.sysbp.sum <- sum(women.sysbp)
women.sysbp.std <- sd(women.sysbp, na.rm = FALSE)
women.sysbp.variance <- women.sysbp.std^2
women.sysbp.range <- women.sysbp.max - women.sysbp.min
women.sysbp.range.over.std <- women.sysbp.range / women.sysbp.std

# WOMEN _ DIASBP

women.diasbp <- dataSet[which(dataSet$GENDER == woman),]['DIASBP'][, 1]
women.diasbp.sorted <- sort(women.diasbp)
women.diasbp.mean <- mean(women.diasbp)
women.diasbp.median <- median(women.diasbp)
women.diasbp.max <- max(women.diasbp)
women.diasbp.min <- min(women.diasbp)
women.diasbp.sum <- sum(women.diasbp)
women.diasbp.std <- sd(women.diasbp, na.rm = FALSE)
women.diasbp.variance <- women.diasbp.std^2
women.diasbp.range <- women.diasbp.max - women.diasbp.min
women.diasbp.range.over.std <- women.diasbp.range / women.diasbp.std

boxplot(men.diasbp, women.diasbp, men.sysbp, women.sysbp)
