
library(pROC); # good
library(corrplot); # good
library(MASS); # good
library(igraph); # good
library(gRbase); # good

library(splitTools); # bad
library(ROSE); # bad
library(Rmisc); # bad
library(caret); # bad
library(regclass); # bad
library(Hmisc); # bad
library(dplyr); # bad
library(ggplot2); # bad
library(cowplot); # bad

data <- read.csv(file = "../data/SeoulBikeData.csv", sep = ",");
# Initial info
names(data);
dim(data);
# Data summary
summary(data);

# class disbalance
table(data$y)

# unique values of categorical features
categorical_feats <- c("default", "housing", "loan", "contact", "education", 
                       "job", "marital", "month", "day_of_week", "poutcome")
for (feat in categorical_feats) {
  print(feat)
  print(unique(data[, feat]))
  print("_________________________________________________________________")
}

# group features
bin_feats <- c("default", "housing", "loan", "contact")
ordinal_feats <- c("education")
nominal_feats <- c("job", "marital",
                   "month", "day_of_week", "poutcome")
continuous_feats <- c("age", "duration", "campaign", "pdays", "previous",
                     "emp.var.rate", "cons.price.idx", "cons.conf.idx",
                     "euribor3m", "nr.employed")

# prepare some data for analysis
# convert cat feats to factor 
data$y <- as.factor(data$y)
data$marital <- factor(data$marital, levels = c("single", "divorced", 
                                                "married"), ordered = FALSE)
data$education <- factor(data$education, levels = c('illiterate',
                                                    'basic.4y','basic.6y',
                                                    'basic.9y','high.school',
                                                    'professional.course',
                                                    'university.degree'), 
                         ordered = TRUE)
data$default <- as.factor(data$default)
data$housing <- as.factor(data$housing)
data$loan <- as.factor(data$loan)
data$contact <- as.factor(data$contact)
                                                     
data$day_of_week <- factor(data$day_of_week, 
                           levels = c("mon", "tue", "wed", "thu", "fri"), 
                           ordered = FALSE)
data$poutcome <- factor(data$poutcome, levels = c("nonexistent", "failure", 
                                                  "success"), ordered = FALSE)
data$job <- factor(data$job, levels = c('admin.','blue-collar','entrepreneur',
                                        'housemaid', 'management','retired',
                                        'self-employed','services',
                                        'student','technician','unemployed'), 
                   ordered = FALSE)

# temporarily treat month as an ordered factor
data$month <- factor(data$month, levels = c("jan", "feb", "mar", "apr", "may", 
                                            "jun", "jul", "aug", "sep", "oct", 
                                            "nov", "dec"), ordered = TRUE)
# adding year
data$year <- NA
year = 2008
for (i in 1:(nrow(data)-1)) {
  data[i, 'year'] <- year
  if (data[i, 'month'] > data[i+1, 'month']) {
    year <- year + 1
  } 
}
data[i+1, 'year'] <- year
continuous_feats <- c(continuous_feats, "year")

# temporal variable for creating barplots
data$y_num <- as.integer(as.integer(data$y) - 1)

# adding date
data$date <- paste(as.character(data$year), "01", 
                   as.character(as.numeric(data$month)), 
                   sep = "-")
data$date <- as.Date(data$date)
levels = as.character(unique(data$date))
data$date <- as.character(data$date)
data$date <- factor(data$date, levels=levels, ordered = TRUE)

# Let's see how the quality of campaign changed during the time
# Preparing data for plots
data_sse <- summarySE(data, measurevar="y_num", groupvars="date")
# alpha for building confidence interval
a <- .05
interval_value <- qnorm(1 - a / 2) * sqrt((1 / data_sse$N) * data_sse$y_num * 
                                               (1 - data_sse$y_num))
data_sse$upper_bound <- data_sse$y_num + interval_value
data_sse$lower_bound <- data_sse$y_num - interval_value
data_sse$lower_bound <- ifelse(data_sse$lower_bound > 0, data_sse$lower_bound, 
                               0)

settings <- theme(plot.title = element_text(hjust = 0.5, size = 16),
                  axis.title.x = element_text(size = 16),
                  axis.title.y = element_text(size = 16))

c1 <- ggplot(data_sse, aes(x=date, y=y_num)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound),
                width=.2,                  
                position=position_dodge(.9)) +
  labs(x = "Date", y = "Y ratio") + 
  ggtitle("Target values distribution by time") +
  settings

c2 <- ggplot(data, aes(x = date)) +
  geom_bar() + 
  labs(x = "Date", y = "Clients") + 
  ggtitle("Amount of calls by time") +
  settings

c3 <- ggplot(data, aes(x = date, y=y_num)) +
  geom_bar(stat = "identity", na.rm=TRUE) + 
  labs(x = "Date", y = "Positive Clients") + 
  ggtitle("Amount of attracted clients by time") +
  settings 

cowplot::plot_grid(c1, c2, c3, labels = NULL, ncol = 1, align = "v")

# plotting correlation matrix for continuous features
corrmat <- cor(data[, continuous_feats],
               use="pairwise.complete.obs")
corrplot(corrmat, method="number")

# plotting independence graph
S <- var(data[, continuous_feats], use="pairwise.complete.obs")
P <- cov2cor(S)
R <- -cov2cor(solve(S))
thr <- 0.2
G <- abs(R)>thr
diag(G) <- 0
Gi <- as(G, "igraph")
tkplot(Gi, vertex.color="white")

# distribution of continuous variables split by target pt 1

settings <- theme(axis.title.x = element_text(size = 22),
                  axis.title.y = element_text(size = 22),
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size=10)) 

g_age <- ggplot(data,aes(x=age,fill=y))+
  ggplot2::geom_density(alpha=.3) + settings

g_duration <- ggplot(data,aes(x=duration,fill=y))+
  ggplot2::geom_density(alpha=.3) + settings

g_campaign <- ggplot(data,aes(x=campaign,fill=y))+
  ggplot2::geom_histogram(aes(y=0.5*..density..),
                          alpha=0.5,position='identity') + labs(y = "density")+
  settings

g_pdays <- ggplot(data,aes(x=pdays,fill=y))+
  ggplot2::geom_histogram(aes(y=0.5*..density..),
                          alpha=0.5,position='identity') + labs(y = "density")+
  settings

# also we plot distribution of real pdays (<999)
g_pdays_f <- ggplot(data[data$pdays<999, ],aes(x=pdays,fill=y))+
  ggplot2::geom_density(alpha=.3)+ labs(x = "pdays < 999") + settings

g_previous <- ggplot(data,aes(x=previous,fill=y))+
  ggplot2::geom_histogram(aes(y=0.5*..density..),
                          alpha=0.8,position='identity') + labs(y = "density")+
  settings 

plot <- plot_grid(g_age, g_duration, g_campaign,
                  g_pdays, g_pdays_f, g_previous,
                  labels = NULL, 
                  nrow = 2, ncol = 3)

title <- ggdraw() + 
  draw_label(
    "Distribution of continuous variables split by target",
    fontface = 'bold',
    x = 0,
    hjust = -0.1,
    size = 22
  )
plot_grid(
  title, plot,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# distribution of continuous variables split by target pt 2

g_emp.var.rate <- ggplot(data,aes(x=emp.var.rate,fill=y))+
  geom_histogram(aes(y=0.5*..density..),
                 alpha=0.5,position='identity') + labs(y = "density")

g_cons.price.idx <- ggplot(data,aes(x=cons.price.idx,fill=y))+
  geom_histogram(aes(y=0.5*..density..),
                 alpha=0.5,position='identity') + labs(y = "density")

g_cons.conf.idx <- ggplot(data,aes(x=cons.conf.idx,fill=y))+
  geom_histogram(aes(y=0.5*..density..),
                 alpha=0.5,position='identity') + labs(y = "density")

g_euribor3m <- ggplot(data,aes(x=euribor3m,fill=y))+
  geom_histogram(aes(y=0.5*..density..),
                 alpha=0.5,position='identity') + labs(y = "density")

g_nr.employed <- ggplot(data,aes(x=nr.employed,fill=y))+
  geom_histogram(aes(y=0.5*..density..),
                 alpha=0.8,position='identity') + labs(y = "density")

plot <- plot_grid(g_emp.var.rate, g_cons.price.idx, g_cons.conf.idx,
                  g_euribor3m, g_nr.employed,
                  labels = NULL, 
                  nrow = 2, ncol = 3)

title <- ggdraw() + 
  draw_label(
    "Distribution of continuous variables split by target",
    fontface = 'bold',
    x = 0,
    hjust = -0.3
  )
plot_grid(
  title, plot,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

par(mfrow=c(2, 3))
for (feat in continuous_feats[1:5]) {
  boxplot(data[,feat] ~ data[,"y"], xlab="y", ylab=feat)
}
mtext("Distribution of continuous variables split by target", side = 3, 
      line = - 2, outer = TRUE)

# Let's set ylim for some plots in order to eliminate some outliers with 
# extremely huge values
par(mfrow=c(2, 3))
boxplot(data$age ~ data$y, xlab="y", ylab="age")
boxplot(data$duration ~ data$y, xlab="y", ylab="duration", ylim = c(0, 2000))
boxplot(data$campaign ~ data$y, xlab="y", ylab="campaign", ylim = c(0, 10))
boxplot(data[data$pdays<999, 'pdays'] ~ data[data$pdays<999, 'y'], 
        xlab='y', ylab='pdays')
boxplot(data$previous ~ data$y, xlab="y", ylab="previous")
mtext("Distribution of continuous variables split by target", side = 3, 
      line = - 2, outer = TRUE)

par(mfrow=c(2, 3))
for (feat in continuous_feats[6:10]) {
  boxplot(data[,feat] ~ data[,"y"], xlab="y", ylab=feat)
}
mtext("Distribution of continuous variables split by target", side = 3, 
      line = - 2, outer = TRUE)

# distribution of continuous variables split by date pt 1
par(mfrow=c(2, 3))
for (feat in continuous_feats[1:5]) {
  boxplot(data[,feat] ~ data[,"date"], xlab="date", ylab=feat)
}
mtext("Distribution of continuous variables split by date", side = 3, 
      line = - 2, outer = TRUE)

# Let's set ylim for some plots in order to eliminate some outliers with 
# extremely huge values
par(mfrow=c(2, 3))
boxplot(data$age ~ data$date, xlab="date", ylab="age")
boxplot(data$duration ~ data$date, xlab="date", 
        ylab="duration", ylim = c(0, 2000))
boxplot(data$campaign ~ data$date, xlab="date", ylab="campaign", 
        ylim = c(0, 10))
# here we're considering only real pdays
boxplot(data[data$pdays<999, 'pdays'] ~ data[data$pdays<999, 'date'], 
        xlab='date', ylab='pdays')
boxplot(data$previous ~ data$date, xlab="date", ylab="previous")
mtext("Distribution of continuous variables split by target", side = 3, 
      line = - 2, outer = TRUE)

# distribution of continuous variables split by date pt 2
par(mfrow=c(2, 3))
for (feat in continuous_feats[6:10]) {
  boxplot(data[,feat] ~ data[,"date"], xlab="date", ylab=feat)
}
mtext("Distribution of continuous variables split by date", side = 3, 
      line = - 2, outer = TRUE)

# some of these features are constant during a month or three months
# let's use other kind of plot

agg_tbl <- data %>% dplyr::group_by(date) %>% 
  dplyr::summarise(emp.var.rate=unique(emp.var.rate))
agg_tbl$cons.price.idx <- as.data.frame(data %>% dplyr::group_by(date) %>% 
  dplyr::summarise(cons.price.idx=unique(cons.price.idx)))$cons.price.idx
agg_tbl$cons.conf.idx <-as.data.frame(data %>% dplyr::group_by(date) %>% 
  dplyr::summarise(cons.conf.idx=unique(cons.conf.idx)))$cons.conf.idx
agg_tbl$nr.employed <-as.data.frame(data %>% dplyr::group_by(date) %>% 
  dplyr::summarise(nr.employed=unique(nr.employed)))$nr.employed
list_plots <- list()
list_plots <- outlist <- append(list_plots, list(ggplot(agg_tbl, 
                                                   aes(x = date,
                                                       y = emp.var.rate, 
                                                    group = 1)) + geom_line()))
list_plots <- outlist <- append(list_plots, list(ggplot(agg_tbl, 
                                                   aes(x = date, 
                                                       y = cons.price.idx, 
                                                    group = 1)) + geom_line()))
list_plots <- outlist <- append(list_plots, list(ggplot(agg_tbl, 
                                                   aes(x = date,
                                                      y = cons.conf.idx, 
                                                    group = 1)) + geom_line()))
list_plots <- outlist <- append(list_plots, list(ggplot(agg_tbl, 
                                                   aes(x = date,
                                                       y = nr.employed, 
                                                    group = 1)) + geom_line()))
plot <- plot_grid(plotlist=list_plots, 
                  labels = NULL, 
                  nrow = 4, ncol = 1)
title <- ggdraw() + 
  draw_label(
    "Social and economic context feature",
    fontface = 'bold',
    x = 0,
    hjust = -0.4
  )
plot_grid(
  title, plot,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# distribution of binary variables split by target
list_plots <- list()
# alpha for building confidence interval
a <- .05
for (feat in bin_feats) {
  if (feat == "contact") {lab = "contact is phone"} else {lab = feat}
  data_feat <- data[!is.na(data[, feat]), ]
  data_feat[, feat] <- as.factor(data_feat[, feat])
  data_feat[, feat] <- as.numeric(data_feat[, feat]) - 1
  data_temp_sse <- summarySE(data_feat, measurevar=feat, groupvars="y")
  interval_value <- qnorm(1 - a / 2) * sqrt((1 / data_temp_sse$N) * 
                                              data_temp_sse[, feat] * 
                                              (1 - data_temp_sse[, feat]))
  interval_value[is.na(interval_value)] <- 0
  data_temp_sse$upper_bound <- data_temp_sse[, feat] + interval_value
  data_temp_sse$lower_bound <- data_temp_sse[, feat] - interval_value
  data_temp_sse$lower_bound <- ifelse(data_temp_sse$lower_bound > 0, 
                                      data_temp_sse$lower_bound, 0)
  curr_plot <- ggplot(data_temp_sse, aes(x=.data[["y"]], y=.data[[feat]])) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + ylab(lab)
  list_plots <- outlist <- append(list_plots,list(curr_plot))
}

plot <- plot_grid(plotlist=list_plots, 
          labels = NULL, 
          nrow = 2, ncol = 2)

title <- ggdraw() + 
  draw_label(
    "Distribution of binary variables split by target",
    fontface = 'bold',
    x = 0,
    hjust = -0.3
  )
plot_grid(
  title, plot,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# distribution of binary variables split by date 
list_plots <- list()
for (feat in bin_feats) {
  data_temp <- data[!is.na(data[, feat]), ]
  data_temp[, feat] <- as.factor(data_temp[, feat])
  data_temp[, feat] <- as.numeric(data_temp[, feat]) - 1
  data_temp_sse <- summarySE(data_temp, measurevar=feat, groupvars="date")
  interval_value <- qnorm(1 - a / 2) * sqrt((1 / data_temp_sse$N) * 
                                              data_temp_sse[, feat] * 
                                              (1 - data_temp_sse[, feat]))
  interval_value[is.na(interval_value)] <- 0
  data_temp_sse$upper_bound <- data_temp_sse[, feat] + interval_value
  data_temp_sse$lower_bound <- data_temp_sse[, feat] - interval_value
  data_temp_sse$lower_bound <- ifelse(data_temp_sse$lower_bound > 0, 
                                      data_temp_sse$lower_bound, 0)
  curr_plot <- ggplot(data_temp_sse, aes(x=.data[["date"]], y=.data[[feat]])) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))
  list_plots <- outlist <- append(list_plots,list(curr_plot))
}

plot <- plot_grid(plotlist=list_plots, 
                  labels = NULL, 
                  nrow = 4, ncol = 1)
title <- ggdraw() + 
  draw_label(
    "Distribution of binary variables split by date",
    fontface = 'bold',
    x = 0,
    hjust = -0.4
  )
plot_grid(
  title, plot,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# distribution of categorical variables split by target
list_plots <- list()
for (feat in c(nominal_feats, ordinal_feats)) {
  data_feat <- with(data, table(y, data[, feat], dnn = c('y', feat)))
  data_feat <- as.data.frame(data_feat)
  # For each group we normalize the frequencies separately
  data_feat$NormFreq <- data_feat$Freq / 
    sapply(data_feat$y, function(x) sum(data_feat[data_feat$y == x, 'Freq']))
  curr_plot <- ggplot(data_feat, aes(x=.data[["y"]], y=.data[["NormFreq"]], 
                                     fill = .data[[feat]])) +     
    geom_col(position = 'dodge') + 
    ylab(feat)
  list_plots <- outlist <- append(list_plots,list(curr_plot))
}
plot <- plot_grid(plotlist=list_plots, labels = NULL, nrow = 3, ncol = 2)
title <- ggdraw() + 
  draw_label(
    "Distribution of categorical variables split by target",
    fontface = 'bold',
    x = 0,
    hjust = -0.4
  )
plot_grid(
  title, plot,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# distribution of categorical variables split by date
list_plots <- list()
cut_date <- as.Date("2009-01-05")
cut_date <- as.character(cut_date)
for (feat in c(nominal_feats, ordinal_feats)) {
  data_timesplit <- data
  data_timesplit$timesplit <- data_timesplit$date > cut_date
  data_feat <- with(data_timesplit, table(timesplit, data_timesplit[, feat], 
                                          dnn = c('timesplit', feat)))
  data_feat <- as.data.frame(data_feat)
  data_feat$NormFreq <- data_feat$Freq / 
    sapply(data_feat$timesplit, 
           function(x) sum(data_feat[data_feat$timesplit == x, 'Freq']))
  
  
  
  curr_plot <- ggplot(data_feat, aes(x=.data[["timesplit"]], 
                                     y=.data[["NormFreq"]], 
                                     fill = .data[[feat]])) +     
    geom_col(position = 'dodge') + 
    ylab(feat) + scale_x_discrete(labels=c("TRUE" = "after", 
                                           "FALSE" = "before"))
  list_plots <- outlist <- append(list_plots,list(curr_plot))
}
plot <- plot_grid(plotlist=list_plots, labels = NULL, nrow = 3, ncol = 2)
title <- ggdraw() + 
  draw_label(
    "Distribution of categorical variables split by date",
    fontface = 'bold',
    x = 0,
    hjust = -0.4
  )
plot_grid(
  title, plot,
  ncol = 1,
  rel_heights = c(0.1, 1)
)