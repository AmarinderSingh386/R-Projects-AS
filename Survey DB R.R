getwd()
setwd("C:/Users/samar/Downloads")
list.files()
library(tidyverse)
library(ggplot2)

################################ HISTOGRAM #####################################

HG <- read.csv("histogram_survey_data.csv",
                           header = TRUE,
                           sep = ",")
str(HG)

Age_hist <- ggplot(HG,
               aes(x = Age)) +
  geom_histogram(binwidth=5,bins = 12,
                 fill = "#651a1a",
                 color = "white") +
  theme_classic() +
  ggtitle("Distribution of Age") +
  xlab("Age Interval") +
  ylab("Count of People") +
  theme(plot.title = element_text(size = 20, 
                                  face = "bold", hjust = 0.5))
Age_hist

##Comments##

#The data distribution is positive skewed (1.009940035) 
#It is leptokurtic as kurtosis is >0 (0.479094097) 
#The distribution is following a downward trend which denotes that the no of people
#With geodemographic feature reduce as the age increase. 

################################ BOXPLOT #######################################

install.packages("readxl")
library("readxl")
BP <- read_excel("boxplot.xlsx")
View(BP)

ggplot(BP, aes(x=Party, y=Score, fill=Party)) + 
  geom_boxplot(alpha=0.9) +  
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")

##Comments##

#The level of satisfaction is sometimes independent of political party.
#Democrat group's rating are evenly distributed for level of satisfaction with spreadout data..
#Independent group's ratings have the highest level of satisfaction with compact data.
#Republican group's rating are above level of satisfaction.
#The outlier in republican data means that few people are dissatisfied and rated 1.

################################ Scatter Plot ##################################

library("wesanderson")
names(wes_palettes)

SP <- read.csv("scatter_plot.csv", 
                           header = TRUE, 
                           sep = ",")
str(SP)

scatter<- ggplot(SP,aes(x = Budget, y = Sales))+
  geom_point(color = "midnightblue", size = 2, alpha = 0.9)+
  geom_smooth(se = FALSE, color = "goldenrod2", size = 1)+
  theme_classic()+
  theme(legend.justification = c(0.01, 1),
        legend.position = c(0.01,1))+
  ggtitle("Relationship Between Budget & Sales") +
  xlab("Sales")+
  ylab("Budget")

scatter

##Comments##

#We can see as the budget for marketing increase the sales go up so we can say,
#The relationship between the variable is positive.
#The variation is not much because we can see less outliers therefore the data/sample is good.

  










