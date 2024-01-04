## set working directory
setwd("your path")

## read the file
gss <- read.csv("GSS_SOCUA_W2.csv")

## mean and variance of a subsetted data
mean(gss$sibs)
unique(gss$sibs)
gss[gss$sibs>=0 , "sibs"]
mean(gss[gss$sibs>=0,"sibs"])
var(gss$sibs)
var(gss[gss$sibs>=0 & gss$year==2018,"sibs"])

## subset and summarize data using dplyr
library(dplyr)
gss %>% filter(sibs>=0)
gss %>% filter(sibs>=0) %>% summarize(sibs_mean = mean(sibs))
gss %>% filter(sibs>=0) %>% summarize(sibs_mean = mean(sibs),
                                      sibs_var = var(sibs),
                                      sibs_median = median(sibs))

## histogram
hist(gss$age) ## all obs.
hist(gss[gss$age>0,"age"])
hist(gss[gss$age>0 & gss$year<=2018 & gss$year>=2012,"age"]) ## subset obs.
hist(gss[gss$age>0 & gss$year<=2018 & gss$year>=2012,"age"],
     main = "Distribution of Respondents Ages in 2012-2018, GSS", 
     xlab = "Age",
     ylab = "Count") ## change title and x-axis label
hist(gss[gss$age>0 & gss$year<=2018 & gss$year>=2012,"age"], 
     main = "Distribution of Respondents Ages in 2012-2018, GSS",
     xlab = "Age",
     breaks = 30) ## change the number of bins

## barplot
counts <- table(gss[gss$age>0 & gss$year<=2018 &
                      gss$year>=2012 & gss$marital>=0, "marital"])
counts

barplot(counts)
barplot(counts, main="Distribution of Marital Status in 2012-2018, GSS",
        xlab="Marital Status",
        names.arg=c("Married", "Widowed", "Divorced", "Separated", 
                    "Never Married"))

## barplot - change category order
gss$marital <- factor(gss$marital,levels = c(1,3,4,2,5))
counts <- table(gss[gss$age>0 & gss$year<=2018 &
                      gss$year>=2012, "marital"])
barplot(counts, main="Distribution of Marital Status in 2012-2018, GSS",
        xlab="Marital Status",
        names.arg=c("Married", "Divorced", "Separated", "Widowed", "Never Married"))

## summarize data by group
unique(gss$relig)
gss[gss$relig!=4 & gss$relig>=0,
    "relig"] <- 1
gss[gss$relig==4,
    "relig"] <- 0

gss %>%
  filter(relig>=0&sex>0) %>%
  group_by(sex) %>%
  summarize(religious = mean(relig))

## Exercise: How to get the same statistic, but only in 2021?

## Exercise: How to get the proportion of religious people (at the time of being interviewed, the column relig) 
## by whether they were raised in a religious family?

gss %>%
  filter(relig>=0&sex>0&year==2021) %>%
  group_by(sex) %>%
  summarize(religious = mean(relig))

## visualizing the trend
library(ggplot2)
gss %>%
  filter(relig>=0&sex>0) %>%
  group_by(year, sex) %>%
  summarize(religious = mean(relig)) %>%
  ggplot(aes(x=year,y=religious,group=sex,color=sex)) +
  geom_line()

