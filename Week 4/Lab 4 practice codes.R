## set working directory
setwd("your path")

## read the file
gss <- read.csv("GSS_SOCUA_W2.csv")

## subset and summarize data using dplyr
library(dplyr)
gss %>% filter(sibs>=0)
gss %>% filter(sibs>=0) %>% summarize(sibs_mean = mean(sibs))
gss %>% filter(sibs>=0) %>% summarize(sibs_mean = mean(sibs),
                                      sibs_var = var(sibs),
                                      sibs_median = median(sibs))

## summarize data by group
unique(gss$relig)
gss[gss$relig!=4 & gss$relig>0,
    "relig"] <- 1
gss[gss$relig==4,
    "relig"] <- 0


gss %>% filter(relig>=0&sex>=0) %>%
  group_by(sex) %>%
  summarize(mean_relig = mean(relig))

## Exercise: How to get the same statistic, but only in 2021?

gss %>%
  filter(relig>=0&sex>0&year==2021) %>%
  group_by(sex) %>%
  summarize(religious = mean(relig))

## Exercise: How to get the proportion of religious people (at the time of being interviewed, the column relig) by whether they were raised in a religious family?

## visualizing the trend
library(ggplot2)
gss %>%
  filter(relig>=0&sex>0) %>%
  group_by(year, sex) %>%
  summarize(religious = mean(relig)) %>%
  ggplot(aes(x=year,y=religious,group=sex,color=sex)) +
  geom_line()

## Central Limit Theorem

## create population
population <- c(rep(0,4800000),rep(1,5200000)) ## p = 52%

## sample with n=1000
sample <- sample(population,1000)

## calculate sample mean
mean(sample)


## iterate a process for 10 times - using a FOR LOOP
for (i in c(10,30,50,70,90)){
  print(i)
  print(1+1)
}

for (i in c(1,3,5,7,9)){
  print(1+1)
}

## empty vector to store means of sample (K times)
mean_of_sample <- c()

## iterate the process of 2000 times
for (i in 1:2000){
  ## sample with n=2000
  sample <- sample(population,2000)
  mean <- mean(sample)
  mean_of_sample <- c(mean_of_sample,mean)
}

## plot histogram
hist(mean_of_sample,breaks=20,
     main="Distribution of Sample Means",
     xlab="Sample Mean")


