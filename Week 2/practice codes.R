########################
####### Vector #########
########################

## create a vector
x <- c(10,15,20,25,30,35,40)

## subset the vector
x[3]
x[c(1,3,5)]
x[x<20] ## try by yourself what x<20 would produce! See if you can understand the logic of x[x<20] here

########################
####### Matrix #########
########################

## create a matrix
x <- 1:9
X <- matrix(x, nrow = 3, ncol = 3, byrow = TRUE)

## subset the matrix
X[1,2]
X[1,]
X[,2]

## exercise
X[c(1,3),c(2,3)]
X[c(TRUE,TRUE,FALSE),c(TRUE,TRUE,FALSE)]

############################
####### Data frame #########
############################

## create a data frame
example_data <- data.frame(x = c(1, 3, 5, 7, 9, 1, 3, 5, 7, 9),
                           y = c(rep("Hello", 9), "NYU"),
                           z = rep(c(TRUE, FALSE), 5))

## subset the data frame
example_data[1,2]
example_data[2,]


#######Import Data#########

## get current working directory
getwd()

## set current working directory
setwd("~/Dropbox/Teaching/Week 2")

## read the GSS data stored in your current working directory
gss <- read.csv("GSS_SOCUA_W2.csv")

## inspect data
head(gss, n=5)
tail(gss, n=5)
View(gss)
str(gss)

## inspect column/variable names
colnames(gss)

## select a certain column/variable
gss$sibs

## inspect variables
min(gss$sibs)
max(gss$sibs)
unique(gss$marital)

## inspect the data structure
dim(gss)
nrow(gss)
ncol(gss)

gss[gss$year > 2018, "marital"]
gss[gss$year > 2018, c("marital","year","sibs")]

## subset data (dplyr)
install.packages("dplyr")
library(dplyr)
gss %>% 
  filter(year>2018) %>% 
  select(marital,sibs,age,sex)

## summarize data
mean(gss$sibs)
median(gss$sibs)
var(gss$sibs)

## subset and summarize data (base R)
gss[gss$sibs>=0,"sibs"]
mean(gss[gss$sibs>=0,"sibs"])

var(gss[gss$year==2018 & gss$sibs>=0,"sibs"])

## subset and summarize data (dplyr)
gss %>% filter(sibs>0) %>% summarize(sibs_mean = mean(sibs))

## histogram
hist(gss[gss$age>0 & gss$year<=2018 & gss$year>=2012,"age"],
     main = "Distribution of Respondents Ages in 2012-2018, GSS", 
     xlab = "Age",
     breaks = 30)

## barplot
counts <- table(gss[gss$age>0 & gss$year<=2018 &
                      gss$year>=2012 & gss$marital>=0, "marital"])

barplot(counts, main="Distribution of Marital Status in 2012-2018, GSS",
        xlab="Marital Status",
        names.arg=c("Married", "Widowed", "Divorced", "Separated", 
                    "Never Married"))


