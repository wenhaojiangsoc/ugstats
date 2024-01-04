## import packages you will need
library(dplyr)
library(stargazer)
library(jtools)
library(ggplot2)

################################################
### you should run install.packages("dplyr")
### if you have not installed the package before
################################################

## set working directory - you should set your own unique one!
setwd("~/Dropbox/Teaching/SOCUA-302/Week 2")

## read the file
gss <- read.csv("GSS_SOCUA_W2.csv")

## transform missing values to NA (Not Available)
gss[gss<0] <- NA

## recode religion
gss[which(gss$relig!=4),"relig"] <- 1 ## if the original relig is not 4, recode them into 1 (some religion)
gss[which(gss$relig==4),"relig"] <- 0 ## if the original relig is 4, recode them into 0 (atheist)
########################################################
### you can follow this format in recode other
### variables. For example, you may want to recode
### education from years into categories
### The codes below categorize education into 4 groups
### high school or less, some college, college, and graduate
### gss[which(gss$educ<=12),"educ_cat"] <- 1
### gss[which(gss$educ>12&gss$educ<16),"educ_cat"] <- 2
### gss[which(gss$educ==16),"educ_cat"] <- 3
### gss[which(gss$educ>16),"educ_cat"] <- 4
########################################################

## filter only a certain year of observation or filter a subset of the data by some conditions
gss_2021data <- gss %>% filter(year==2021) ## return observations only in 2021 and store the data in a new dataframe called gss_newdata
gss_college <- gss %>% filter(educ>=16) ## return observations who have at least college education

## cross table
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(gss, row.vars = "educ_cat",
         col.vars = "relig",
         type = "f") ## "f" represents frequency
## check instructions here 
## http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
################################################
### Check Lab 11 for other options. Be sure to
### indicate in your memo what each value means
### in your cross table
################################################

## visualize trend
gss %>%
  filter(!is.na(sex)) %>% ## drop NA values for the variable sex
  mutate(sex = as.character(sex)) %>% ## transform the variable sex into characters
  group_by(year, sex) %>% ## summarize the data by year and sex
  summarize(religious = mean(relig,na.rm=T)) %>% ## summarize the year-sex-specific proportion of religious people; NA values are omitted when calculating the proportion by na.rm=T
  ggplot(aes(x=year,y=religious,color=sex)) + ## set the x-axis to be year, y-axis to be religious proportion (the one I created in the above line), colors vary by gender
  geom_line() + ## add the trending line
  scale_color_manual(labels = c("Male", "Female"), ## manually set lables and colors for the two lines
                     values = c("darkblue", "darkred")) +
  theme_bw() + ## change the background theme. You can also use theme_classic()
  ggtitle("The Proportion of Religious People by Gender, 1972-2021, GSS") + ## change the title
  xlab("Year") + ## change the x-axis title
  ylab("Proportion with Religious Identifications") + ## change the y-axis title
  theme(plot.title=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8)) ## format the data. You can skip it if your plot looks good

## now we go back to Week 11's data 
setwd("~/Dropbox/Teaching/SOCUA-302/Week 11")
gss <- read.csv("GSS_SOCUA_W11.csv")
gss[which(gss$relig!=4),"relig"] <- 1 ## if the original relig is not 4, recode them into 1 (some religion)
gss[which(gss$relig==4),"relig"] <- 0 ## if the original relig is 4, recode them into 0 (atheist)

## regression
model1 <- lm(rincome~educ,gss)
model2 <- lm(rincome~educ+sex,gss)
model3 <- lm(rincome~educ+sex+educ*sex,gss)
stargazer(model1,model2,model3, type = "text",
          header=FALSE,
          title = "The association between education, sex and income",
          digits = 1,
          omit.stat = c("rsq","f","ser"),
          dep.var.labels = "Annual Income",
          covariate.labels = c("Education","Gender","Education*Gender"))

## regression when the independent variable is categorical
model4 <- lm(relig~educ+sex+factor(marital),gss)
stargazer(model4, type = "text",
          header=FALSE,
          title = "The association between education, sex, marital status and religion",
          digits = 3,
          single.row = T,
          omit.stat = c("rsq","f","ser"),
          dep.var.labels = "Religious Identification",
          covariate.labels = c("Education","Gender","Widowed","Separated","Divorced","Single"))

plot_summs(model4,
           colors="darkred",
           coefs = c("Education" = "educ", "Women" = "sex",
                     "Widowed" = "factor(marital)2",
                     "Divorced" = "factor(marital)3",
                     "Separated" = "factor(marital)4",
                     "Never married" = "factor(marital)5"))

## plot the fitted regression lines if there are interactions

## create a hypothetical data for men
predict_men <-
  data.frame(educ=seq(0,20,1),
             sex=rep(0,21))

## make predictions
y_hat <-
  predict.lm(model3,
             predict_men)
## store the data
predict_men$y_hat <- y_hat

## create a hypothetical data for women
predict_women <-
  data.frame(educ=seq(0,20,1),
             sex=rep(1,21))
## make predictions
y_hat <-
  predict.lm(model3,
             predict_women)
## store the data
predict_women$y_hat <- y_hat

## combine the two data
predict <- rbind(predict_men,predict_women)

## plot
ggplot(predict, ## the data
       aes(x=educ,y=y_hat,group=factor(sex),color=factor(sex))) + ## x-axis is education, y-axis is predicted income, we group and color predictions by sex
  geom_line() + ## plot the trending line
  scale_color_manual(labels = c("Men", "Women"),
                     values = c("darkblue", "darkred")) + ## manually set labels and colors for the two lines
  xlab("Years of education") + ## x-axis label
  ylab("Predicted Income") + ## y-axis label
  theme_bw() +
  theme(legend.title= element_blank()) ## no legend title
