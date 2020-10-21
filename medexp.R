#' ---
#' title: "Analysis of medical expenses over time"
#' author: "Jingheng Cui"
#' date: "OCT 21, 2020"
#' ---

#import data
#DATA SOURCE: OECD Data
library(readr)
med_exp = read_csv('data-viz-test-QueryResult.csv')
head(med_exp)

#data visualization with ggplot
library(ggplot2)
ggplot(data = med_exp, mapping = aes(x=time,y=value))+
  geom_point()

ggplot(data = med_exp, mapping = aes(x=time,y=value,color=location,group=location))+
  geom_line()

ggplot(data = med_exp, mapping = aes(x=factor(time),y=value))+
  geom_boxplot()

library(lme4)

mixed = lmer(value~time+(1|location),data = med_exp,REML = TRUE)
summary(mixed)

#Inference for random term - location
non_loc = lm(value~time,data = med_exp)
library(RLRsim)
exactLRT(mixed,non_loc)

plot(fitted(mixed),resid(mixed,type='pearson'))
qqnorm(resid(mixed))
