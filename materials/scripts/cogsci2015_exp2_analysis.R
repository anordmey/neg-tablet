###negpad verification###
###Merge data files###

rm(list=ls())

#Load libraries
library(ggplot2)
library(bootstrap)
library(lme4)

#set working directory
#setwd("")

#Functions
# for bootstrapping 95% confidence intervals
theta <- function(x,xdata) {mean(xdata[x])}
ci.low <- function(x) {
  quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
  quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.975)}

plot.style <- theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), legend.position="right", axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))


#Load data
d <- read.csv("../data/cogsci2015_exp2.csv")

#Hist of ratings scale
histogram(d$rating)

##Plot data

##Plot everything
ms <- aggregate(rating ~ subid + condition + sentence.type + truth.value, d, mean)
mss <- aggregate(rating ~ condition + sentence.type + truth.value, ms, mean)
mss$low <- aggregate(rating ~ condition + sentence.type + truth.value, ms, ci.low)$rating
mss$high <- aggregate(rating ~ condition + sentence.type + truth.value, ms, ci.high)$rating

qplot(data=mss, x=condition, y=rating, fill=sentence.type, 
      geom="bar", stat="identity", position=position_dodge(width=.95), 
      facets=~truth.value) + 
  geom_errorbar(aes(ymin=low, ymax=high), width=0,
                position=position_dodge(width=.95)) + 
  plot.style

##Just look at negatives
ms <- aggregate(rating ~ subid + condition + truth.value, d, mean)
mss <- aggregate(rating ~ condition +  truth.value, ms, mean)
mss$low <- aggregate(rating ~ condition + truth.value, ms, ci.low)$rating
mss$high <- aggregate(rating ~ condition + truth.value, ms, ci.high)$rating

qplot(data=mss, x=condition, y=rating,  
      geom="bar", stat="identity", position=position_dodge(width=.95), 
      facets=~truth.value) + 
  geom_errorbar(aes(ymin=low, ymax=high), width=0,
                position=position_dodge(width=.95)) + 
  plot.style

#Just look at true negative sentences
d.trueneg <- d[d$truth.value=="TRUE" & d$sentence.type =="negative",]

##Look at negation concept & negation syntax (just true negatives)
ms <- aggregate(rating ~ subid + condition, d.trueneg, mean)
mss <- aggregate(rating ~ condition, ms, mean)
mss$low <- aggregate(rating ~ condition, ms, ci.low)$rating
mss$high <- aggregate(rating ~ condition, ms, ci.high)$rating

mss$condition <- factor(mss$condition, levels=c("none","foil", "target"), labels=c("None", "Foil", "Target"))

qplot(data=mss, x=condition, y=rating, 
      geom="bar", stat="identity", position="dodge") + 
  geom_errorbar(aes(ymin=low, ymax=high), width=0,
                position=position_dodge(width=.9)) + 
  scale_fill_grey() + 
  xlab("Context") + ylab("Rating") + 
  coord_equal(1/1.5) +
  plot.style

###### -----Statistical Models-------- #######
d$sentence.type <- factor(d$sentence.type, levels=c("positive","negative"))
d$condition <- factor(d$condition, levels=c("none","foil","target"))

model.all <- lmer(rating ~ condition*sentence.type*truth.value +
                    (sentence.type*truth.value | subid) +
                    (sentence.type*truth.value | item), 
                  data=d)

model.neg <- lmer(rating ~ condition*truth.value +
                        (truth.value | subid) +
                        (truth.value | item), 
                      data=subset(d, sentence.type=="negative"))


model.trueneg <- lmer(rating ~ condition +
                     (1 | subid) +
                     (1 | item), 
                   data=subset(d, truth.value=="TRUE" & sentence.type=="negative"))

