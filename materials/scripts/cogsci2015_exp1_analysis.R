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
d <- read.csv("../data/cogsci2015_exp1.csv")

#Hist of ratings scale
histogram(d$rating)

##Plot data

##Plot everything
ms <- aggregate(rating ~ subid + condition + sentence.type + truth.value + neg.concept + neg.syntax, d, mean)
mss <- aggregate(rating ~ condition + sentence.type + truth.value + neg.concept + neg.syntax, ms, mean)
mss$low <- aggregate(rating ~ condition + sentence.type + truth.value + neg.concept + neg.syntax, ms, ci.low)$rating
mss$high <- aggregate(rating ~ condition + sentence.type + truth.value + neg.concept + neg.syntax, ms, ci.high)$rating

qplot(data=mss, x=sentence.type, y=rating, 
      color=neg.concept, shape=neg.syntax, 
      geom="point", stat="identity", position=position_dodge(width=.5), 
      facets=~truth.value~condition) + 
  geom_errorbar(aes(ymin=low, ymax=high), width=0,
                position=position_dodge(width=.5)) + 
  plot.style

#Just look at negative sentences
d.neg <- d[d$sentence.type =="negative",]

##Look at negation concept & negation syntax (just true negatives)
ms <- aggregate(rating ~ subid + condition + truth.value + neg.concept + neg.syntax, d.neg, mean)
mss <- aggregate(rating ~ condition + truth.value + neg.concept + neg.syntax, ms, mean)
mss$low <- aggregate(rating ~ condition + truth.value + neg.concept + neg.syntax, ms, ci.low)$rating
mss$high <- aggregate(rating ~ condition + truth.value + neg.concept + neg.syntax, ms, ci.high)$rating

mss$condition <- factor(mss$condition, levels=c("noContext","context"), labels=c("None", "Target"))
mss$neg.concept <- factor(mss$neg.concept, levels=c("something","nothing"), labels=c("Alternative", "Nonexistence"))
mss$neg.syntax <- factor(mss$neg.syntax, levels=c("has no","doesn't have"), labels=c("has no X", "doesn't have X"))
mss$truth.value <- factor(mss$truth.value, levels=c("TRUE","FALSE"))

qplot(data=mss, x=condition, y=rating, fill=neg.concept, 
      geom="bar", stat="identity", position="dodge", 
      facets=truth.value~neg.syntax) + 
  geom_errorbar(aes(ymin=low, ymax=high), width=0,
                position=position_dodge(width=.9)) + 
  scale_fill_grey("Negation Type") + 
  xlab("Context") + ylab("Rating") + 
  plot.style


#Just look at true negative sentences
d.trueneg <- d[d$truth.value=="TRUE" & d$sentence.type =="negative",]

##Look at negation concept & negation syntax (just true negatives)
ms <- aggregate(rating ~ subid + condition + neg.concept + neg.syntax, d.trueneg, mean)
mss <- aggregate(rating ~ condition + neg.concept + neg.syntax, ms, mean)
mss$low <- aggregate(rating ~ condition + neg.concept + neg.syntax, ms, ci.low)$rating
mss$high <- aggregate(rating ~ condition + neg.concept + neg.syntax, ms, ci.high)$rating

mss$condition <- factor(mss$condition, levels=c("noContext","context"), labels=c("None", "Target"))
mss$neg.concept <- factor(mss$neg.concept, levels=c("something","nothing"), labels=c("Alternative", "Nonexistence"))
mss$neg.syntax <- factor(mss$neg.syntax, levels=c("has no","doesn't have"), labels=c("has no X", "doesn't have X"))

qplot(data=mss, x=condition, y=rating, fill=neg.concept, 
      geom="bar", stat="identity", position="dodge", 
      facets=~neg.syntax) + 
  geom_errorbar(aes(ymin=low, ymax=high), width=0,
                position=position_dodge(width=.9)) + 
  scale_fill_grey("Negation Type") + 
  xlab("Context") + ylab("Rating") + 
  plot.style

###### -----Statistical Models-------- #######
d$sentence.type <- factor(d$sentence.type, levels=c("positive","negative"))
d$condition <- factor(d$condition, levels=c("noContext","context"))
model.all <- lmer(rating ~ sentence.type*condition*truth.value +
                    (sentence.type*truth.value | subid) +
                    (sentence.type*truth.value | item), 
                  data=d)

model.neg <- lmer(rating ~ condition*truth.value +
                         (truth.value | subid) + 
                         (condition*truth.value | item), 
                       data=subset(d, sentence.type=="negative"))

model.true.neg <- lmer(rating ~ condition*neg.concept*neg.syntax +
                         (neg.concept*neg.syntax | subid) + 
                         (neg.concept*neg.syntax | item), 
                       data=subset(d, truth.value=="TRUE" & sentence.type=="negative"))