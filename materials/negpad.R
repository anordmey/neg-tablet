##AEN Negpad felicity
rm(list=ls())
#Load libraries
library(reshape2)
library(dplyr)
library(ggplot2)

#functions
## add some style elements for ggplot2
plot.style <- theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), legend.position="right", axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

## number of unique subs
n.unique <- function (x) {
  length(unique(x))
}

## for bootstrapping 95% confidence intervals
theta <- function(x,xdata) {mean(xdata[x])}
ci.low <- function(x) {
  quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
  quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.975)}


#Load in data
all.data <- read.csv("negpad_long_data.csv")

##Condense scale (1=bad, 2=neutral, 3=good)
all.data$resp2 <- 2
all.data[all.data$resp > 3,]$resp2 <- 3
all.data[all.data$resp < 3,]$resp2 <- 1
all.data$resp2 <- factor(all.data$resp2)

#reject subjects who don't understand scale (based on positive sentences)
reject <- all.data %>%
  filter(sent.type=="positive") %>% #Only look at positive sentences
  group_by(subid) %>%
  mutate(total = n()) %>% #get total # of positive sentences child saw
  group_by(subid, condition, truth, total, resp2) %>%
  filter((truth=="True" & resp2==3) | (truth=="False" & resp2==1)) %>% #Get # "good" for true pos and "bad" for false pos
  summarize(counts = n()) %>% 
  group_by(subid, condition, total) %>%
  summarize(counts = sum(counts)) %>% #total # "correct" responses
  mutate(prop = counts/total) %>% #proportion correct
  filter(prop < .6) #reject kids who got < .6 "correct" (this allows for 2/6 "mistakes")

for (i in reject$subid) {
  all.data <- filter(all.data, subid !=i)
}

#Make sure there aren't any kids who just used one side of scale.
scaleUse <- aggregate(resp2 ~ subid, all.data, n.unique)
table(scaleUse$resp2) #Are any resp2=1

#now reject kids who only chose a single data point

##Categorize kids based on response type
tn_responses <- all.data %>%
  filter(sent.type=="negative" & truth=="True") %>%
  group_by(subid) %>%
  mutate(total = n()) %>%
  group_by(subid, condition, total, resp2) %>%
  summarize(counts = n()) %>%
  mutate(prop = counts/total)

category <- dcast(tn_responses, subid + condition ~ resp2)
names(category) <- c("subid","condition","bad","neutral","good")
category[is.na(category)] <- 0

category$type <- "other"
#category[category$neutral > .6,]$type <- "tn_neutral"
category[category$bad > .6,]$type <- "tn_bad"
category[category$good > .6,]$type <- "tn_good"

cat_counts <- category %>%
  group_by(condition, type) %>%
  summarise(counts = n())
cat_counts$condition <- factor(cat_counts$condition, levels=c("none","target"), labels=c("None","Target"))
cat_counts$type <- factor(cat_counts$type, levels=c("tn_bad","tn_good","other"), labels=c("True Negatives = Bad", "True Negatives = Good", "Inconsistent/Other"))

qplot(data=cat_counts, x=condition, y=counts, fill=type, 
      stat="identity", position="dodge", geom="bar") + 
  scale_fill_hue("Response Type") +
  ylab("Count") + xlab("Context Condition") +
  plot.style

##Plot data
ms <- all.data %>%
  group_by(subid, condition, sent.type, truth) %>%
  summarise(subm = mean(resp)) %>%
  group_by(condition, sent.type, truth) %>%
  summarise(m = mean(subm),
            cih = ci.high(subm),
            cil = ci.low(subm))
ms$condition <- factor(ms$condition, labels=c("None","Target"))
ms$truth <- factor(ms$truth, levels=c("True","False"))

qplot(data=subset(ms, sent.type=="negative"), 
      x=condition, y=m, facets=~truth,
      stat="identity", position="dodge", geom="bar") +
  geom_errorbar(aes(ymin=cil, ymax=cih), 
                position=position_dodge(.9), width=0) + 
  scale_fill_grey("") +
  xlab("Context") + ylab("Response") +
  scale_y_continuous(limits=c(0, 5), breaks=seq(1,5,1)) +
  #coord_equal(1/1.5) +
  plot.style


#histogram of responses
truenegs <- filter(ms, truth=="True" & sent.type == "negative")

#make df for histogram (for formatting reasons)
hist_data <- all.data %>%
  filter(truth=="True" & sent.type=="negative") %>%
  group_by(condition, resp) %>%
  summarise(count = n())
hist_data$condition <- factor(hist_data$condition, labels=c("None","Target"))


qplot(data=hist_data, y=count, x=resp, 
      fill = condition, width=.5, 
      geom="bar", position = position_dodge(.6), stat="identity") +
  geom_point(data=truenegs, aes(x=m, y=c(41, 42), color=condition)) +
  geom_segment(data=truenegs, aes(x=cil, xend=cih, y=c(41, 42), yend=c(41, 42), color=condition)) + 
  scale_fill_grey("Condition") + scale_color_grey("Condition") +
  xlab("Response") + ylab("Count") +
  #ggtitle("True Negatives, 3-5-year-olds (N=43)") +
  plot.style




#######BREAK DOWN BY AGE################
##Plot data
ms <- all.data %>%
  group_by(subid, agegroup, condition, sent.type, truth) %>%
  summarise(subm = mean(resp)) %>%
  group_by(condition, agegroup, sent.type, truth) %>%
  summarise(m = mean(subm),
            cih = ci.high(subm),
            cil = ci.low(subm))
ms$condition <- factor(ms$condition, labels=c("None","Target"))
ms$truth <- factor(ms$truth, levels=c("True","False"))

qplot(data=subset(ms, sent.type=="negative"), 
      x=condition, y=m, facets=agegroup~truth,
      stat="identity", position="dodge", geom="bar") +
  geom_errorbar(aes(ymin=cil, ymax=cih), 
                position=position_dodge(.9), width=0) + 
  scale_fill_grey("") +
  xlab("Context") + ylab("Response") +
  scale_y_continuous(limits=c(0, 5), breaks=seq(1,5,1)) +
  #coord_equal(1/1.5) +
  plot.style

#histogram of responses
trueneg_3s <- filter(ms, truth=="True" & sent.type == "negative" & agegroup == "3")
trueneg_4s <- filter(ms, truth=="True" & sent.type == "negative" & agegroup == "4")

hist_data_3s <- all.data %>%
  filter(truth=="True" & sent.type=="negative" & agegroup=="3") %>%
  group_by(condition, resp) %>%
  summarise(count = n())
hist_data_3s$condition <- factor(hist_data_3s$condition, labels=c("None","Target"))

#quartz()
qplot(data=hist_data_3s, y=count, x=resp, 
      fill = condition, width=.5, 
      geom="bar", position = position_dodge(.6), stat="identity") +
  geom_point(data=trueneg_3s, aes(x=m, y=c(40, 41), color=condition)) +
  geom_segment(data=trueneg_3s, aes(x=cil, xend=cih, y=c(40, 41), yend=c(40, 41), color=condition)) + 
  scale_fill_grey("Condition") + scale_color_grey("Condition") +
  xlab("Response") + ylab("Count") +
  ylim(c(0, 80)) + 
  #ggtitle("True Negatives, 3-year-olds (N=35)") +
  plot.style



hist_data_4s <- all.data %>%
  filter(truth=="True" & sent.type=="negative" & agegroup=="4") %>%
  group_by(condition, resp) %>%
  summarise(count = n())
hist_data_4s$condition <- factor(hist_data_4s$condition, labels=c("None","Target"))

#quartz()
qplot(data=hist_data_4s, y=count, x=resp, 
      fill = condition, width=.5, 
      geom="bar", position = position_dodge(.6), stat="identity") +
  geom_point(data=trueneg_4s, aes(x=m, y=c(40, 41), color=condition)) +
  geom_segment(data=trueneg_4s, aes(x=cil, xend=cih, y=c(40, 41), yend=c(40, 41), color=condition)) + 
  scale_fill_grey("Condition") + scale_color_grey("Condition") +
  xlab("Response") + ylab("Count") +
  ylim(c(0, 80)) + 
  #ggtitle("True Negatives, 4-year-olds (N=34)") +
  plot.style


## playing around
ms <- all.data %>%
  group_by(sent.type, truth, condition, agegroup, subid) %>%
  summarise(resp = mean(resp)) %>%
  group_by(sent.type, truth, condition, agegroup) %>%
  summarise(cih = ci.high(resp),
            cil = ci.low(resp),
            m = mean(resp)) 

ggplot(ms, aes(x = sent.type:truth, y = m, fill = condition)) + 
  geom_bar(stat="identity", position = "dodge") + 
  geom_linerange(aes(ymin = cil, ymax = cih), 
                 position = position_dodge(width = .9)) +
  facet_grid(.~agegroup)

summary(lmer(resp ~ condition * agegroup * truth  + (1|subid) 
     + (1|item), 
     data = filter(all.data, sent.type == "negative")))

summary(lmer(resp ~ condition * agegroup + 
               (1|subid) + (1|item), 
             data = filter(all.data, sent.type == "negative" & truth == "True")))





## DISCRETE 
all.data$bin.resp <- all.data$resp > 3

ms <- all.data %>%
  group_by(sent.type, truth, condition, agegroup, subid) %>%
  summarise(resp = mean(bin.resp)) %>%
  group_by(sent.type, truth, condition, agegroup) %>%
  summarise(cih = ci.high(resp),
            cil = ci.low(resp),
            m = mean(resp)) 

ggplot(ms, aes(x = sent.type:truth, y = m, fill = condition)) + 
  geom_bar(stat="identity", position = "dodge") + 
  geom_linerange(aes(ymin = cil, ymax = cih), 
                 position = position_dodge(width = .9)) +
  facet_grid(.~agegroup)

summary(glmer(bin.resp ~ (condition + agegroup + truth)^3  + (1|subid) 
             + (1|item), family = "binomial",
             data = filter(all.data, sent.type == "negative")))


### subjectwise mean distribution
ms <- all.data %>%
  group_by(sent.type, truth, condition, agegroup, subid) %>%
  summarise(m = mean(resp))
  
qplot(round(m),
      fill = condition, 
      facets = ~ agegroup,
      position = "dodge", 
      binwidth = .5,
      data = filter(ms, truth == "True" & 
                    sent.type == "negative"))


#Statistics
model <- lmer(resp ~ condition*agegroup +
                (1 | subid) + 
                (1 | item), 
              data=subset(all.data, sent.type == "negative" & truth == "True"))
summary(model)

threes <- filter(all.data, agegroup == "3")
threes_subs <- aggregate(resp ~ subid + condition, threes, mean)
t.test(resp ~ condition, threes_subs)

fours <- filter(all.data, agegroup == "4")
fours_subs <- aggregate(resp ~ subid + condition, fours, mean)
t.test(resp ~ condition, fours_subs)

subs <- aggregate(resp ~ subid + condition, all.data, mean)
t.test(resp ~ condition, subs)
