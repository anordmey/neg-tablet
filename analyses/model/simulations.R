##Neg-tablet model simulations

##################### PARAMETERS ##########################
n.pos <- 1 # number of positive features shared by everyone
n.neg <- 10 # number of negative features shared by everyone

#Creates the vocabulary - all possible words that could describe these characters
#1 is the "target" item (e.g. the thing that is negated)
#2 is the alternative/foil item for relevant trials (where foil items are not relevant, 2 becomes just an additional negative feature shared by all)
#Note that 1 & 2 are mutually exclusive -- you can't have both features.
vocab <- c(1:(2+n.pos+n.neg),-(2+n.pos+n.neg):-1)

# create the full set of experiment 1 conditions
# pos vs. neg is whether the target character has the target items (corresponds to true pos and true neg if we only look at true sentences)
# non vs. alt is nonexistence vs. alternative negation type
# target vs none is context vs no context 
exp1.pos.none  <- matrix(c(1, 0, rep(1,n.pos), rep(0,n.neg),
                           0, 0, rep(1,n.pos), rep(0,n.neg),
                           0, 0, rep(1,n.pos), rep(0,n.neg),
                           0, 0, rep(1,n.pos), rep(0,n.neg)), 
                         byrow=TRUE,
                         nrow=4, ncol=2 + n.pos + n.neg)

exp1.pos.target <- exp1.pos.none 
exp1.pos.target[,1] <- c(1,1,1,1)

exp1.neg.non.none  <- exp1.pos.none 
exp1.neg.non.none [,1] <- c(0,0,0,0)

exp1.neg.non.target <- exp1.pos.none 
exp1.neg.non.target[,1] <- c(0,1,1,1)

exp1.neg.alt.none  <- exp1.neg.non.none 
exp1.neg.alt.none[,2] <- c(1,0,0,0)

exp1.neg.alt.target <- exp1.neg.non.target
exp1.neg.alt.target[,2] <- c(1,0,0,0)

# create the full set of experiment 2 conditions
exp2.pos.none <- exp1.pos.none

exp2.pos.foil <- exp1.pos.none
exp2.pos.foil[,2] <- c(0,1,1,1)

exp2.pos.target <- exp1.pos.target

exp2.neg.none <- exp1.neg.alt.none

exp2.neg.foil <- exp1.neg.alt.none
exp2.neg.foil[,2] <- c(1,1,1,1)

exp2.neg.target <- exp1.neg.alt.target
  
#binds all positive contexts into a single list.
contexts <- list(exp1.pos.none, 
                 exp1.pos.target,
                 exp1.neg.non.none,
                 exp1.neg.non.target,
                 exp1.neg.alt.none,
                 exp1.neg.alt.target,
                 exp2.pos.none,
                 exp2.pos.foil,
                 exp2.pos.target,
                 exp2.neg.none,
                 exp2.neg.foil,
                 exp2.neg.target)

names(contexts) <- c("exp1.pos.none", 
                     "exp1.pos.target",
                     "exp1.neg.non.none",
                     "exp1.neg.non.target",
                     "exp1.neg.alt.none",
                     "exp1.neg.alt.target",
                     "exp2.pos.none",
                     "exp2.pos.foil",
                     "exp2.pos.target",
                     "exp2.neg.none",
                     "exp2.neg.foil",
                     "exp2.neg.target")

n <- length(contexts)

####################compare model to data#####################

#############Individual simulation###############
##PARAMETERS:
cost = .8

probs <- data.frame(word=c(rep("positive", n), rep("negative", n)), 
                    truth.value = c("True","True","False","False","False","False","True","True","True","False","False","False","False","False","True","True","True","True","False","False","False","True","True","True"),
                    experiment=c(rep("exp1", 6), rep("exp2", 6), rep("exp1", 6), rep("exp2", 6)), 
                    context=rep(c("none","target","none","target","none","target","none","foil","target","none","foil","target"), 2),
                    negation.type = c("NA","NA","non","non","alt","alt","NA","NA","NA","alt","alt","alt","NA","NA","non","non","alt","alt","NA","NA","NA","alt","alt","alt"),
                    p.word=NA)

#set up variables for loop
words <- c(1,-1) #two possible words: e.g "apples" and "no apples"
count <- 1

  for (w in 1:2) {
    for (context.num in 1:n) {
      
      probs$p.word[count] <- p.word.given.context(word=words[w],
                                                  context=contexts[context.num][[1]],
                                                  cost.per.word=cost)      
      count <- count + 1
      
    }
  }


##Plot probs
#Just look at true negative sentences
probs$context <- factor(probs$context, levels=c("none","foil","target"), labels=c("None","Foil","Target"))
probs$negation.type <- factor(probs$negation.type, levels=c("alt","non"), labels=c("Alternative", "Nonexistence"))
probs$experiment <- factor(probs$experiment, labels=c("Experiment 1", "Experiment 2"))
# qplot(data=subset(probs, truth.value=="True" & word=="negative"), 
#       x=context, y=p.word, fill=negation.type,
#       geom="bar", stat="identity", position="dodge") + 
#   facet_grid(.~experiment, scales="free") +
#   #ggtitle("Model predictions") + 
#   scale_fill_grey("Negation Type") + 
#   xlab("Context") + ylab("Model Probabilities") + 
#   plot.style
