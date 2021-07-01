#! Add college data to df, update ndf2, rerun

library(ggplot2)

df <- read.csv("PriorPosterior.csv")
df <- df[-c(17),] #!!
View(df)

#Extract NFL completion percentages (not including average CP)
nfl <- df[,c(6:22)]
View(nfl)

#Function for taking rows out of dataframe
selectRow <- function(dafr,ro){
  return(as.numeric(dafr[ro,]))
}

#Rank players halfway through season
rank <- subset(ndf, select = c("names","half"))
rank <- rank[order("half"),]

#Putting data together
vecs <- c()
for (e in c(1:16,18:30)){
  vecs <- append(vecs,list(na.omit(as.numeric(nfl[e,]))))
}
vecs
df$vecs <- vecs

df$len <- lengths(df$vecs)

plot(df$College,df$guess2,main="EOS Guess Compl % vs. College Compl %",xlab="College Mean",ylab="End-of-Season Prediction")

#Graph College Compl% against EOS guess
library("tidyverse")
df %>%
  ggplot(aes(College,guess2,colour = College.SD))+
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = glm, se = T)+
  labs(x = "College Compl %", y = "EOS Guess", title = "EOS Guess Compl % vs. College Compl %")
  theme_minimal()
  
#Graph Half-Season guess against end-of-season completion
ndf %>%
  ggplot(aes(half,eos))+
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = glm, se = T)+
  labs(x = "Half-Season Guess", y = "EOS Guess", title = "EOS Guess Compl % vs. Half Season Guess")
  theme_minimal()  
  
ndf$halfError <- (ndf$half-ndf$CP.)^2
ndf$tfError <- (ndf$tf-ndf$CP.)^2
ndf$startError <- (ndf$College-ndf$CP.)^2
sqrt(sum(ndf$startError))
sqrt(sum(ndf$halfError))
sqrt(sum(ndf$tfError))
  
#Distances
df$halfError <- (df$halfGuess-df$CP.)^2
df$tfError <- (df$threefourthguess-df$CP.)^2
df$startError <- (df$College-df$CP.)^2
sqrt(sum(df$halfError))
sqrt(sum(df$tfError))
sqrt(sum(df$startError))

#Extract Guesses
df <- subset(df, select = c("College","CP.","Players"))
ndf <- subset(df,select=c("College","CP."))
ndf$collegeCP <- as.numeric(ndf$College)
ndf$half <- as.numeric(halfGuess)
ndf$tf <- as.numeric(guess34)
ndf$eos <- as.numeric(guess)
ndf$names <- df$Players

#Invert rows and columns
ndf2 <- data.frame(t(ndf[-1]))
colnames(ndf2) <- df$Players
ndf2 <- ndf2[-c(6),]
ndf2$order <- c(17,1,8,12,16)
ndf2$`Justin Herbert`
#GGplot into function (lapply later)
ggplot(ndf2,mapping=aes(order,`Joe Burrow `))
ggplot(ndf2,mapping=aes(order,`Justin Herbert`))

ndf2

ggplot(ndf2,aes(x=order))+
  xlim(min=0,max=17)+
  geom_line(aes(y=as.numeric(as.character(`Joe Burrow `))))

ggplot(ndf2,aes(x=as.numeric(order)))+
  xlim(min=0,max=17)+
  ylim(min=60,max=70)+
  geom_line(aes(y=as.numeric(as.character(`Joe Burrow `)), color = "orange"))+
  geom_line(aes(y=as.numeric(as.character(`Justin Herbert`)),color="yellow"))+
  geom_line(aes(y=as.numeric(as.character(`Tua Tagovailoa`)),color="blue"))+
  geom_line(aes(y=as.numeric(as.character(`Kyler Murray`)),color="red"))+
  geom_line(aes(y=as.numeric(as.character(`Daniel Jones`)),color="purple"))+
  labs(title="Predicted Completion Percentage Throughout Season",color="Player")+
  scale_color_manual(labels=c("Joe Burrow","Justin Herbert","Tua Tagavailoa","Kyler Murray","Daniel Jones"),
                              values=c("orange","yellow","blue","red","purple"))


#Take first half of every vector
allHalfs <- list()
for (e in c(1:16,18:29)){
  vec <- unlist(df$vecs[e])
  l <- df$len[e]
  print(vec[1:l/2])
  #allHalfs <- append(allHalfs,as.numeric(vec[1:(l/2)]))
  allHalfs[[e]] <- as.numeric(vec[1:(l/2)])
}
df$halfs <- allHalfs

#Guess based on first half of data
halfGuess <- c()
for (e in c(1:29)){
  vecs <- df$halfs[e]
  m <- df$College[e]
  sd <- df$College.SD[e]
  halfGuess <- append(halfGuess,bEst(vecs,m,sd))
}

df$halfGuess <- halfGuess

#Take first 3/4 half of every vector
threefourth <- list()
for (e in c(1:17,18:29)){
  vec <- unlist(df$vecs[e])
  l <- df$len[e]
  #print(vec[1:l*3/4])
  #allHalfs <- append(allHalfs,as.numeric(vec[1:(l/2)]))
  threefourth[[e]] <- as.numeric(vec[1:(l*3/4)])
}
df$threefourth <- threefourth

#Guess based on first three fourths of data
guess34 <- c()
for (e in c(1:29)){
  vecs <- df$threefourth[e]
  m <- df$College[e]
  sd <- df$College.SD[e]
  guess34 <- append(guess34,bEst(vecs,m,sd))
}
df$threefourthguess <- guess34

#Guess based on entire season
df$bGuess <- bEst(df$vecs,df$College,df$College.SD)
guess <- c()
for (e in c(1:17,18:29)){
  vecs <- df$vecs[e]
  m <- df$College[e]
  sd <- df$College.SD[e]
  guess <- append(guess,bEst(vecs,m,sd))
}
df$guess2 <- guess

#Bayesian Estimation Function
bEst <- function(vec,m,sd){
  seq <- c(0:(2*m))
  posterior <- c()
  optimum <- 0
  for (val in seq){
    priorProb <- prob_dens(val,m,sd)
    likelihood <- prob(vec,val,sd)
    posterior <- append(posterior, priorProb*likelihood)
    if (priorProb*likelihood >= max(posterior)){
      optimum <- val
    }
  }
  #return(data.frame(seq,posterior))
  return(optimum)
}

print("Maximum likelihood free-throw percentage: ")
print(bEst(c(100,100,100),60,15))

#Probability Density Function (Two-Tailed)
prob_dens <- function(a,m,sd){
  return(dnorm(a,m,sd))
}
  
#Probability of a Series of Data [Function]
prob <- function(vec,pm,psd){
  return(prod(sapply(vec,prob_dens,pm,psd)))
}

