

## ---- rcode001
library(tidyverse)
library(fixest)

## ---- rcode002
genclassroomdata <- function(classShock,seed){
  
  # set seed
  set.seed(seed)
  
  # data size
  N <- 5000
  classrooms <- 50
  Nclassroom <- N/classrooms
  
  # main data
  textbooks <- rep(rbinom(classrooms,1,0.5),each=Nclassroom) # textbook allocation
  classshock <- rep(rnorm(classrooms,mean=0,sd=classShock),each=Nclassroom) # classroom-specific shock
  y <- 5 + 0*textbooks + classshock + rnorm(N,mean=0,sd=1) # outcome variable  
  
  # return tibble
  df <- tibble(
    i = seq(1,N),
    classroom = rep(seq(1:classrooms),each=Nclassroom),
    textbooks = textbooks,
    y = y
  )
  
  return(df)
  
}

## ---- rcode002b
data <- genclassroomdata(classShock=0,seed=1)
head(data)

## ---- rcode003
reg01 <- feols(y~textbooks,data=data)
etable(reg01)

## ---- rcode004
data <- genclassroomdata(classShock=1,seed=1)
reg02 <- feols(y~textbooks,data=data)
etable(reg02)

## ---- rcode005
runtextbookeval<- function(classShock,seed){
  
  # generate data
  data <- genclassroomdata(classShock=classShock,seed=seed)
  
  # run regression
  fit <- feols(y~textbooks,data=data)
  fit <- summary(fit)
  
  # save and return the coefficients and t-stat
  coef <- c(fit$coeftable[1],fit$coeftable[1,3],fit$coeftable[2,1],fit$coeftable[2,3])
  return(coef)
  
}

## ---- rcode006
B <- 1000 # number of interations
results <- sapply(1:B,function(i) runtextbookeval(classShock=0,seed=i))
r <- tibble(
  intercept = results[1,],
  intercept_tstat = results[2,],
  textbooks = results[3,],
  textbooks_tstat = results[4,]
)
head(r)

## ---- rcode007
r %>% ggplot(aes(intercept)) + geom_density(colour="black",alpha=0.1,fill="black") + geom_vline(xintercept=5,colour="black",linetype="dashed") + xlab("Coefficients") + ylab("") + theme_bw()

## ---- rcode008
r %>% ggplot(aes(textbooks)) + geom_density(colour="black",alpha=0.1,fill="black") + geom_vline(xintercept=0,colour="black",linetype="dashed") + xlab("Coefficients") + ylab("") + theme_bw()

## ---- rcode009
r %>% ggplot(aes(abs(textbooks_tstat))) + geom_density(colour="red",alpha=0.1,fill="red") + geom_vline(xintercept=1.96,colour="black",linetype="dashed") + annotate("text",x=3,y=0.6,label=paste("Area: ",mean(abs(r$textbooks_tstat)>1.96))) + xlab("Absolute value of t-stat") + ylab("") + theme_bw()

## ---- rcode010
B <- 1000 # number of interations
results <- sapply(1:B,function(i) runtextbookeval(classShock=1,seed=i))
r <- tibble(
  intercept = results[1,],
  intercept_tstat = results[2,],
  textbooks = results[3,],
  textbooks_tstats = results[4,]
)
head(r)

## ---- rcode011
r %>% ggplot(aes(textbooks)) + geom_density(colour="black",alpha=0.1,fill="black") + geom_vline(xintercept=0,colour="black",linetype="dashed") + xlab("Coefficients") + ylab("") + theme_bw()

## ---- rcode012
r %>% ggplot(aes(abs(textbooks_tstats))) + geom_density(colour="red",alpha=0.1,fill="red") + geom_vline(xintercept=1.96,colour="black",linetype="dashed") + annotate("text",x=10,y=0.1,label=paste("Area: ",mean(abs(r$textbooks_tstats)>1.96))) + xlab("Absolute value of t-stat") + ylab("") + theme_bw()

## ---- rcode013
reg03 <- feols(y~textbooks,data=data)
etable(reg03)

## ---- rcode014
reg04 <- feols(y~textbooks,data=data,vcov=cluster~classroom)
etable(reg04)

## ---- rcode015
runtextbookeval2 <- function(i){
  
  # generate data
  data <- genclassroomdata(classShock=1,i)
  
  # run regression - iid standard errors
  fit1 <- feols(y~textbooks,data=data)
  fit1 <- summary(fit1)
  
  # run regression - heteroskedastic
  fit2 <- feols(y~textbooks,data=data,vcov="hetero")
  fit2 <- summary(fit2)
  
  # run regression - clustered
  fit3 <- feols(y~textbooks,data=data,vcov=cluster~classroom)
  fit3 <- summary(fit3)
  
  # save and return the coefficients and t-stat
  coef <- c(fit1$coeftable[2,3],fit2$coeftable[2,3],fit3$coeftable[2,3])
  return(coef)
  
}

## ---- rcode016
B <- 1000 # number of simulations
results <- sapply(1:B,runtextbookeval2)
r <- tibble(
  tstat_iid = results[1,],
  tstat_hetero = results[2,],
  tstat_cluster = results[3,]
)
head(r)

## ---- rcode017
r %>% ggplot(aes(abs(tstat_iid))) + geom_density(colour="red",alpha=0.1,fill="red") + geom_vline(xintercept=1.96,colour="black",linetype="dashed") + annotate("text",x=10,y=0.1,label=paste("Area: ",mean(abs(r$tstat_iid)>1.96))) + xlab("Absolute value of t-stat") + ylab("") + theme_bw()

## ---- rcode018
r %>% ggplot(aes(abs(tstat_hetero))) + geom_density(colour="red",alpha=0.1,fill="red") + geom_vline(xintercept=1.96,colour="black",linetype="dashed") + annotate("text",x=10,y=0.1,label=paste("Area: ",mean(abs(r$tstat_hetero)>1.96))) + xlab("Absolute value of t-stat") + ylab("") + theme_bw()

## ---- rcode019
r %>% ggplot(aes(abs(tstat_cluster))) + geom_density(colour="red",alpha=0.1,fill="red") + geom_vline(xintercept=1.96,colour="black",linetype="dashed") + annotate("text",x=3,y=0.6,label=paste("Area: ",mean(abs(r$tstat_cluster)>1.96))) + xlab("Absolute value of t-stat") + ylab("") + theme_bw()



