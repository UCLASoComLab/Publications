
##cross classified

library(MASS)
library(lme4)

set.seed(12345)
b0 <- 2.6974
b_cond <- 0.1476
b_gender <- -0.03433
b_int <- 0.002804
rand_b0 <- 0.6090 #kkk
rand_cond <- 0.01624 #jjjj
rand_covar <- -0.05148 #llll
rand_stim <- 0.02457 #xxxxx
resvar <- 0.3579
samples <- 10000
#adjust these to get sample size needed for power X%
num_subjects <- 91
npersubject <- 20

#total obs for each sampled dataset
length <- num_subjects*npersubject

#s <- matrix(c(kkkk,lllll,llll,jjjj),2,2)
s <- matrix(c(0.609,-0.05148,-0.05148,0.01624),2,2)

#estimate holds the coefficients
estimate <- rep(0, samples)
#p holds the p-values
p <- rep(0, samples)
for (i in 1:samples) {
  #get random effects for each subject
  re <- mvrnorm(91,mu=c(0,0), Sigma=s)
  #expand to 20 rows per subjects
  r <- data.frame(id = rep(1:num_subjects, each=npersubject), 
                  time = rep(1:npersubject, times=num_subjects), 
                  sub_int = rep(re[,1], each=npersubject), 
                  sub_cond=rep(re[,2], each=npersubject),
                  stim = c(rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20), 91)))
  #52 females and 39 males
  r$gender = r$id <= 52
  #cond values of 0, 2, 4, 6, 8, each repeated 4 times
  r$cond <- c(rep(c(0, 0, 0, 0, 2, 2, 2, 2, 4, 4, 4, 4, 6, 6, 6, 6, 8, 8, 8, 8), 91))
  #get residuals
  r$residual <- rnorm(length)*sqrt(resvar)
  r$stim_resid <- rnorm(length)*sqrt(rand_stim)
  #y is the outcome
  r$y <- (b0+r$sub_int+r$stim_resid) + (b_cond+r$sub_cond)*r$cond + 
    (b_gender)*r$gender + (b_int)*r$cond*r$gender + r$residual
  r$id <- as.factor(r$id)
  r$stim<- as.factor(r$stim)
  #full model
  m1 <- lmer(y ~ gender*cond + (1+cond|id) +(1|stim),data=r, REML=T)
  #get coefficient
  estimate[i] <- fixef(m1)["genderTRUE:cond"]
  #reduced model wihtout interaction
  m2 <- update(m1, . ~ .-gender:cond, REML=F)
  #store p-value
  p[i] <- anova(m1,m2)$`Pr(>Chisq)`[2]
}

#call significance
sig <- p<0.05
#proportion significant is
mean(sig)
paste0("Power is ", mean(sig))