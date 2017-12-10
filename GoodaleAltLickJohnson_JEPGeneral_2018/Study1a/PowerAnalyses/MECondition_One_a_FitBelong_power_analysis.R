
##cross classified

library(MASS)
library(lme4)

set.seed(12345)
b0 <- 5.8916
b_cond <- -0.154
b_gender <- -0.8587
rand_b0 <- 6.837 #kkk
rand_cond <- 0.2125 #jjjj
rand_covar <- -1.0382 #llll
rand_stim <- 0.09677 #xxxxx
resvar <- 1.8185
samples <- 10000
#adjust these to get sample size needed for power X%
num_subjects <- 91
npersubject <- 20

#total obs for each sampled dataset
length <- num_subjects*npersubject

#s <- matrix(c(kkkk,lllll,llll,jjjj),2,2)

s <- matrix(c(6.837, -1.0382, -1.0382, 0.2125),2,2)

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
    (b_gender)*r$gender + r$residual
  r$id <- as.factor(r$id)
  r$stim<- as.factor(r$stim)
  #full model
  m1 <- lmer(y ~ gender+cond + (1+cond|id) +(1|stim),data=r, REML=T)
  #get coefficient
  estimate[i] <- fixef(m1)["cond"]
  #reduced model wihtout interaction
  m2 <- update(m1, . ~ .-cond, REML=F)
  #store p-value
  p[i] <- anova(m1,m2)$`Pr(>Chisq)`[2]
}

#call significance
sig <- p<0.05
#proportion significant is
mean(sig)
paste0("Power is ", mean(sig))
