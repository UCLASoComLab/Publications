library(MASS)
library(lme4)

set.seed(12345)
b0 <- 6.4662
b_cond <- 0.2612
b_gender <- -0.3873
b_int <- -0.1135
rand_b0 <- 2.3247
rand_cond <- 0.08546
rand_covar <- 0.03676
resvar <- 9.5698
samples <- 10
#adjust these to get sample size needed for power X%
num_subjects <- 75
npersubject <- 50

#total obs for each sampled dataset
length <- num_subjects*npersubject

s <- matrix(c(2.3247,0.03676,0.03676,0.08546),2,2)

#estimate holds the coefficients
estimate <- rep(0, samples)
#p holds the p-values
p <- rep(0, samples)
for (i in 1:samples) {
  #get random effects for each subject
  re <- mvrnorm(75,mu=c(0,0), Sigma=s)
  #expand to 50 rows per subjects
  r <- data.frame(id = rep(1:num_subjects, each=npersubject), 
                  time = rep(1:npersubject, times=num_subjects), 
                  sub_int = rep(re[,1], each=npersubject), 
                  sub_cond=rep(re[,2], each=npersubject))
  #56 females and 19 males
  r$gender = r$id <= 56
  #cond values of 0, 3, 6, 9, 12
  #r$cond <- floor(r$time/10)*3
  r$cond <- c(rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
                    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 
                    9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
                    12, 12, 12, 12, 12, 12, 12, 12, 12, 12), 75))
  #get residuals
  r$residual <- rnorm(length)*sqrt(resvar)
  #y is the outcome
  r$y <- (b0+r$sub_int) + (b_cond+r$sub_cond)*r$cond + 
    (b_gender)*r$gender + (b_int)*r$cond*r$gender + r$residual
  #for the cross-classified model something like this:
  #r$y <- (b0+r$random1 + r$random2) + (b_cond+)*r$cond + 
   # (b_gender)*r$gender + (b_int)*r$cond*r$gender + r$residual
  #full model
  m1 <- lmer(y ~ gender*cond + (1+cond|id),data=r, REML=F)
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

