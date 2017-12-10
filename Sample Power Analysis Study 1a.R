library(plyr)
library(ggplot2)
theme_set(theme_bw())

#expanding my data, where 91 = number of ppts with 39 men and 52 women who saw 100 diff targets in 5 conditions
expdat.me2 <- data.frame(ppt = factor(rep(1:91, each=100)), 
                         condition = rep(c(0,2,4,6,8), 1820),
                         target = factor(rep(1:100, 91)),
                         gender = factor(c(rep(0,3900), rep(1,5200))))

set.seed(101)
nsim <- 20
beta <- c(3.9224359, 0.2528846, 2.5875579, -0.7122019)
#need to find 4th theta value, 
getME("theta", mod3b)
theta <- c(3.92283, 0.08805, 0.09683)


ss <- simulate(~condition*gender + (1+condition | ppt) + (1 | target), nsim = nsim, 
               REML=TRUE, family=NULL, weights = rep(25, nrow(expdat.me2)), 
               newdata = expdat.me2, newparams = list(theta = theta, 
                                                  beta = beta))

expdat$resp <- ss[, 1]
fit1 <- glmer(resp ~ ttt + (1 | indiv) + (1 | obs), family = binomial, weights = rep(25, 
                                                                                     nrow(expdat)), data = expdat)

fit1B <- refit(fit1, ss[[2]])
fitsim <- function(i) {
  coef(summary(refit(fit1, ss[[i]])))["tttvar", ]
}

t1 <- system.time(fitAll <- laply(seq(nsim), function(i) fitsim(i)))
## you can use .progress='text' to get a progress indicator ...
fitAll <- setNames(as.data.frame(fitAll), c("est", "stderr", "zval", "pval"))

head(fitAll)
with(fitAll, mean(pval < 0.05))
ggplot(arrange(fitAll, est), aes(x = seq(nsim), y = est, ymin = est - 1.96 * 
                                   stderr, ymax = est + 1.96 * stderr)) + geom_pointrange() + geom_hline(yintercept = -0.2, 
                                                                                                         colour = "red")