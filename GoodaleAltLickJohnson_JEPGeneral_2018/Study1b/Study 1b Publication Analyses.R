library(foreign)
library(car)
library(plyr)
library(tidyr)
library(lme4)
library(cowplot)
library(ggplot2)

#need to run multilevel_effect_size_function.R first in order to get effect sizes in code below#
#Read in Data
masterpptlist <- read.csv("~/repos/Publications/GoodaleAltLickJohnson_JEPGeneral_2018/Study1b/Goodale_Data_Study1b.csv",
           header=TRUE)
masterpptlist <- as.data.frame(masterpptlist)

#recoding gender#
masterpptlist$Gender <- factor(masterpptlist$Gender)
masterpptlist$Gender <- recode(masterpptlist$Gender, " '1' = 'Man'; '2' = 'Woman'")
masterpptlist$Gender <- recode(masterpptlist$Gender, " 'Man' = '0'; 'Woman' = '1'")

names <- c("WomenContributeNorm", "NotMakeFunNorm", "ExcludeNorm", "FriendlyNorm", 
           "GoodatMathNorm", "TreatDifferentlyNorm", "DefertoMenNorm", "AskHelpNorm", "Fit", "Belonging", "Condition")
masterpptlist[names] <-sapply(masterpptlist[names], as.numeric)
sapply(masterpptlist, class)
masterpptlist$Participant <- as.factor(masterpptlist$Participant)

#remove participant who non-binary ID, Gender = 3#
masterpptlist <- subset(masterpptlist, masterpptlist$Gender != 3)

#demographics#
variables_want <- c("Participant", "Gender")
demo_subset <- masterpptlist[,variables_want]
demo_subset <- unique(demo_subset)
count(demo_subset$Gender)
56/75 #75% of sample are women

#FitBelong combined as DV#
masterpptlist$FitBelong <- (masterpptlist$Belong + masterpptlist$Fit)/2
class(masterpptlist$FitBelong)

#Reversing coding so can see simple slopes for women as comparison group#
masterpptlist$RCGender<- recode(masterpptlist$Gender, "'1'= 'W'; '0' = 'M'")
masterpptlist$RCGender <- recode(masterpptlist$RCGender, "'W'= '0'; 'M' = '1'")

#Creating Norms Analsis#
masterpptlist$RCWomenContribute <- 8 - masterpptlist$WomenContributeNorm
masterpptlist$RCNotMakeFun <- 8 - masterpptlist$NotMakeFunNorm
masterpptlist$RCFriendly <- 8 - masterpptlist$FriendlyNorm
masterpptlist$RCGoodMath <- 8 - masterpptlist$GoodatMathNorm
masterpptlist$RCHelp <- 8 - masterpptlist$AskHelpNorm

names2 <- c("RCWomenContribute", "RCNotMakeFun", "RCFriendly", "RCGoodMath", "RCHelp") 
masterpptlist[names2] <- sapply(masterpptlist[names2], as.numeric)
sapply(masterpptlist, class)

masterpptlist$NormAvg <- (masterpptlist$RCWomenContribute + masterpptlist$RCFriendly + masterpptlist$RCNotMakeFun +
                            masterpptlist$RCGoodMath + masterpptlist$RCHelp + masterpptlist$TreatDifferentlyNorm+
                            masterpptlist$DefertoMenNorm + masterpptlist$ExcludeNorm)/8

#Data analysis#
#subsetting data for simple effects analyses#
men.data <- data.frame(subset(masterpptlist, Gender == 0))
women.data <- data.frame(subset(masterpptlist, Gender == 1))

#Perceived Sex Ratio of the group#
mod1 <- lmer(Ratio ~ 1 + (1|Participant), data=masterpptlist)
summary(mod1)

#Calculating ICC#
# look at random effects of summary output
# calculate ICC by summing everything in variance column & dividing participant variance by it
0.00/(10.43)

mod3b <- lmer(Ratio~Condition*Gender + (Condition|Participant), data=masterpptlist, control=lmerControl(optimizer = "Nelder_Mead"))
summary(mod3b)

multilevel.effect.size(mod3b)

mod3.new <- with(masterpptlist, data.frame(Gender = factor(c(rep(0,5),rep(1,5))), Condition = c(0,3,6,9,12,0,3,6,9,12)))
mod3.new$Ratio <- NULL
mod3.new$Ratio <- 0
mm <- model.matrix(terms(mod3b),mod3.new)
mod3.new$Ratio <- predict(mod3b,mod3.new,re.form=NA)

## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(mod3b),mm))
tvar1 <- pvar1 + VarCorr(mod3b)$Participant[1]  ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
mod3.new <- data.frame(
  mod3.new
  , plo = mod3.new$Ratio - cmult*sqrt(pvar1)
  , phi = mod3.new$Ratio+cmult*sqrt(pvar1)
  , tlo = mod3.new$Ratio-cmult*sqrt(tvar1)
  , thi = mod3.new$Ratio+cmult*sqrt(tvar1)
)
#plot confidence intervals
g0 <- ggplot(mod3.new, aes(x=Condition, y=Ratio, colour=Gender))
g0 + geom_ribbon(aes(ymin = plo, ymax = phi, linetype=NA), alpha=0.2, show.legend=FALSE)+
  labs(x="Actual Number of Men in Ensemble", y="Perceived Number of Men")+
  theme_cowplot()+ geom_line(size=1)+
  scale_colour_discrete(name = "Perceiver Sex",
                        breaks=c(0, 1),
                        labels = c("Male", "Female"))+
  scale_shape_discrete(name="Perceiver Sex",
                       breaks=c(0, 1),
                       labels = c("Male", "Female"))+
  scale_y_continuous(name="Perceived Number of Men", breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))+
  scale_x_continuous(breaks=c(0, 3, 6, 9, 12))+
  coord_cartesian(ylim= c(1, 12), xlim = c(0, 12))+
  theme(legend.justification=c(1,0.1), legend.position=c(1,0.1), legend.text=element_text(size=16), 
        legend.title=element_text(size=20), axis.title=element_text(size=20), axis.text=element_text(size=16))

#Perceived Facial Masculinity#
mod1 <- lmer(AverageFace ~ 1 + (1|Participant), data=masterpptlist)
summary(mod1)

#Calculating ICC#
# look at random effects of summary output
# calculate ICC by summing everything in variance column & dividing participant variance by it
5.956/(5.956+11.743)

mod3b <- lmer(AverageFace~Condition*Gender + (1+Condition|Participant), data=masterpptlist)
summary(mod3b)
multilevel.effect.size(mod3b)

mod3.new <- with(masterpptlist, data.frame(Gender = factor(c(rep(0,5),rep(1,5))), Condition = c(0,3,6,9,12,0,3,6,9,12)))
mod3.new$AverageFace <- NULL
mod3.new$AverageFace <- 0
mm <- model.matrix(terms(mod3b),mod3.new)
mod3.new$AverageFace <- predict(mod3b,mod3.new,re.form=NA)

## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(mod3b),mm))
tvar1 <- pvar1 + VarCorr(mod3b)$Participant[1]  ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
mod3.new <- data.frame(
  mod3.new
  , plo = mod3.new$AverageFace - cmult*sqrt(pvar1)
  , phi = mod3.new$AverageFace+cmult*sqrt(pvar1)
  , tlo = mod3.new$AverageFace-cmult*sqrt(tvar1)
  , thi = mod3.new$AverageFace+cmult*sqrt(tvar1)
)
#plot confidence intervals
g0 <- ggplot(mod3.new, aes(x=Condition, y=AverageFace, colour=Gender))
g0 + geom_ribbon(aes(ymin = plo, ymax = phi, linetype=NA,), alpha=0.2, show.legend=FALSE)+
  labs(x="Actual Number of Men in Ensemble", y="Perceived Facial Masculinity")+
  theme_cowplot()+ geom_line(size=1)+
  scale_colour_discrete(name = "Perceiver Sex",
                        breaks=c(0, 1),
                        labels = c("Male", "Female"))+
  scale_shape_discrete(name="Perceiver Sex",
                       breaks=c(0, 1),
                       labels = c("Male", "Female"))+
  scale_x_continuous(breaks=c(0, 3, 6, 9, 12))+
  coord_cartesian(ylim= c(1, 21), xlim = c(0, 12))+
  theme(legend.justification=c(1,0.1), legend.position=c(1,0.1), legend.text=element_text(size=16), 
        legend.title=element_text(size=20), axis.title=element_text(size=20), axis.text=element_text(size=16))

#Data analysis for Social Attitudes & Affordances#
#Perceived Sexist Norms#
mod1 <- lmer(NormAvg ~ 1 + (1|Participant), data=masterpptlist)
summary(mod1)

#Calculating ICC#
# look at random effects of summary output
# calculate ICC by summing everything in variance column & dividing participant variance by it
0.247/(0.247+0.7622)

mod3b <- lmer(NormAvg~Condition*Gender + (1+Condition|Participant), data=masterpptlist)
summary(mod3b)
multilevel.effect.size(mod3b)

#Sig difference, so need to use random slopes, random effects#
mod3.new <- with(masterpptlist, data.frame(Gender = factor(c(rep(0,5),rep(1,5))), Condition = c(0,3,6,9,12,0,3,6,9,12)))
mod3.new$NormAvg <- NULL
mod3.new$NormAvg <- 0
mm <- model.matrix(terms(mod3b),mod3.new)
mod3.new$NormAvg <- predict(mod3b,mod3.new,re.form=NA)

## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(mod3b),mm))
tvar1 <- pvar1 + VarCorr(mod3b)$Participant[1]  ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
mod3.new <- data.frame(
  mod3.new
  , plo = mod3.new$NormAvg - cmult*sqrt(pvar1)
  , phi = mod3.new$NormAvg+cmult*sqrt(pvar1)
  , tlo = mod3.new$NormAvg-cmult*sqrt(tvar1)
  , thi = mod3.new$NormAvg+cmult*sqrt(tvar1)
)
#plot confidence intervals
g0 <- ggplot(mod3.new, aes(x=Condition, y=NormAvg, colour=Gender))
g0 + geom_ribbon(aes(ymin = plo, ymax = phi, linetype=NA), alpha=0.2, show.legend=FALSE)+
  labs(x="Actual Number of Men in Ensemble", y="Perceived Sexist Norms")+
  theme_cowplot()+ geom_line(size=1)+
  scale_colour_discrete(name = "Perceiver Sex",
                        breaks=c(0, 1),
                        labels = c("Male", "Female"))+
  scale_shape_discrete(name="Perceiver Sex",
                       breaks=c(0, 1),
                       labels = c("Male", "Female"))+
  scale_y_continuous(name="Perceived Sexist Norms", breaks=c(1, 2, 3, 4, 5, 6, 7))+
  scale_x_continuous(breaks=c(0, 3, 6, 9, 12))+
  coord_cartesian(ylim= c(1, 7), xlim = c(0, 12))+
  theme(legend.justification=c(1,0.1), legend.position=c(1,0.1), legend.text=element_text(size=16), 
        legend.title=element_text(size=20), axis.title=element_text(size=20), axis.text=element_text(size=16))

#Perceived Belonging#
mod1 <- lmer(FitBelong ~ 1 + (1|Participant), data=masterpptlist)
summary(mod1)

# look at random effects of summary output
# calculate ICC by summing everything in variance column & dividing participant variance by it
1.156/(1.156+2.929)

mod3b <- lmer(FitBelong~Condition*Gender + (1+Condition|Participant), data=masterpptlist)
summary(mod3b)

multilevel.effect.size(mod3b)

#Reversing coding so can see simple slopes for women as comparison group#
mod3 <- lmer(FitBelong~Condition*RCGender+ (Condition|Participant), data=masterpptlist)
summary(mod3)

#Simple effects analysis#
mod3a <- lmer(FitBelong~Condition + (Condition|Participant), data=women.data)
summary(mod3a)

mod3b <- lmer(FitBelong~Condition+ (Condition|Participant), data=men.data)
summary(mod3b)

mod3.new <- with(masterpptlist, data.frame(Gender = factor(c(rep(0,5),rep(1,5))), 
                                           Condition = c(0,3,6,9,12,0,3,6,9,12)))
mod3.new$FitBelong <- NULL
mod3.new$FitBelong <- 0
mm <- model.matrix(terms(mod3b),mod3.new)
mod3.new$FitBelong <- predict(mod3b,mod3.new,re.form=NA)

## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(mod3b),mm))
tvar1 <- pvar1 + VarCorr(mod3b)$Participant[1]  ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
mod3.new <- data.frame(
  mod3.new
  , plo = mod3.new$FitBelong - cmult*sqrt(pvar1)
  , phi = mod3.new$FitBelong+cmult*sqrt(pvar1)
  , tlo = mod3.new$FitBelong-cmult*sqrt(tvar1)
  , thi = mod3.new$FitBelong+cmult*sqrt(tvar1)
)
#plot confidence intervals
g0 <- ggplot(mod3.new, aes(x=Condition, y=FitBelong, colour=Gender))
g0 + geom_ribbon(aes(ymin = plo, ymax = phi, linetype=NA), alpha=0.2, show.legend=FALSE)+
  labs(x="Actual Number of Men in Ensemble", y="Perceived Fit/Belonging")+
  theme_cowplot()+ geom_line(size=1)+
  scale_colour_discrete(name = "Perceiver Sex",
                        breaks=c(0, 1),
                        labels = c("Male", "Female"))+
  scale_shape_discrete(name="Perceiver Sex",
                       breaks=c(0, 1),
                       labels = c("Male", "Female"))+
  scale_y_continuous(name="Belonging", breaks=c(1, 2, 3, 4, 5, 6, 7))+
  scale_x_continuous(breaks=c(0, 3, 6, 9, 12))+
  coord_cartesian(ylim= c(1, 7), xlim = c(0, 12))+
  theme(legend.justification=c(1,0.1), legend.position=c(1,0.1), legend.text=element_text(size=16), 
        legend.title=element_text(size=20), axis.title=element_text(size=20), axis.text=element_text(size=16))