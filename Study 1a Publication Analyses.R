#Loading required libraries into R#
library(multilevel)
library(lattice)
library(foreign)
library(lme4)
library(mlmRev)
library(plyr)
library(car)
library(lmerTest)
library(psych)
library(ggplot2)
library(QuantPsyc)
library(cowplot)

#Read in CSV#
LFS1Ensemble<-read.csv("~/Dropbox/Research/Ensemble Coding/Study 1 Women Belongingness/Cleaned Data/Goodale_Data_Study1a.csv", header = TRUE, sep = ',')
LFS1Ensemble<-data.frame(LFS1Ensemble)

class(LFS1Ensemble$ResponseID)
class(LFS1Ensemble$Gender)
LFS1Ensemble$Gender <- factor(LFS1Ensemble$Gender)
levels(LFS1Ensemble$Gender)
LFS1Ensemble$Gender<- recode(LFS1Ensemble$Gender, "'2'= 'W'; '1' = 'M'")
LFS1Ensemble$Gender <- recode(LFS1Ensemble$Gender, "'W'= '1'; 'M' = '0'")
class(LFS1Ensemble$Age)
class(LFS1Ensemble$Condition)
count(LFS1Ensemble$Condition)

#demographics#
variables_want <- c("ResponseID", "Gender", "Age")
demo_subset <- LFS1Ensemble[,variables_want]
demo_subset <- unique(demo_subset)
count(demo_subset$Gender)
52/91 #57% of sample are women
summary(demo_subset$Age) #mean age = 37.14 years old, 
sd(demo_subset$Age) #SD = 12.30

#adding Target Stimuli numbers#
LFS1Ensemble$Stimuli <- NA
library(car)
class(LFS1Ensemble$Image)
LFS1Ensemble$Stimuli <- recode(LFS1Ensemble$Image, "'0men1' = 1 ; '0men2' = 2; '0men3' = 3; '0men4' = 4; '20men1' = 5;'20men2'=6; '20men3'=7; '20men4' = 8;'50men1' = 9; '50men2' = 10; '50men3'= 11; '50men4' = 12; '80men1' = 13; '80men2' = 14; '80men3' = 15; '80men4' = 16; '100men1' = 17; '100men2' = 18; '100men3' = 19; '100men4' = 20")
class(LFS1Ensemble$Stimuli)
library(lme4)

#Reversing coding so can see simple slopes for women as comparison group#
LFS1Ensemble$RCGender<- recode(LFS1Ensemble$Gender, "'1'= 'W'; '0' = 'M'")
LFS1Ensemble$RCGender <- recode(LFS1Ensemble$RCGender, "'W'= '0'; 'M' = '1'")

#FitBelong combined as DV#
LFS1Ensemble$FitBelong <- NULL
LFS1Ensemble$FitBelong <- (LFS1Ensemble$Belonging + LFS1Ensemble$Fit)/2
class(LFS1Ensemble$FitBelong)

#Creating Norms Analsis#
LFS1Ensemble$RCNorm1 <- 8 - LFS1Ensemble$Norm_1
LFS1Ensemble$RCNorm4 <- 8 - LFS1Ensemble$Norm_4
LFS1Ensemble$RCNorm5 <- 8 - LFS1Ensemble$Norm_5
LFS1Ensemble$RCNorm6 <- 8 - LFS1Ensemble$Norm_6
LFS1Ensemble$RCNorm8 <- 8 - LFS1Ensemble$Norm_8
LFS1Ensemble$NormAvg <- (LFS1Ensemble$RCNorm1 + LFS1Ensemble$RCNorm4 + LFS1Ensemble$RCNorm5 + LFS1Ensemble$RCNorm6 + LFS1Ensemble$RCNorm8 + LFS1Ensemble$Norm_2 + LFS1Ensemble$Norm_3 + LFS1Ensemble$Norm_7)/8

#subsetting data for simple effects analyses#
men.data <- data.frame(subset(LFS1Ensemble, Gender == 0))
women.data <- data.frame(subset(LFS1Ensemble, Gender == 1))

##Data Analysis for Perceived Sex Ratio & Perceived Facial Masculinity##
#Perceived Sex Ratio of the group#
mod3a <- lmer(Ratio~1 + (1|ResponseID) + (1|Stimuli), data=LFS1Ensemble)
summary(mod3a)

#ICCs#
0.1728/(0.1728+9.5556+2.6594)

mod3b <- lmer(Ratio~Condition*Gender + (1+Condition|ResponseID) + (1|Stimuli), data=LFS1Ensemble, REML=TRUE)
summary(mod3b)

mod3.new <- with(LFS1Ensemble, data.frame(Gender = factor(c(rep(0,5),rep(1,5))), Condition = c(0,2,4,6,8,0,2,4,6,8)))
mod3.new$Ratio <- NULL
mod3.new$Ratio <- 0
mm <- model.matrix(terms(mod3b),mod3.new)
mod3.new$Ratio <- predict(mod3b,mod3.new,re.form=NA)

## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(mod3b),mm))
tvar1 <- pvar1+VarCorr(mod3b)$ResponseID[1]  ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
mod3.new <- data.frame(
  mod3.new
  , plo = mod3.new$Ratio-cmult*sqrt(pvar1)
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
  coord_cartesian(ylim= c(0, 12), xlim = c(0, 8))+
  theme(legend.justification=c(1,0.1), legend.position=c(1,0.1), legend.text=element_text(size=16), 
        legend.title=element_text(size=20), axis.title=element_text(size=20), axis.text=element_text(size=16))

#Perceived Facial Masculinity#
mod3a <- lmer(Average_Face~1 + (1|ResponseID) + (1|Stimuli), data=LFS1Ensemble, REML=FALSE)
summary(mod3a)

#ICCs#
5.915/(5.915+3.793+9.330)

mod3b <- lmer(Average_Face~Condition*Gender + (Condition|ResponseID) + (1|Stimuli), data=LFS1Ensemble, REML=TRUE)
summary(mod3b)

multilevel.effect.size(mod3b)

mod3.new <- with(LFS1Ensemble, data.frame(Gender = factor(c(rep(0,5),rep(1,5))), Condition = c(0,2,4,6,8,0,2,4,6,8)))
mod3.new$Average_Face <- predict(mod3b, newdata = mod3.new, type = "response", re.form = NA, level=0)

mod3.new$Average_Face <- NULL
mod3.new$Average_Face <- 0
mm <- model.matrix(terms(mod3b),mod3.new)
mod3.new$Average_Face <- predict(mod3b,mod3.new,re.form=NA)

## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(mod3b),mm))
tvar1 <- pvar1+VarCorr(mod3b)$ResponseID[1]  ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
mod3.new <- data.frame(
  mod3.new
  , plo = mod3.new$Average_Face-cmult*sqrt(pvar1)
  , phi = mod3.new$Average_Face+cmult*sqrt(pvar1)
  , tlo = mod3.new$Average_Face-cmult*sqrt(tvar1)
  , thi = mod3.new$Average_Face+cmult*sqrt(tvar1)
)
#plot confidence intervals
g0 <- ggplot(mod3.new, aes(x=Condition, y=Average_Face, colour=Gender))
g0 + geom_ribbon(aes(ymin = plo, ymax = phi, linetype=NA), alpha=0.2, show.legend=FALSE)+
  labs(x="Actual Number of Men in Ensemble", y="Perceived Facial Masculinity")+
  theme_cowplot()+ geom_line(size=1)+
  scale_colour_discrete(name = "Perceiver Sex",
                        breaks=c(0, 1),
                        labels = c("Male", "Female"))+
  scale_shape_discrete(name="Perceiver Sex",
                       breaks=c(0, 1),
                       labels = c("Male", "Female"))+
  coord_cartesian(xlim = c(0, 8), ylim = c(1, 21))+
  theme(legend.justification=c(1,0.1), legend.position=c(1,0.1), legend.text=element_text(size=16), 
        legend.title=element_text(size=20), axis.title=element_text(size=20), axis.text=element_text(size=16))

#Data Analysis for Social Attitudes & Affordances##
#Perceived Sexist Norms#
mod3a <- lmer(NormAvg~1 + (1|ResponseID) + (1|Stimuli), data=LFS1Ensemble)
summary(mod3a)

#ICCs#
0.4450/(0.4450+0.2097+0.4931) #ICC for Response ID grouping

mod3b <- lmer(NormAvg~Condition*Gender + (1+Condition|ResponseID) + (1|Stimuli), data=LFS1Ensemble, REML=TRUE)
summary(mod3b)

multilevel.effect.size(mod3b)

mod11.new <- with(LFS1Ensemble, data.frame(Gender = factor(c(rep(0,5),rep(1,5))), Condition = c(0,2,4,6,8,0,2,4,6,8)))
mod11.new$NormAvg <- predict(mod3b, newdata = mod11.new, type = "response", re.form = NA, level=0)
mod11.new$NormAvg <- NULL
mod11.new$NormAvg <- 0
mm <- model.matrix(terms(mod3b),mod11.new)
mod11.new$NormAvg <- predict(mod3b,mod11.new,re.form=NA)

## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(mod3b),mm))
tvar1 <- pvar1+VarCorr(mod3b)$ResponseID[1]  ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
mod11.new <- data.frame(
  mod11.new
  , plo = mod11.new$NormAvg-cmult*sqrt(pvar1)
  , phi = mod11.new$NormAvg+cmult*sqrt(pvar1)
  , tlo = mod11.new$NormAvg-cmult*sqrt(tvar1)
  , thi = mod11.new$NormAvg+cmult*sqrt(tvar1)
)
#plot confidence intervals
g0 <- ggplot(mod11.new, aes(x=Condition, y=NormAvg, colour=Gender))
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
  coord_cartesian(ylim= c(1, 7), xlim = c(0, 8))+
  theme(legend.justification=c(1,0.1), legend.position=c(1,0.1), legend.text=element_text(size=16), 
        legend.title=element_text(size=20), axis.title=element_text(size=20), axis.text=element_text(size=16))

#Belonging#
mod3a <- lmer(FitBelong~1 + (1|ResponseID) + (1|Stimuli), data=LFS1Ensemble)
summary(mod3a)

# look at random effects of summary output
# calculate ICC by summing everything in variance column & dividing participant variance by it
1.7969/(1.7969+0.2577+3.5953)

mod3b <- lmer(FitBelong~Condition*Gender + (1+Condition|ResponseID) + (1|Stimuli), data=LFS1Ensemble, REML=TRUE)
summary(mod3b)

#Reversing coding so can see simple slopes for women as comparison group#
mod3 <- lmer(FitBelong~Condition*RCGender+ (Condition|ResponseID) + (1|Stimuli), data=LFS1Ensemble)
summary(mod3)

#Looking at Simple Effects#
mod3a <- lmer(FitBelong~Condition+ (Condition|ResponseID) + (1|Stimuli), data=men.data)
summary(mod3a)

mod3b <- lmer(FitBelong~Condition+ (Condition|ResponseID) + (1|Stimuli), data=women.data)
summary(mod3b)

#Graphing model#
mod3.new <- with(LFS1Ensemble, data.frame(Gender = factor(c(rep(0,5),rep(1,5))), Condition = c(0,2,4,6,8,0,2,4,6,8)))
mod3.new$FitBelong <- NULL
mod3.new$FitBelong <- 0
mm <- model.matrix(terms(mod3b),mod3.new)
mod3.new$FitBelong <- predict(mod3b,mod3.new,re.form=NA)

## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(mod3b),mm))
tvar1 <- pvar1+VarCorr(mod3b)$ResponseID[1]  ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
mod3.new <- data.frame(
  mod3.new
  , plo = mod3.new$FitBelong-cmult*sqrt(pvar1)
  , phi = mod3.new$FitBelong+cmult*sqrt(pvar1)
  , tlo = mod3.new$FitBelong-cmult*sqrt(tvar1)
  , thi = mod3.new$FitBelong+cmult*sqrt(tvar1)
)
#plot confidence intervals
library(cowplot)
g0 <- ggplot(mod3.new, aes(x=Condition, y=FitBelong, colour=Gender))
g0 + geom_ribbon(aes(ymin = plo, ymax = phi, linetype=NA), alpha=0.2, show.legend=FALSE)+
  labs(x="Actual Number of Men in Ensemble", y="Belonging")+
  theme_cowplot()+ geom_line(size=1)+
  scale_colour_discrete(name = "Perceiver Sex",
                        breaks=c(0, 1),
                        labels = c("Male", "Female"))+
  scale_shape_discrete(name="Perceiver Sex",
                       breaks=c(0, 1),
                       labels = c("Male", "Female"))+
  scale_y_continuous(name="Belonging", breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9))+
  coord_cartesian(ylim= c(1, 7), xlim = c(0, 8))+
  theme(legend.justification=c(1,0.1), legend.position=c(1,0.1), legend.text=element_text(size=16), 
        legend.title=element_text(size=20), axis.title=element_text(size=20), axis.text=element_text(size=16))
