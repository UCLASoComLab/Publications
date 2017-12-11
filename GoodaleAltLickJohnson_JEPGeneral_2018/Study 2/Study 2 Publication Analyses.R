library(lme4)
library(ggplot2)
library(cowplot)
detach("package:lmerTest", unload=TRUE)


masterpptlist <- read.csv("~/repos/Publications/GoodaleAltLickJohnson_JEPGeneral_2018/Study 2/Goodale_Data_Study2.csv", header = TRUE)
masterpptlist[,1] <- NULL

#FitBelong combined as DV#
masterpptlist$FitBelong <- (masterpptlist$Belong + masterpptlist$Fit)/2
class(masterpptlist$FitBelong)

#subsetting data for simple effects analyses#
men.data <- data.frame(subset(masterpptlist, Gender == 0))
women.data <- data.frame(subset(masterpptlist, Gender == 1))

Zeromen.data <- data.frame(subset(masterpptlist, Condition == 0))
TwentyFivemen.data <- data.frame(subset(masterpptlist, Condition == 3))
Fiftymen.data <- data.frame(subset(masterpptlist, Condition == 6))
SeventyFivemen.data <- data.frame(subset(masterpptlist, Condition == 9))
Onehundredmen.data <- data.frame(subset(masterpptlist, Condition == 12))

#demographics#
variables_want <- c("Participant", "Gender")
demo_subset <- masterpptlist[,variables_want]
demo_subset <- unique(demo_subset)
count(demo_subset$Gender)
47/58 #81% of sample are women

#Running MLM Mediation Analyses#
#Perceived ratio mediating effect of actual ratio on fit/belong#
library(mediation)
masterpptlist$same_sex_ratio <- NA
masterpptlist$actual_same_sex <- NA
masterpptlist$actual_same_sex <- ifelse(masterpptlist$Gender==0 & masterpptlist$Condition==0, 0,
                                        ifelse(masterpptlist$Gender==0 & masterpptlist$Condition==3, 3,
                                        ifelse(masterpptlist$Gender==0 & masterpptlist$Condition==6, 6,
                                        ifelse(masterpptlist$Gender==0 & masterpptlist$Condition==9, 9,
                                        ifelse(masterpptlist$Gender==0 & masterpptlist$Condition==12, 12,
                                        ifelse(masterpptlist$Gender==1 & masterpptlist$Condition==0, 12,
                                        ifelse(masterpptlist$Gender==1 & masterpptlist$Condition==3, 9,
                                        ifelse(masterpptlist$Gender==1 & masterpptlist$Condition==6, 6,
                                        ifelse(masterpptlist$Gender==1 & masterpptlist$Condition==9, 3,
                                        ifelse(masterpptlist$Gender==1 & masterpptlist$Condition==12, 0, NA
                                        ))))))))))

masterpptlist$same_sex_ratio <- masterpptlist$Ratio-1
masterpptlist$same_sex_ratio <- ifelse(masterpptlist$Gender==1, 12-masterpptlist$same_sex_ratio, masterpptlist$Ratio-1)

med.fit <- lmer(same_sex_ratio ~ actual_same_sex + (actual_same_sex|Participant), data = masterpptlist)
out.fit <- lmer(FitBelong ~ actual_same_sex+same_sex_ratio + (actual_same_sex+same_sex_ratio|Participant), data = masterpptlist)

med.out <- mediate(med.fit, out.fit, treat = "actual_same_sex", mediator = "same_sex_ratio", 
                   control.value = 0, treat.value = 3, sims = 10000)
summary(med.out)

med.out <- mediate(med.fit, out.fit, treat = "actual_same_sex", mediator = "same_sex_ratio", 
                   control.value = 0, treat.value = 6, sims = 10000)
summary(med.out)

med.out <- mediate(med.fit, out.fit, treat = "actual_same_sex", mediator = "same_sex_ratio", 
                   control.value = 0, treat.value = 9, sims = 10000)
summary(med.out)

med.out <- mediate(med.fit, out.fit, treat = "actual_same_sex", mediator = "same_sex_ratio", 
                   control.value = 0, treat.value = 12, sims = 10000)
summary(med.out)
