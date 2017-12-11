#Loading required libraries into R#
library(foreign)
library(lme4)

#Read in CSV#
LongformMerged <- read.csv("~/repos/Publications/GoodaleAltLickJohnson_JEPGeneral_2018/Study 3/Stage 2/Goodale_Data_Study3_Stage2.csv",
                           header=TRUE)

#Trait Group 0 = Majority Men Ensemble, Trait Group 1 = Majority Women Ensemble#
#Making Categorization Responses into Numeric Dummy codes#
LongformMerged$Categorization<- recode(LongformMerged$Categorization, "'2'= 'W'; '1' = 'M'")
LongformMerged$Categorization<- recode(LongformMerged$Categorization, "'W'= '1'; 'M' = '0'")

LongformMerged$Gender <- recode(LongformMerged$Gender, "'2'= 'W'; '1' = 'M'")
LongformMerged$Gender<- recode(LongformMerged$Gender, "'W'= '1'; 'M' = '0'")

#demographics#
variables_want <- c("ResponseID", "Gender")
demo_subset <- LongformMerged[, variables_want]
demo_subset <- unique(demo_subset)
count(demo_subset$Gender)
54/(54+66) #45% of sample are women

#conditional logistic regression, since only n=2 groups/2 observations)
library(survival)
fit.CLR <- clogit(Categorization ~ TraitGroup+strata(ResponseID), method="exact", data=LongformMerged)
summary(fit.CLR)

#checking if perceiver gender moderates outcome
#Need to create interaction term since clogit does not handle interactions
LongformMerged$MajMen_PerceiverWoman <- ifelse(LongformMerged$Gender==1 & LongformMerged$TraitGroup==0, 1, 0)
LongformMerged$MajWomen_PerceiverWoman <- ifelse(LongformMerged$Gender==1 & LongformMerged$TraitGroup==1, 1, 0)
LongformMerged$MajMen_PerceiverMan <- ifelse(LongformMerged$Gender==0 & LongformMerged$TraitGroup==0, 1, 0)
LongformMerged$MajWomen_PerceiverMan <- ifelse(LongformMerged$Gender==0 & LongformMerged$TraitGroup==1, 1, 0)

fit.CLR2a <- clogit(Categorization ~ MajWomen_PerceiverMan+strata(ResponseID), method="exact", data=LongformMerged)
summary(fit.CLR2a) #p=.0499 **

fit.CLR2b <- clogit(Categorization ~ MajWomen_PerceiverWoman+strata(ResponseID), method="exact", data=LongformMerged)
summary(fit.CLR2b) #p=.178 NS

fit.CLR2c <- clogit(Categorization ~ MajMen_PerceiverWoman+strata(ResponseID), method="exact", data=LongformMerged)
summary(fit.CLR2c) #p=.178 NS

fit.CLR2d <- clogit(Categorization ~ MajMen_PerceiverMan+strata(ResponseID), method="exact", data=LongformMerged)
summary(fit.CLR2d) #**
