#Study 1a
Dt_keep<-read.csv("~/Dropbox/Research/Ensemble Coding/Study 1 Women Belongingness/Cleaned Data/R Analyses Cleaned Data Set Corrected.csv", header = TRUE, sep = ',')
DT_keep<-data.frame(Dt_keep)
var_want <- c("ResponseID", "Gender", "Age", "Image", "Condition", "Fit", "Belonging", "Norm_1", "Norm_2",
              "Norm_3", "Norm_4", "Norm_5", "Norm_6", "Norm_7", "Norm_8", "Ratio", "Average_Face")
DT_keep <- DT_keep[, var_want]
DT_keep<- DT_keep[which(!is.na(DT_keep$Gender)),]

write.csv(DT_keep, file = "~/Dropbox/Research/Ensemble Coding/Study 1 Women Belongingness/Cleaned Data/Goodale_Data_Study1a.csv", header=TRUE)

#Study 1b

dat1 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 1/Long Form/rev_corr_experiment_2016-01-25-08h58m24s_long_form.csv", colClasses='character')
dat2 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 1/Long Form/rev_corr_experiment_2016-01-25-11h13m06s_long_form.csv", colClasses='character')
dat3 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 1/Long Form/rev_corr_experiment_2016-01-26-13h02m55s_long_form.csv", colClasses='character')
dat4 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 1/Long Form/rev_corr_experiment_2016-01-27-10h06m12s_long_form.csv", colClasses='character')
dat4$participant_id[dat4$participant_id == "RP24" & dat4$demographics_206_gender_1_keypress == "1"] <- "RP99"

dat5 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 1/Long Form/rev_corr_experiment_2016-01-29-10h04m31s_long_form.csv", colClasses='character')
dat6 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 1/Long Form/rev_corr_experiment_2016-02-01-14h02m23s_long_form.csv", colClasses='character')
dat7 <-read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 1/Long Form/rev_corr_experiment_2016-02-02-11h08m28s_long_form.csv", colClasses='character')
dat8 <-read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 1/Long Form/rev_corr_experiment_2016-02-03-11h10m02s_long_form.csv", colClasses='character')
dat9 <-read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 1/Long Form/rev_corr_experiment_2016-02-05-10h03m03s_long_form.csv", colClasses='character')

#Raw Data Computer 2#
dat10 <-read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 2/Long Form/rev_corr_experiment_2016-01-25-08h59m10s_long_form.csv", colClasses='character')
dat11 <-read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 2/Long Form/rev_corr_experiment_2016-01-26-13h04m22s_long_form.csv", colClasses='character')
dat12 <-read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3//Computer 2/Long Form/rev_corr_experiment_2016-01-27-10h06m56s_long_form.csv", colClasses='character')
dat13 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 2/Long Form/rev_corr_experiment_2016-01-28-12h28m00s_long_form.csv", colClasses='character')
dat14 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 2/Long Form/rev_corr_experiment_2016-01-29-10h07m51s_long_form.csv", colClasses='character')
dat15 <-read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 2/Long Form/rev_corr_experiment_2016-02-03-11h10m52s_long_form.csv", colClasses='character')
dat16 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 2/Long Form/rev_corr_experiment_2016-02-05-10h04m19s_long_form.csv", colClasses='character')

#Raw Data From Computer #4#
dat17 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 3/Long Form/rev_corr_experiment_2016-01-25-09h03m50s_long_form.csv", colClasses='character')
dat18 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 3/Long Form/rev_corr_experiment_2016-01-27-10h11m14s_long_form.csv", colClasses='character')
dat19 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 3/Long Form/rev_corr_experiment_2016-01-29-10h12m43s_long_form.csv", colClasses='character')
dat20 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 3/Long Form/rev_corr_experiment_2016-02-03-11h15m10s_long_form.csv", colClasses='character')
dat21 <- read.csv("~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Computer 3/Long Form/rev_corr_experiment_2016-02-05-10h08m44s_long_form.csv", colClasses='character')
dat21$image_grid_stimulus_keypress <- NA

dat1 <- unique(dat1)
dat2<-unique(dat2)
dat3<-unique(dat3)
dat4<-unique(dat4)
dat5<-unique(dat5)
dat6<-unique(dat6)
dat7<-unique(dat7)
dat8<-unique(dat8)
dat9<-unique(dat9)
dat10<-unique(dat10)
dat11<-unique(dat11)
dat12<-unique(dat12)
dat13<-unique(dat13)
dat14<-unique(dat14)
dat15<-unique(dat15)
dat16<-unique(dat16)
dat17<-unique(dat17)
dat18<-unique(dat18)
dat19<-unique(dat19)
dat20<-unique(dat20)
dat21<-unique(dat21)

#To load all the data frames into a single file##

masterpptlist <- do.call("rbind", list(dat1, dat2, dat3, dat4, dat5,
                                       dat6,
                                       dat7,
                                       dat8,
                                       dat9,
                                       dat10,
                                       dat11,
                                       dat12,
                                       dat13,
                                       dat14,
                                       dat15,
                                       dat16,
                                       dat17,
                                       dat18,
                                       dat19,
                                       dat20,
                                       dat21))

masterpptlist<- subset(masterpptlist, select = -c(image_grid_stimulus_keypress))

#rename columns#
colnames(masterpptlist) <- c("Participant", "Gender", "Fit", "Condition", "Belonging", "WomenContributeNorm", "NotMakeFunNorm", "ExcludeNorm", "FriendlyNorm",
                             "GoodatMathNorm", "TreatDifferentlyNorm", "DefertoMenNorm", "AskHelpNorm", "Ratio", "AverageFace")

#removing RP from participants string#
masterpptlist$Participant <- gsub("RP", "", masterpptlist$Participant)

#recoding ratio measure#
#1, 2, 3, 4, 5, 6, 7, 8, 9, 0=10, q=11, w=12, e=13"
masterpptlist$Ratio <- recode(masterpptlist$Ratio, " '0' = '10'; 'Q' = '11'; 'q' = '11'; 'W' = '12'; 'w' = '12'; 'E'='13'; 'e' = '13'")
masterpptlist$Ratio <-as.numeric(masterpptlist$Ratio)

#recoding Average Face measure#
#1, 2, 3, 4, 5, 6, 7, 8, 9, 0=10, q=11, w=12, e=13, r=14, t=15, y=16, u=17, i=18, o=19, p=20, a=21#
masterpptlist$AverageFace <- recode(masterpptlist$AverageFace, "'0'='10'; 'Q' = '11'; 'q' = '11'; 'W' = '12'; 'w' = '12'; 
                                    'E'='13'; 'e' = '13'; 'r'='14'; 'R'='14'; 't'='15'; 'T'='15'; 'y'='16'; 'Y'='16';
                                    'u'='17'; 'U'='17'; 'i'='18'; 'I'='18'; 'o'='19'; 'O'='19'; 'p'='20'; 'P'='20'; 'a'='21';
                                    'A'='21'")
masterpptlist$AverageFace <- as.numeric(masterpptlist$AverageFace)
masterpptlist <- masterpptlist[which(!is.na(masterpptlist$Gender)),]

write.csv(masterpptlist, "~/Dropbox/Research/Ensemble Coding/Study 3 or 1B Replicating Study 1/Redo Study 3/Data Analysis/Final Publication Analyses & Data/Goodale_Data_Study1b.csv")
