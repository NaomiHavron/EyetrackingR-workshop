require(lme4)
library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)
library(eyetrackingR)
library (pbapply)
library(effsize)
library(lmerTest)
devtools::install_github("jwdink/eyetrackingR") ##newest version of eyetrackingR for bug fixes

setwd("") ## change acording to directory where file is
data<- read.table("Results.txt", header = TRUE) #change to fit name of your txt file

##change the "time" column since it is a reserved word
colnames(data)[2]<-"Time1"

#if you need to recode the "Target" Variable to a "looks_animate":
data$looks_anim<-ifelse(data$Condition=="A", data$Target, data$Distractor)
data$looks_ina<-ifelse(data$Condition=="I", data$Target, data$Distractor)

## cleaning trials with more than 50% lost data 
#first step, create a dataset with a colunm for sum_away/length_away
out <- ddply(subset(data, Time1 >= 0 & Time1 <= 10000),.(Subject, Trial), summarize, fun = sum(Away)/length(Away))

# next we merge it with out data
data_clean <- merge(data, out)

# Each trial that has more than 50% of away/missing gaze is removed 
data_clean <- subset(data_clean, fun <= 0.50)

# Display the number of remaining trials per subject 
ddply(subset(data_clean),.(Subject, Condition), summarize, N = length(unique(Trial)))

#manually remove those with less than X trials
data_clean<-subset(data_clean, Subject!="100")

##note that we did not use the eyetrackingR function for cleaning the data, as it gave us results
##that were inconsistent with the manual way of working

##making eye-tracking data 
eyetrackingr_data <- make_eyetrackingr_data(subset(data_clean,Time1>=0 & Time1<=10000), 
                                   participant_column = "Subject",
                                   trial_column = "Trial",
                                   item_columns = "Item",
                                   time_column = "Time1",
                                   trackloss_column = "Away",
                                   aoi_columns = c('looks_anim', 'looks_ina'),
                                   treat_non_aoi_looks_as_missing = F
)


## down-sampling the data to 20ms (change number if needed).
##use this code if you need to run a mixed effects permutation analysis
sequence_data<-make_time_sequence_data(data = eyetrackingr_data, 20, aois = "looks_anim",
                               predictor_column ="Condition")
##but this code if you are going to run a t test permutation analysis!!
## down-sampling the data to 20ms (change number if needed)
sequence_data<-make_time_sequence_data(data = eyetrackingr_data, 20, aois = "looks_anim",
                                       predictor_column ="Condition", summarize_by = "Subject")



###########
##to graph the gaze data

plot(subset(sequence_data, Time>0 & Time<=10000), predictor_column = "Condition", dv = "Prop", model = NULL)+
  scale_colour_manual(values=c( "red", "blue"))+
  theme(text = element_text(size = 20)) +
  coord_cartesian(xlim = c(350, 9500)) +
  scale_x_continuous(name = "Time (in ms)", breaks = seq(0, 10000, 500))+
  scale_y_continuous(name = "Proportion of looks towards action video", breaks = seq(0, 1, 0.25), limits = (0:1))

# for conventional anlysis, I average proportion of looks and run a t test or a lmer:
## avergae the data per item for a t.test

subjs <- ddply(sequence_data, .(Subject, Condition), function(x) mean(x$Prop, na.rm = T))

#how many participants do we have in each condition? what is the mean and SD?
xtabs(~subjs$Condition)
ddply(subjs, .(Condition), function(x) mean(x$V1, na.rm = T))
ddply(subjs, .(Condition), function(x) sd(x$V1, na.rm = T))

# t test
t.test (subjs$V1~subjs$Condition, paired= F)

##claculate cohen's d
cohen.d(subjs$V1,subjs$Condition,pooled=TRUE,paired=FALSE,
        conf.level=0.95)

# mixed effects model is not done on the averaged data
summary(lmer(ArcSin~Condition+(1|Subject), data = sequence_data))

    ##plotting the overall looking time
ggplot(subjs,aes(x= Condition, y=V1))+
  geom_boxplot(aes(fill=Condition))+
  theme(text = element_text(size = 20)) +
  geom_dotplot(binwidth = 0.005, binaxis='y', stackdir='center', size = 2, fill = "grey") +
  stat_summary(fun.y=mean, geom = "text", label="----", size= 10, color= "white")+
  scale_x_discrete(name = "Condition")+
  scale_y_continuous(name = "Proportion of looks", breaks = seq(0.2, 0.9, 0.1))
  
## to do a cluster analysis you next have to make_time_cluster_data
##either a t test

cluster_data<- make_time_cluster_data(data = sequence_data, predictor_column = "Condition",
                              test = "t.test",
                              aoi="looks_anim",
                              threshold = 1.5,
                              formula = ArcSin ~ Condition
)
summary(data)
## or a mixed effects
cluster_data<- make_time_cluster_data(data = sequence_data, predictor_column = "Condition",
                              test = "lmer",
                              aoi="looks_anim",
                              treatment_level = "A",
                              threshold = 1.5,
                              formula = ArcSin ~ Condition + (1|Subject)
                              
)
summary(data)


##and then analyze_time_clusters as a t test
f1<-analyze_time_clusters(cluster_data, within_subj=FALSE,
                          samples = 1000, formula = ArcSin ~ Condition,
                          shuffle_by = "Condition", parallel = F,
                          quiet = FALSE)
summary(f1)

##or a mixed effects model
f1<-analyze_time_clusters(cluster_data, within_subj=FALSE,
                          samples = 1000, formula = ArcSin ~ Condition + (1|Subject), 
                          treatment_level = "A", shuffle_by = "Condition", parallel = F,
                          quiet = FALSE)
summary(f1)


### Any other questions? go to http://www.eyetracking-r.com/, 
#or contact me an naomi.havron@mail.huji.ac.il