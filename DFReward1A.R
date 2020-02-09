##################################
# Reward_DF Exp 1A Data Analysis #
##################################

#By Sara Gallant 

# clear the workspace
rm(list = ls())

#set the working directory
setwd("/Users/saragallant/GDrive/USC/Projects/Reward_DF/Exp1A/Data")

# install the relevant packages
install.packages('dplyr')
install.packages('xlsx')
install.packages("tidyverse")
install.packages('data.table')
install.packages('rprojroot')
install.packages('magrittr')
install.packages('MASS')

# load the packages (and all its dependencies)
library(data.table)
library(rprojroot)
library(tidyr)
library(dplyr)
library(xlsx)
library(tidyverse)
library(MASS)

#read in CSV files using tidyverse
CB1<-read_csv("CB1.csv", skip = 1)
CB2<-read_csv("CB2.csv", skip = 1)
CB3<-read_csv("CB3.csv", skip = 1)
CB4<-read_csv("CB4.csv", skip = 1)
CB5<-read_csv("CB5.csv", skip = 1)
CB6<-read_csv("CB6.csv", skip = 1)

#merge CSV files into one dataframe
merged.data<-rbind(CB1, CB2, CB3, CB4, CB5, CB6)

#save the merged file to working directory...then code memory performance in excel
write.table(merged.data, file="CBmerged.csv", sep=",")

#re-import the now coded recognition data 
rm(list = ls())
rec.perf<-fread(file="rec.coded.csv", na.strings = NULL, stringsAsFactors = TRUE)

#rename variables prior to restructuring
rec.perf <- rec.perf %>%
  rename(R = R.avg, F = F.avg, New = New.avg)

#restructure the dataset to a long format by combining the means for R, F, and New words into one variable
rec.perf <- rec.perf %>%
  pivot_longer(c('R', 'F'), names_to = "Cue", values_to = "Mean")

#convert Cue variable from a character to a factor
rec.perf$Cue <- as.factor(rec.perf$Cue)

#summarize the data as a function of age group and cue level...compute mean, sd, and se
rec.perf.summary <- rec.perf %>%
  group_by(AgeGrp, Cue) %>%
  summarize(mean.perf = mean(Mean, na.rm = TRUE), 
            sd.perf = sd(Mean, na.rm = TRUE),
            n.perf = n()) %>%
  mutate(se.perf = sd.perf / sqrt(n.perf))

#plot the data
ggplot(data = rec.perf.summary, aes(x = Cue, y = mean.perf)) + 
  geom_bar(stat = "identity", colour = "black", width = 0.6, position = position_dodge(0.7), size = 0.5) +
  geom_errorbar(aes(ymin=(mean.perf-se.perf), ymax=(mean.perf+se.perf)), width=.1, position = position_dodge(0.7)) +
  facet_grid(. ~ AgeGrp) +
  ylab(label="Mean Recognition") +
  xlab(label=element_blank()) +
  ylim(0.0,1.0) +
  scale_x_discrete(limits=c("R", "F")) +
  theme(
    strip.text.x = element_text(size = 18, colour = "black", face = "bold"), 
    axis.text.x = element_text(size = 18, colour="black"),
    axis.text.y = element_text(size = 18, colour = "black", ),
    axis.title.y = element_text(size = 18, colour = "black")
  )

#run stats: age x cue anova and then print the results
rm.anova <- aov(Mean ~ AgeGrp + Cue + AgeGrp*Cue, data = rec.perf)  
summary(rm.anova) 

#posthoc comparison
TukeyHSD(rm.anova)
