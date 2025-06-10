#############################################################################################
#############################################################################################

# STATISTICAL ANALYSES EXPERIMENT 2 (THIRD-PARTY)
# Based on Tables prepared in "TablePrep_third.R" Script

# 27/03/2023

# **TABLE OF CONTENTS**
# SECTION 1: OVERVIEW OF VALID TRIALS
# SECTION 2: DESCRIPTIVE DATA, VISUALIZATIONS, & ANALYSES
# SECTION 3: EXPLORATORY ANALYSES

#############################################################################################
#############################################################################################

# Clear workspace
rm(list = ls(all.names = TRUE)) # clears everything
graphics.off() # close all open graphics

# read in data frames (created in Table prep script)
mean.overall.data <- read.csv("mean.overall.data.third", header = TRUE, sep='\t')
overall.data <- read.csv("overall.data.third", header = TRUE,sep='\t')
merged.data <- read.csv("mean.overall.data.merged.csv", header = TRUE, sep=';') # contains data from Experiment 1 & 2

# adjusting overall.data in a way that concom*outcome model is more similar to the identity*soncom and location*concom model
overall.data$ConSoc <- factor(overall.data$ConSoc, c("NCOM", "COM"))
overall.data$ConOut <- factor(overall.data$ConOut, c("NO", "LOCATION", "IDENTITY"))

# Load packages
library("ggplot2")
library("lme4")
library("exactRankTests")
library("tidyverse")
library("emmeans")

#############################################################################################
# SECTION 1: OVERVIEW OF VALID TRIALS
#############################################################################################

# Overview of valid trials for each individual child)
aggregate(overall.data$trial_valid ~ Condition+ID, data=overall.data, sum, na.rm=TRUE)

# Table S8 in Supplementary Materials: Summary of valid trial info per condition # if it doesn't give for each condition: detach(package:dplyr)
mean.overall.data %>%
  group_by(Condition) %>%
  summarise(sum=sum(trial_valid),
            min=min(trial_valid),
            max=max(trial_valid),
            mean=mean(trial_valid),
            sd=sd(trial_valid)
  )

# Table S8 in Supplementary Materials: Total number of trials over all conditions
mean.overall.data %>%
  summarise(sum=sum(trial_valid),
            min=min(trial_valid),
            max=max(trial_valid),
            mean=mean(trial_valid),
            sd=sd(trial_valid)
  )

# Table S8 in Supplementary Materials: Number of trials per CONDITION in which infants looked and DID NOT look at the object during familarization
mean.overall.data %>%
  group_by(Condition) %>%
  summarise(LOB=sum(trial_LOB_Act_valid),
            NLOB=sum(trial_valid) - sum(trial_LOB_Act_valid)
  )

# Table S8 in Supplementary Materials: Number of trials ACROSS CONDITIONS in which infants looked and DID NOT look at the object during familarization
mean.overall.data %>%
  summarise(LOB=sum(trial_LOB_Act_valid),
            NLOB=sum(trial_valid) - sum(trial_LOB_Act_valid)
  )


#################################################################################################
# SECTION 2: DESCRIPTIVE DATA, VISUALIZATIONS, & ANALYSES
#################################################################################################

# We have preregistered three different measurement approaches to extract infants' surprise response from the
# eye-tracking raw data: (1) Overall looking time to the screen, (2) Overall looking time to the object,
# (3) First look at the object. The measures (1) and (3) are reported in the main manuscript. Measure (2) is
# reported in the Supplementary Materials.
#
# ANALYSIS: "MAIN FIXED EFFECTS FULL MODEL: The full model will include two interactions as main fixed effects:
# the interaction between the binary fixed effect variable communicative context (communicative referential: 1/0)
# and the binary location change variable (1/0), as well as an interaction between the binary fixed effect variable
# communicative context and the binary identity change variable (1/0).
#
# REDUCED MODEL: The reduced model includes communicative context and outcome as main fixed effect variables. Social context will be
# included as a binary variable (communicative referential: 1/0) and outcome will be included as two binary dummy
# variables (location change: 1/0; identity change: 1/0, no change condition: represented by the intercept).
#
# All factors will be tested within subjects. In both models, we will include fixed effect control variables for trial
# number (1-12), trial number within condition (1-2), and object position (up or down). We will include a full random
# effect structure in both models."

# -----------------------------------------------------------------------------------------------
# Measure (1): TOTAL LOOKING TIME TO THE SCREEN (Main preregistered measure)
# "Defined as the cumulative length of all fixations within the screen AOI beginning at the first
# frame of the curtain opening and ending when the infant looks away for 2 seconds or after 15 seconds elapse."
# -----------------------------------------------------------------------------------------------

# TOTAL LT TO THE SCREEN in the outcome phase

# Visualization of the descriptive data - TOTAL LOOKING TIME TO THE SCREEN

#Figure 4a in the main manuscript: Illustrating the means for each condition (6 data points per child)
mean.overall.data$Condition <- as.character(mean.overall.data$Condition)
Boxplot_3 <- ggplot(mean.overall.data, aes(Condition, LTScreenOut))
Boxplot_3 + geom_boxplot(fill=c("grey", "grey","grey", "white", "white", "white")) +
  scale_y_continuous(limits=c(0,15000), breaks= c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000, 13000, 14000, 15000), name="Mean proportion of looking time") + labs(x="Condition") +
  scale_fill_manual(values=c("blue", "darkgrey")) + geom_dotplot(binaxis="y",stackdir="center",stackratio=0.7,dotsize=0.5, fill=NA)+
  theme(axis.title.x = element_text(face="bold", size=15, vjust=-0.4, hjust=0.54),
        axis.text.x  = element_text(size=10,face="bold",vjust=0.7, color = "black"),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(face="bold", size=10, angle=90, vjust=5.0),
        axis.ticks.length=unit(.25, "cm"),
        panel.background = element_blank(),
        axis.line = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
  )

## Means and SDs of TOTAL LT TO THE SCREEN in the outcome phase (Okumura)
#Table 2 in the main manuscript: for mean.overall.data (analogously to the boxplots)
mean.overall.data %>%
  group_by(Condition) %>%
  summarise(mean=mean(LTScreenOut),
            sd=sd(LTScreenOut),
            na.rm = TRUE) %>%
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric, round, 3)

#for overall.data (analogously to the models)
overall.data %>%
  group_by(ConContext, Location_change, Identity_change) %>%
  summarise(mean=mean(LTScreenOut, na.rm=TRUE),
            sd=sd(LTScreenOut, na.rm=TRUE)) %>%
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric, round, 3)

# checking disctribution of the dependent variable
hist(overall.data$LTScreenOut)

# helper function increasing the number of iterations
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))

# z-tranform trial variables
overall.data$z.TrialRun <- scale(overall.data$TrialRun)
overall.data$z.TrialCon <- scale(overall.data$TrialCon)

# fitting the full model
full4 <- glmer(LTScreenOut ~ ConContext*Identity_change + ConContext*Location_change
               + z.TrialRun + z.TrialCon + ObjectPosAct
               + (1 + z.TrialRun + z.TrialCon + ObjectPosAct + ConContext | ID)
               , data=overall.data, family=Gamma(link=log), control=contr)

model.matrix(full4)

# fitting reduced model
red4 <- glmer(LTScreenOut ~ ConContext + Identity_change + Location_change
              + z.TrialRun + z.TrialCon + ObjectPosAct
              + (1 + z.TrialRun + z.TrialCon + ObjectPosAct + ConContext | ID)
              , data=overall.data, family=Gamma(link=log), control=contr)

# Comparing full model with reduced model
# "The comparison between the full model and the reduced model revealed a significant result indicating that at least
# one of the interactions had an impact on infants’ total looking time duration at the screen during the outcome phase
# (χ2(2) = 6.43, p = .04, see Figure 4a)."
as.data.frame(anova(full4, red4, test="Chisq"))

# More specifically, the interaction between third-party eye contact and identity change outcome had a significant effect
# on infants’ looking time response (χ2(1) = 6.40, p = .01, estimate = 0.30, SE = .12), with the looking times at identity
# change vs. no change outcomes being longer in the eye contact condition (M = 5036.75; SD = 2225.87) compared to the
# no eye contact condition (M = 3985.07; SD = 2137.05). The interaction between third-party eye contact and location change
# outcome did not have a significant effect (χ2(1) = 1.35, p = .24, estimate = 0.14, SE = .12). We found a significant effect
# of running trial (χ2(1) = 9.22, p = .002, estimate = −0.22, SE = .07) in that infants’ looking times decreased over time.
# The main effects of identity change (χ2(1) = 0.07, p = .79, estimate = 0.02, SE = .08), location change (χ2(1) = 0.80, p = .37,
# estimate = −0.08, SE = .09) , trial within condition (χ2(1) = 0.78, p = .38, estimate = −0.06, SE = .07) and object position
# (χ2(1) = 0.03, p = .86, estimate = −0.01, SE = .06) did not reveal a significant effect."

# Estimates and SDs
summary(full4)

# Significances
drop1(full4, scope = c("ConContext:Identity_change" ,
                      "ConContext:Location_change" ,
                      "ConContext" ,
                      "Identity_change" ,
                      "Location_change",
                      "z.TrialRun", "z.TrialCon", "ObjectPosAct"), test="Chisq")

# Table S10 in the Supplemental Materials: Conducting pairwise comparisons
means <- emmeans(full4, ~ Identity_change*Location_change|ConContext, adjust = "none")
means_selected <- means[c(1,2,3,5,6,7)]
summary(contrast(means_selected, method = "pairwise")[c(1,2,6,15,14,13)], adjust = "none")
#It requires a manual adjustment of the alpha level via bonferroni correction (calculating by 3)

# -----------------------------------------------------------------------------------------------
# Measure (2): TOTAL LOOKING TIME TO THE OBJECT (reported in the supplemental Materials)
# "Defined as the cumulative length of all fixations within the object AOI beginning at the first
# frame of the curtain opening and ending when the infant looks away for 2 seconds or after 15 seconds elapse."
# -----------------------------------------------------------------------------------------------

# Visualization of the TOTAL LOOKING TIME TO THE OBJECT (exploratory)

#Figure S7 in the Supplemental Materials: Illustrating the means for each condition (6 data points per child)
mean.overall.data$Condition <- as.character(mean.overall.data$Condition)
Boxplot_3 <- ggplot(mean.overall.data, aes(Condition, LTObjectOut))
Boxplot_3 + geom_boxplot(fill=c("grey", "grey","grey", "white", "white", "white")) +
  scale_y_continuous(limits=c(0,15000), breaks= c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000, 13000, 14000, 15000), name="Mean proportion of looking time") + labs(x="Condition") +
  scale_fill_manual(values=c("blue", "darkgrey")) + geom_dotplot(binaxis="y",stackdir="center",stackratio=0.7,dotsize=0.5, fill=NA)+
  theme(axis.title.x = element_text(face="bold", size=15, vjust=-0.4, hjust=0.54),
        axis.text.x  = element_text(size=10,face="bold",vjust=0.7, color = "black"),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(face="bold", size=10, angle=90, vjust=5.0),
        axis.ticks.length=unit(.25, "cm"),
        panel.background = element_blank(),
        axis.line = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
  )


## Means and SDs of TOTAL LT TO THE OBJECT in the outcome phase (Okumura)
#Table S9 in the Supplemental Materials: for mean.overall.data (analogously to the boxplots)
mean.overall.data %>%
  group_by(Condition) %>%
  summarise(mean=mean(LTObjectOut),
            sd=sd(LTObjectOut),
            na.rm = TRUE) %>%
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric, round, 3)

#for overall.data (analogously to the models)
overall.data %>%
  group_by(ConContext, Location_change, Identity_change) %>%
  summarise(mean=mean(LTObjectOut, na.rm=TRUE),
            sd=sd(LTObjectOut, na.rm=TRUE)) %>%
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric, round, 3)

# checking disctribution of the dependent variable
hist(overall.data$LTObjectOut)

# helper function increasing the number of iterations
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))

# z-tranform trial variables
overall.data$z.TrialRun <- scale(overall.data$TrialRun)
overall.data$z.TrialCon <- scale(overall.data$TrialCon)

# fitting the full model
full5 <- glmer(LTObjectOut ~ ConContext*Identity_change + ConContext*Location_change
               + z.TrialRun + z.TrialCon + ObjectPosAct
               + (1 + z.TrialRun + z.TrialCon + ObjectPosAct +ConContext | ID)
               , data=overall.data, family=Gamma(link=log), control=contr)

model.matrix(full5)

# fitting reduced model
red5 <- glmer(LTObjectOut ~ ConContext + Identity_change + Location_change
              + z.TrialRun + z.TrialCon + ObjectPosAct
              + (1 + z.TrialRun + z.TrialCon + ObjectPosAct + ConContext | ID)
              , data=overall.data, family=Gamma(link=log), control=contr)


# "The comparison between the full model and the reduced model revealed a significant result indicating that at least one of the interactions
# had an impact on infants’ total looking time duration at the screen during the outcome phase (χ2(2) = 11.12, p = .004, see Figure S6).

# Comparing full model with reduced model (when the full model is fitted appropriately)
as.data.frame(anova(full5, red5, test="Chisq"))

# "More specifically, the interaction between third-party eye contact and identity change outcome had a significant effect on infants’ looking time
# response (χ2(1) = 10.84, p = <.001, estimate = 0.44, SE = .13, with the looking time response at the identity change vs. no change outcome being
# longer in the eye contact condition (M = 4046.58; SD = 1991.95) compared to the no eye contact condition (M = 3056.63, SD = 1695.95). The interaction
# between third-party eye contact and location change outcome did not have a significant effect (χ2(1) = 1.72, p = .19, estimate = 0.18, SE = .14).
# We found a significant effect of running trial (χ2(1) = 7.45, p = .006, estimate = −0.22, SE = .07) in that infants’ looking time at the screen decreased over trials.
# Trial within condition (χ2(1) = 0.66, p = .42, estimate = 0.06, SE = .08) and object position (χ2(1) = 0.07, p = .80, estimate = −0.02, SE = .07)
# did not reveal a significant effect."

#Estimates & SDs
summary(full5)
#Significances
drop1(full5, scope = c("ConContext:Identity_change" ,
                       "ConContext:Location_change" ,
                       "ConContext" ,
                       "Identity_change" ,
                       "Location_change",
                       "z.TrialRun", "z.TrialCon", "ObjectPosAct"), test="Chisq")

# Table S12 in the Supplemental Materials: Conducting pairwise comparisons
means <- emmeans(full5, ~ Identity_change*Location_change|ConContext, adjust = "none")
means_selected <- means[c(1,2,3,5,6,7)]
summary(contrast(means_selected, method = "pairwise")[c(1,2,6,15,14,13)], adjust = "none")
#It requires a manual adjustment of the alpha level via bonferroni correction (calculating by 3)

# -----------------------------------------------------------------------------------------------
# Measure (3) FIRST LOOK at object
# "Defined as the time interval between the first fixation in the object AOI and the end of the
# last fixation within the same AOI, including the duration of saccades between fixations"
# -----------------------------------------------------------------------------------------------

# Visualization of the TOTAL LOOKING TIME TO THE OBJECT (exploratory)

#Figure 4b in the main manuscript: Illustrating the means for each condition (6 data points per child)
mean.overall.data$Condition <- as.character(mean.overall.data$Condition)
Boxplot_3 <- ggplot(mean.overall.data, aes(Condition, FirstLookDurationObjectOut))
Boxplot_3 + geom_boxplot(fill=c("grey", "grey","grey", "white", "white", "white")) +
  #scale_y_continuous(limits=c(0,15000), breaks= c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000,9000,10000,11000,12000,13000,14000,15000), name="Mean proportion of looking time") + labs(x="Condition") +
   scale_y_continuous(limits=c(0,7000), breaks= c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000), name="FirstLookDurationObjectOut_Third") +
    scale_fill_manual(values=c("blue", "darkgrey")) + geom_dotplot(binaxis="y",stackdir="center",stackratio=0.7,dotsize=0.5, fill=NA)+
  theme(axis.title.x = element_text(face="bold", size=15, vjust=-0.4, hjust=0.54),
        axis.text.x  = element_text(size=10,face="bold",vjust=0.7, color = "black"),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(face="bold", size=10, angle=90, vjust=5.0),
        axis.ticks.length=unit(.25, "cm"),
        panel.background = element_blank(),
        axis.line = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
  )

## Means and SDs of FIRST LOOK DURATION AT OBJECT in the outcome phase
#Table 2 in the main manuscript: for mean.overall.data (analogously to the boxplots)
mean.overall.data %>%
  group_by(Condition) %>%
  summarise(mean=mean(FirstLookDurationObjectOut),
            sd=sd(FirstLookDurationObjectOut),
            na.rm = TRUE) %>%
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric, round, 3)

#for overall.data (analogously to the models)
overall.data %>%
  group_by(ConContext, Location_change, Identity_change) %>%
  summarise(mean=mean(FirstLookDurationObjectOut, na.rm=TRUE),
            sd=sd(FirstLookDurationObjectOut, na.rm=TRUE)) %>%
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric, round, 3)

# checking disctribution of the dependent variable
hist(overall.data$FirstLookDurationObjectOut)

# helper function increasing the number of iterations
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))

# z-tranform trial variables
overall.data$z.TrialRun <- scale(overall.data$TrialRun)
overall.data$z.TrialCon <- scale(overall.data$TrialCon)

# fitting the full model
full6 <- glmer(FirstLookDurationObjectOut ~ ConContext*Identity_change + ConContext*Location_change
               + z.TrialRun + z.TrialCon + ObjectPosAct
               + (1 + z.TrialRun + z.TrialCon + ObjectPosAct + ConContext | ID)
               , data=overall.data, family=Gamma(link=log), control=contr)
model.matrix(full6)

# full model revealed a singular fit, with high SDs for TrialRun and Concontext (random effects). model with simpler random effect structure revealed similar estimates suggesting that the warning can be ignored
full6_2 <- glmer(FirstLookDurationObjectOut ~ ConContext*Identity_change + ConContext*Location_change
               + z.TrialRun + z.TrialCon + ObjectPosAct
               + (1 + z.TrialCon + ObjectPosAct | ID)
               , data=overall.data, family=Gamma(link=log), control=contr)
summary(full6_2)
summary(full6)

# fitting reduced model
red6 <- glmer(FirstLookDurationObjectOut ~ ConContext + Identity_change + Location_change
              + z.TrialRun + z.TrialCon + ObjectPosAct
              + (1 + z.TrialRun + z.TrialCon + ObjectPosAct + ConContext | ID)
              , data=overall.data, family=Gamma(link=log), control=contr)

# reduced model revealed a convergence warning, with high SDs for TrialRun and Concontext (random effects). model with simpler random effect structure revealed similar estimates suggesting that the warning can be ignored
red6_2 <- glmer(FirstLookDurationObjectOut ~ ConContext + Identity_change + Location_change
              + z.TrialRun + z.TrialCon + ObjectPosAct
              + (1 + z.TrialCon + ObjectPosAct | ID)
              , data=overall.data, family=Gamma(link=log), control=contr)

summary(red6_2)
summary(red6)

# "The comparison between the full model and the reduced model revealed a significant result (χ2(2) = 45.37, p = <.001, see Figure 4b)."
# Comparing full model with reduced model (when the full model is fitted appropriately)
as.data.frame(anova(full6, red6, test="Chisq"))

# "In addition, we found a main effect of eye contact condition (χ2(1) = 4.33, p = .04, estimate = −.17, SE = .08) in that the looking times
# at the object seemed significantly longer following the eye contact condition compared to the eye contact condition. However, this effect
# seemed to be mainly driven by the interaction between identity outcome, since the estimates of the main effect and the interaction pointed
# in opposite directions. In addition, we found a significant main effect of location change outcome (χ2(1) = 9.74, p = .001, estimate = .24, SE = .08),
# indicating that infants’ first look duration toward location changes was higher compared to the no change baseline, irrespective of the communicative context.
# The interaction between third-party eye contact and location change outcome did not have a significant effect (χ2(1) = 0.55, p = .46, estimate = 0.08, SE = .11).
# We did not find a significant main effect of identity change outcome (χ2(1) = 1.50, p = .22, estimate = 0.09, SE = .08), running trial (χ2(1) = 2.15, p = .14,
# estimate = −0.08, SE = .05), trial within condition (χ2(1) = 0.43, p = .51, estimate = 0.04, SE = .06) and object position (χ2(1) = 0.76, p = .38, estimate = −0.06, SE = .07).

# Estimates & SDs
summary(full6)

# Significances
drop1(full6, scope = c("ConContext:Identity_change" ,
                       "ConContext:Location_change" ,
                       "ConContext" ,
                       "Identity_change" ,
                       "Location_change",
                       "z.TrialRun", "z.TrialCon", "ObjectPosAct"), test="Chisq")

# Table S11 in the Supplemental Materials: Conducting pairwise comparisons
means <- emmeans(full6, ~ Identity_change*Location_change|ConContext, adjust = "none")
means_selected <- means[c(1,2,3,5,6,7)]
summary(contrast(means_selected, method = "pairwise")[c(1,2,6,15,14,13)], adjust = "none")
#It requires a manual adjustment of the alpha level via bonferroni correction (calculating by 3)


#################################################################################################
# SECTION 3: EXPLORATORY ANALYSES ACTION PHASE (NOT PRE-REG - reported in supplemental materials)
#################################################################################################

# -----------------------------------------------------------------------------------------------
# (a) Looking Times
# "We did not find condition differences in infants’ looking times at the overall screen (χ2(1) = 0.91, p = .33, estimate = −.04, SE = .04)
# and infants’ looking times at the object (χ2(1) = 0.03, p = .86, estimate = −.04, SE = .249.0). However, infants looked longer at the actors’
# faces (χ2(1) = 5.11, p = .02, estimate = −.10, SE = .04) in the eye contact condition compared to the no eye contact condition. In addition,
# we found a main effect of running trial, in that infants’ looking times at the actors’ faces decreased over trials (χ2(1) = 0.11, p = .02, estimate = −.10, SE = .04)."
# -----------------------------------------------------------------------------------------------

#################################################################################################
# overall attention to the screen during encoding
hist(overall.data$TotalLTScreenAct)

## Table S13 in the Suppelementary Materials: Means and SDs of TOTAL LT TO THE SCREEN in the action phase
mean.overall.data %>%
  group_by(ConditionCom) %>%
  summarise(mean=mean(TotalLTScreenAct),
            sd=sd(TotalLTScreenAct),
            na.rm = TRUE) %>%
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric, round, 3)

# helper function increasing the number of iterations
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))
# z-tranform trial variables
overall.data$z.TrialRun <- scale(overall.data$TrialRun)
overall.data$z.TrialCon <- scale(overall.data$TrialCon)

full1 <- glmer(TotalLTScreenAct ~ ConContext
               + z.TrialRun + z.TrialCon
               + (1 + z.TrialRun + z.TrialCon + ConContext | ID)
               , data=overall.data, family=Gamma(link=log), control=contr)

summary(full1)
drop1(full1, test="Chisq")

#################################################################################################
# Looking times to the object during encoding
hist(overall.data$TotalLTObjectAct)

## Table S13 in the Suppelementary Materials: Means and SDs of TOTAL LT TO THE Object in the action phase
mean.overall.data %>%
  group_by(ConditionCom) %>%
  summarise(mean=mean(TotalLTObjectAct, na.rm = TRUE),
            sd=sd(TotalLTObjectAct, na.rm = TRUE)) %>%
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric, round, 3)

# helper function increasing the number of iterations
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))
# z-tranform trial variables
overall.data$z.TrialRun <- scale(overall.data$TrialRun)
overall.data$z.TrialCon <- scale(overall.data$TrialCon)
overall.data$TotalLTObjectAct[overall.data$TotalLTObjectAct == 0] <- NA #replace NAs with 0 for averaging in the next steps

full1 <- lmer(TotalLTObjectAct ~ ConContext
               + z.TrialRun + z.TrialCon
               + (1 + z.TrialRun + z.TrialCon + ConContext | ID)
               , data=overall.data)

summary(full1)
drop1(full1, test="Chisq")

#################################################################################################
# Looking times to the faces during encoding
hist(overall.data$TotalLTFaceSumAct)

## Table S13 in the Suppelementary Materials: Means and SDs of TOTAL LT TO THE Faces in the action phase
mean.overall.data %>%
  group_by(ConditionCom) %>%
  summarise(mean=mean(TotalLTFaceSumAct),
            sd=sd(TotalLTFaceSumAct),
            na.rm = TRUE) %>%
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric, round, 3)

# helper function increasing the number of iterations
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))
# z-tranform trial variables
overall.data$z.TrialRun <- scale(overall.data$TrialRun)
overall.data$z.TrialCon <- scale(overall.data$TrialCon)

full1 <- glmer(TotalLTFaceSumAct ~ ConContext
               + z.TrialRun + z.TrialCon
               + (1 + z.TrialRun + z.TrialCon +  ConContext | ID)
               , data=overall.data, family=Gamma(link=log), control=contr)

summary(full1)
drop1(full1, test="Chisq")

# -----------------------------------------------------------------------------------------------
# (b) Influence of direct attention to the object during encoding on IDENTITY BIAS in subsequent outcome phase
# "We found that infants’ identity bias was not influenced by the looking time at the object in the action phase—neither when using the identity score based on
# the total looking time at the screen (χ2(1) = 0.73, p = .39, estimate = 236.1, SE = 281.0), nor when using the identity score based on the total looking time at
# the object (χ2(1) = 0.12, p = .73, estimate = −75.71, SE = 221.15), or the first look at the object (χ2(1) = 0.01, p = .90, estimate = 11.44, SE = 99.59).
# In line with the results of our preregistered main analysis, we found a significant main effect of eye contact condition in all three models, in that infants’
# identity bias was higher in the eye contact condition compared to the no eye contact condition."
# -----------------------------------------------------------------------------------------------

#create table allowing for identity bias analysis
mean.overall.data.NO <- subset(mean.overall.data, Condition=="1" | Condition=="4")
mean.overall.data.NO <- mean.overall.data.NO %>%
  rename(
    "LTScreenOutNO" = "LTScreenOut",
    "LTObjectOutNO" =  "LTObjectOut",
    "FirstLookDurationObjectOutNO" = "FirstLookDurationObjectOut"
  )

mean.overall.data.ID <- subset(mean.overall.data, Condition=="2" | Condition=="5")
mean.overall.data.ID <- mean.overall.data.ID %>%
  rename(
    "LTScreenOutID" = "LTScreenOut",
    "LTObjectOutID" =  "LTObjectOut",
    "FirstLookDurationObjectOutID" = "FirstLookDurationObjectOut"
  )

mean.overall.data.LO <- subset(mean.overall.data, Condition=="3" | Condition=="6")
mean.overall.data.LO <- mean.overall.data.LO %>%
  rename(
    "LTScreenOutLO" = "LTScreenOut",
    "LTObjectOutLO" =  "LTObjectOut",
    "FirstLookDurationObjectOutLO" = "FirstLookDurationObjectOut"
  )

mean.overall.data.bias <- mean.overall.data.NO
mean.overall.data.bias$LTScreenOutID <- mean.overall.data.ID$LTScreenOutID
mean.overall.data.bias$LTObjectOutID <- mean.overall.data.ID$LTObjectOutID
mean.overall.data.bias$FirstLookDurationObjectOutID <- mean.overall.data.ID$FirstLookDurationObjectOutID

mean.overall.data.bias$LTScreenOutLO <- mean.overall.data.LO$LTScreenOutLO
mean.overall.data.bias$LTObjectOutLO <- mean.overall.data.LO$LTObjectOutLO
mean.overall.data.bias$FirstLookDurationObjectOutLO <- mean.overall.data.LO$FirstLookDurationObjectOutLO

#Identity Bias
mean.overall.data.bias$IDBiasScreen <- mean.overall.data.bias$LTScreenOutID - mean.overall.data.bias$LTScreenOutNO
mean.overall.data.bias$IDBiasObject <- mean.overall.data.bias$LTObjectOutID - mean.overall.data.bias$LTObjectOutNO
mean.overall.data.bias$IDBiasFirst <- mean.overall.data.bias$FirstLookDurationObjectOutID - mean.overall.data.bias$FirstLookDurationObjectOutNO
#Location Bias
mean.overall.data.bias$LOBiasScreen <- mean.overall.data.bias$LTScreenOutLO - mean.overall.data.bias$LTScreenOutNO
mean.overall.data.bias$LOBiasObject <- mean.overall.data.bias$LTObjectOutLO - mean.overall.data.bias$LTObjectOutNO
mean.overall.data.bias$LOBiasFirst <- mean.overall.data.bias$FirstLookDurationObjectOutLO - mean.overall.data.bias$FirstLookDurationObjectOutNO

#################################################################################################
# Identity Bias condition difference based on Total Looking time at Screen
hist(mean.overall.data.bias$IDBiasScreen)
# helper function increasing the number of iterations
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))
# z-tranform trial variables
mean.overall.data.bias$z.TotalLTObjectAct <- as.vector(scale(mean.overall.data.bias$TotalLTObjectAct))

full1 <- lmer(IDBiasScreen ~ z.TotalLTObjectAct + ConditionCom +
               + (1 | ID)
               , data=mean.overall.data.bias)

drop1(full1, scope = c("z.TotalLTObjectAct","ConditionCom"), test="Chisq")
summary(full1)

# Table S14 in the Supplemental Materials
mean.overall.data.bias %>%
  group_by(ConditionCom) %>%
  summarise(mean=mean(IDBiasScreen),
            sd=sd(IDBiasScreen),
            na.rm = TRUE) %>%
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric, round, 3)

#################################################################################################
# Identity Bias condition difference based on Total Looking Time at Object
hist(mean.overall.data.bias$IDBiasObject)
# helper function increasing the number of iterations
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))
# z-tranform trial variables
mean.overall.data.bias$z.TotalLTObjectAct <- as.vector(scale(mean.overall.data.bias$TotalLTObjectAct))

full1 <- lmer(IDBiasObject ~ z.TotalLTObjectAct + ConditionCom +
                + (1 | ID)
              , data=mean.overall.data.bias)

drop1(full1, scope = c("z.TotalLTObjectAct","ConditionCom"), test="Chisq")
summary(full1)

# Table S14 in the Supplemental Materials
mean.overall.data.bias %>%
  group_by(ConditionCom) %>%
  summarise(mean=mean(IDBiasObject),
            sd=sd(IDBiasObject),
            na.rm = TRUE) %>%
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric, round, 3)

#################################################################################################
# Identity Bias condition difference based on First look at Object
hist(mean.overall.data.bias$IDBiasFirst)
# helper function increasing the number of iterations
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))
# z-tranform trial variables
mean.overall.data.bias$z.TotalLTObjectAct <- as.vector(scale(mean.overall.data.bias$TotalLTObjectAct))

full1 <- lmer(IDBiasFirst ~ z.TotalLTObjectAct + ConditionCom +
                + (1 | ID)
              , data=mean.overall.data.bias)

drop1(full1, scope = c("z.TotalLTObjectAct","ConditionCom"), test="Chisq")

summary(full1)

# Table S14 in the Supplemental Materials
mean.overall.data.bias %>%
  group_by(ConditionCom) %>%
  summarise(mean=mean(IDBiasFirst),
            sd=sd(IDBiasFirst),
            na.rm = TRUE) %>%
  as.data.frame(.) %>%
  dplyr::mutate_if(is.numeric, round, 3)

# -----------------------------------------------------------------------------------------------
# (c) Merged analyses including first-and third-party data (Reported in Main Manuscript)
# "To compare the data from Experiment 1 and 2, we repeated our main analyses for both outcome measures over a merged sample including infants
# from both experiments (N = 72). In addition to the fixed effects included in the main analysis, we included experiment (1 or 2) as fixed effect."
# -----------------------------------------------------------------------------------------------

#################################################################################################
# Based on Total Looking time at the Screen
# "Like in the separate analyses of the two experiments, infants’ total looking time at the screen varied as a function of eye contact and identity
# change outcome (effect of the interaction: χ2(1) = 9.01, p = .003, estimate = 0.27, SE = .09). In addition, we found a significant effect of
# running trial (χ2(1) = 9.03, p = .003, estimate = −0.19, SE = .06). We did not find significant effects of the interaction between eye contact and
# location change (χ2(1) = 0.46, p = .50, estimate = 0.06, SE = .09), experiment (χ2(1) = 0.04, p = .84, estimate = 0.02, SE = .10), main effect of
# location change (χ2(1) = 0.25, p = .62, estimate = −0.03, SE = .62), main effect of identity change (χ2(1) = 0.02, p = .88, estimate = 0.01, SE = .06),
# trial within condition (χ2(1) = 0.02, p = .89, estimate = −0.01, SE = .06), or object position (χ2(1) = 0.35, p = .56, estimate = −0.03, SE = .05)."
# identity bias was higher in the eye contact condition compared to the no eye contact condition."

# helper function increasing the number of iterations
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))
# z-tranform trial variables
merged.data$z.TrialRun <- scale(merged.data$TrialRun)
merged.data$z.TrialCon <- scale(merged.data$TrialCon)

fullTotal <- glmer(LTScreenOut ~ ConContext*Identity_change + ConContext*Location_change
               + Experiment + z.TrialRun + z.TrialCon + ObjectPosAct
               + (1 + z.TrialRun + z.TrialCon + ObjectPosAct + Experiment + ConContext | ID)
               , data=merged.data, family=Gamma(link=log), control=contr)

# estimates & SEs
summary(fullTotal)

#significances
drop1(fullTotal, scope = c("ConContext:Identity_change" ,
                           "Experiment",
                       "ConContext:Location_change" ,
                       "ConContext" ,
                       "Identity_change" ,
                       "Location_change",
                       "z.TrialRun", "z.TrialCon", "ObjectPosAct"), test="Chisq")

#################################################################################################
# Based on First Look at the Object
# "The merged analysis based on infants’ first look duration at the object also revealed a significant effect of the interaction between eye contact
# and identity outcome (χ2(1) = 42.54, p < .001, estimate = 0.52, SE = .08). In addition, we found significant main effects of identity change
# (χ2(1) = 13.47, p < .001, estimate = 0.20, SE = .05) and location change (χ2(1) = 25.28, p < .001, estimate = 0.28, SE = .06) indicating longer first
# look durations at both change outcomes compared to the baseline. We did not find significant effects of the interaction between eye contact and location
# change (χ2(1) = 0.01, p = .92, estimate = −0.01, SE = .08), experiment (χ2(1) = 2.27, p = .13, estimate = 0.17, SE = .11), trial within condition
# (χ2(1) = 1.10, p = 30, estimate = 0.04, SE = .04), running trial (χ2(1) = 3.38, p = .07, estimate = −0.07, SE = .04), or object position (χ2(1) = 1.02, p = .31,
# estimate = −0.05, SE = .05)."

# helper function increasing the number of iterations
contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))
# z-tranform trial variables
merged.data$z.TrialRun <- scale(merged.data$TrialRun)
merged.data$z.TrialCon <- scale(merged.data$TrialCon)

fullFirst <- glmer(FirstLookDurationObjectOut ~ ConContext*Identity_change + ConContext*Location_change
                   + Experiment + z.TrialRun + z.TrialCon + ObjectPosAct
                   + (1 + z.TrialRun + z.TrialCon + ObjectPosAct + Experiment + ConContext | ID)
                   , data=merged.data, family=Gamma(link=log), control=contr)

# estimates & SDs
summary(fullFirst)

# significances
drop1(fullFirst, scope = c("ConContext:Identity_change" ,
                           "Experiment",
                           "ConContext:Location_change" ,
                           "ConContext" ,
                           "Identity_change" ,
                           "Location_change",
                           "z.TrialRun", "z.TrialCon", "ObjectPosAct"), test="Chisq")
