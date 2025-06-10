#############################################################################################
#############################################################################################

# PREPARING THE TABLES FOR ANAYSES EXPERIMENT 2 (THIRD-PARTY)
# Based on pre-processed data from Stats_Third.R Script

# 27/03/2023

# **TABLE OF CONTENTS**
# SECTION 1: Create Table for Main Analysis (12 rows per participant, 2 per condition)
# SECTION 2: Create Table for Post Hoc Analyses (1 row per condition = 6 rows per participant)

#############################################################################################
#############################################################################################

# Clear workspace
rm(list = ls(all.names = TRUE)) # clears everything
graphics.off() # close all open graphics

# Set directories
recs_dir <- "./PreProcessedTables/"

# Load packages
library("ggplot2")
library("lme4")
library("exactRankTests")
library("emmeans")
library("tidyverse")
library("fs")

# reads all files in recs folder
flist <- list.files(recs_dir)

#############################################################################################
# SECTION 1: Create overall.data Table for Main Analysis (12 rows per participant, 2 per condition)
#############################################################################################

# -------------------------------------------------------------------------------------------
# STEP 1: Create new summary dataframe for overall data (chunking all individual files from preprocessed tables folder in one file)
# -------------------------------------------------------------------------------------------

# Read in tsv files from pre-processing folder
overall.data <- "PreProcessedTables"
tsv_files <- fs::dir_ls(overall.data, regexp = "\\.tsv$")
tsv_files

# Creating data frame
overall.data <- tsv_files %>%
  map(read_tsv) %>%
  reduce(rbind)

# make the "sex" column a factor
overall.data$Sex <- as.factor(revalue(overall.data$Sex, c("FALSE" = "F")))

# -------------------------------------------------------------------------------------------
# STEP 2: Filter the data in overall.data table according to preregistered inclusion criteria
# -------------------------------------------------------------------------------------------

# replace 0 with NA in outcome column
overall.data$LTScreenOut[overall.data$LTScreenOut == 0] <- NA #replace NAs with 0 for averaging in the next steps
overall.data$LTObjectOut[overall.data$LTObjectOut == 0] <- NA #replace NAs with 0 for averaging in the next steps
overall.data$FirstLookDurationObjectOut[overall.data$FirstLookDurationObjectOut == 0] <- NA #replace NAs with 0 for averaging in the next steps
# Filter 1: Exclude if child does not look at the screen for at least one fixation during the relevant phases
overall.data$LTScreenOut[overall.data$InterPhaseCheckerValid==FALSE] <- NA   #Write NA in outcome column if this trial is not valid (if child does not look at the screen in relevant phases)
overall.data$LTObjectOut[overall.data$InterPhaseCheckerValid==FALSE] <- NA
overall.data$FirstLookDurationObjectOut[overall.data$InterPhaseCheckerValid==FALSE] <- NA
# Filter 2: Has the child looked at the object for at least one fixation during outcome?
overall.data$trial_valid <-ifelse(overall.data$LTObjectOut > 0, 1, 0) # create trial_valid column, a trial is valid if LTObjectOut is >=0. due to the previous step exclusion do to insufficient looking in relevant phases is considered already.
overall.data$trial_valid[is.na(overall.data$trial_valid)] <- 0 #replace NAs with 0 for averaging in the next steps
# Dependent variable: Exclude trials during which child has not looked at object during outcome
overall.data$LTScreenOut[overall.data$trial_valid==0] <- NA   #Write NA in propLT column if this trial is not valid (if child does not look at the screen in relevant phases)
overall.data$FirstLookDurationObjectOut[overall.data$trial_valid==0] <- NA   #Write NA in propLT column if this trial is not valid (if child does not look at the screen in relevant phases)
overall.data$FirstLookDurationScreen[overall.data$trial_valid==0] <- NA   #Write NA in propLT column if this trial is not valid (if child does not look at the screen in relevant phases)
overall.data$LTObjectOut[overall.data$trial_valid==0] <- NA   #Write NA in propLT column if this trial is not valid (if child does not look at the screen in relevant phases)
overall.data$TotalLTObjectOut[overall.data$trial_valid==0] <- NA   #Write NA in propLT column if this trial is not valid (if child does not look at the screen in relevant phases)
overall.data$TotalLTScreenAct[overall.data$trial_valid==0] <- NA   #Write NA in propLT column if this trial is not valid (if child does not look at the screen in relevant phases)
overall.data$TotalLTObjectAct[overall.data$trial_valid==0] <- NA   #Write NA in propLT column if this trial is not valid (if child does not look at the screen in relevant phases)
overall.data$TotalLTActorSumAct[overall.data$trial_valid==0] <- NA   #Write NA in propLT column if this trial is not valid (if child does not look at the screen in relevant phases)
overall.data$TotalLTFaceSumAct[overall.data$trial_valid==0] <- NA   #Write NA in propLT column if this trial is not valid (if child does not look at the screen in relevant phases)

# -------------------------------------------------------------------------------------------
# STEP 3: select columns of interest, delete unnecessary columns, add relevant columns
# -------------------------------------------------------------------------------------------

# define columns of interest
coi <-
  c(
    "ID",
    "Sex",
    "Age_Days",
    "ConSoc",
    "ConOut",
    "Condition",
    "Dyad",
    "Object",
    "ObjectPos",
    "ObjectPosAct",
    "TrialRun",
    "TrialCon",
    "OutcomeDuration",
    "LTScreenOut",
    "LTObjectOut",
    "FirstLookDurationObjectOut",
    "FirstLookDurationScreen",
    "TotalLTScreenAct",
    "TotalLTObjectOut",
    "TotalLTObjectAct",
    "TotalLTActorSumAct",
    "TotalLTFaceSumAct",
    "trial_valid"
  )

# filter unnecessary columns from overall data set (include only cois)
overall.data <- overall.data[, coi]

# add dummy variables for main analysis
overall.data$ConContext <- ifelse(overall.data$ConSoc == 'COM', 1, 0)
overall.data$ObjectPos <- ifelse(overall.data$ObjectPos == 'OBEN', 1, 0)
overall.data$ObjectPosAct <- ifelse(overall.data$ObjectPosAct == 'OBEN', 1, 0)
overall.data$Identity_change <- ifelse(overall.data$ConOut == 'IDENTITY', 1, 0)
overall.data$Location_change <- ifelse(overall.data$ConOut == 'LOCATION', 1, 0)

# add column indicating whether infants have looked at the object during the encoding phase
overall.data$TotalLTObjectActNoNas <- overall.data$TotalLTObjectAct
overall.data$TotalLTObjectActNoNas[is.na(overall.data$TotalLTObjectActNoNas)] <- 0
overall.data$trial_LOB_Act <- ifelse(overall.data$TotalLTObjectActNoNas > 0, 1, 0)

# reorder columns in overall.data table
overall.data <- overall.data[, c("ID",
                                 "Sex",
                                 "Age_Days",
                                 "ConContext",
                                 "Identity_change",
                                 "Location_change",
                                 "ConSoc",
                                 "ConOut",
                                 "Condition",
                                 "ObjectPos",
                                 "ObjectPosAct",
                                 "TrialRun",
                                 "TrialCon",
                                 "LTScreenOut",
                                 "LTObjectOut",
                                 "FirstLookDurationObjectOut",
                                 "FirstLookDurationScreen",
                                 "TotalLTScreenAct",
                                 "TotalLTObjectAct",
                                 "TotalLTActorSumAct",
                                 "TotalLTFaceSumAct",
                                 "TotalLTObjectOut",
                                 "OutcomeDuration",
                                 "trial_valid",
                                 "trial_LOB_Act",
                                 "Dyad",
                                 "Object")]


#############################################################################################
# SECTION 2: Create mean.overall.data Table for Post Hoc Analyses (1 row per condition = 6 rows per participant)
#############################################################################################

subject_list <- unique(overall.data$ID)
condition_list <- c("1", "2", "3", "4", "5", "6")
mean.overall.data <- data.frame(matrix(NA, nrow = length(subject_list) * 6, ncol = 0), stringsAsFactors = FALSE)

mean.overall.data_ID_vector <- c()
mean.overall.data_Cond_vector <- c()
trial_valid_vector <- c()
trial_LOB_Act_vector <- c()
LTScreenOut_vector <- c()
FirstLookDurationObjectOut_vector <- c()
FirstLookDurationScreen_vector <- c()
LTObjectOut_vector <- c()
TotalLTObjectOut_vector <- c()
TotalLTScreenAct_vector <- c()
TotalLTObjectAct_vector <- c()
TotalLTActorSumAct_vector <- c()
TotalLTFaceSumAct_vector <- c()

# within current subject (i) and within current condition (j)
for (i in subject_list) {
  for (j in condition_list) {

    mean.overall.data_ID_vector <- c(mean.overall.data_ID_vector, i)
    mean.overall.data_Cond_vector <- c(mean.overall.data_Cond_vector, j)

    trial_valid_vector <- c(trial_valid_vector, sum(
      overall.data$trial_valid[which(overall.data$ID == i & overall.data$Condition == j)]))

    trial_LOB_Act_vector <- c(trial_LOB_Act_vector, sum(
      overall.data$trial_LOB_Act[which(overall.data$ID == i & overall.data$Condition == j)]))

    LTScreenOut_vector <- c(LTScreenOut_vector, mean(
      overall.data$LTScreenOut[which(overall.data$ID == i & overall.data$Condition == j)], na.rm = TRUE))

    FirstLookDurationObjectOut_vector <- c(FirstLookDurationObjectOut_vector, mean(
      overall.data$FirstLookDurationObjectOut[which(overall.data$ID == i & overall.data$Condition == j)], na.rm = TRUE))

    FirstLookDurationScreen_vector <- c(FirstLookDurationScreen_vector, mean(
      overall.data$FirstLookDurationScreen[which(overall.data$ID == i & overall.data$Condition == j)], na.rm = TRUE))

    LTObjectOut_vector <- c(LTObjectOut_vector, mean(
      overall.data$LTObjectOut[which(overall.data$ID == i & overall.data$Condition == j)], na.rm = TRUE))

    TotalLTObjectOut_vector <- c(TotalLTObjectOut_vector, mean(
      overall.data$TotalLTObjectOut[which(overall.data$ID == i & overall.data$Condition == j)], na.rm = TRUE))

    TotalLTScreenAct_vector <- c(TotalLTScreenAct_vector, mean(
      overall.data$TotalLTScreenAct[which(overall.data$ID == i & overall.data$Condition == j)], na.rm = TRUE))

    TotalLTObjectAct_vector <- c(TotalLTObjectAct_vector, mean(
      overall.data$TotalLTObjectAct[which(overall.data$ID == i & overall.data$Condition == j)], na.rm = TRUE))

    TotalLTActorSumAct_vector <- c(TotalLTActorSumAct_vector, mean(
      overall.data$TotalLTActorSumAct[which(overall.data$ID == i & overall.data$Condition == j)], na.rm = TRUE))

    TotalLTFaceSumAct_vector <- c(TotalLTFaceSumAct_vector, mean(
      overall.data$TotalLTFaceSumAct[which(overall.data$ID == i & overall.data$Condition == j)], na.rm = TRUE))
  }
}

mean.overall.data$ID <- mean.overall.data_ID_vector
mean.overall.data$Condition <- mean.overall.data_Cond_vector
mean.overall.data$ConditionCom <- ifelse(mean.overall.data$Condition < 4, 1, 0)
mean.overall.data$trial_valid <- trial_valid_vector
mean.overall.data$trial_LOB_Act_valid <- trial_LOB_Act_vector
mean.overall.data$LTScreenOut <- LTScreenOut_vector
mean.overall.data$FirstLookDurationObjectOut <- FirstLookDurationObjectOut_vector
mean.overall.data$FirstLookDurationScreen <- FirstLookDurationScreen_vector
mean.overall.data$LTObjectOut <- LTObjectOut_vector
mean.overall.data$TotalLTObjectOut <- TotalLTObjectOut_vector
mean.overall.data$TotalLTScreenAct <- TotalLTScreenAct_vector
mean.overall.data$TotalLTObjectAct <- TotalLTObjectAct_vector
mean.overall.data$TotalLTActorSumAct <- TotalLTActorSumAct_vector
mean.overall.data$TotalLTFaceSumAct <- TotalLTFaceSumAct_vector

#write tables (for further usage in stats script)
write.table(mean.overall.data, file="mean.overall.data.third", sep='\t', row.names = FALSE)
write.table(overall.data, file="overall.data.third", sep='\t', row.names = FALSE)



