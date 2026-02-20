# ==============================================================================
# PROJECT: From Conflict to Cohesion: Structural Similarity Dampens Uncivil 
#          Discourse in Polarized Social Groups
# SCRIPT:  5_R_script.R
# CODE AUTHOR(s):  Matthew Yeaton, Sarayu Anshuman
# ==============================================================================
# DESCRIPTION:
# This R script reads the various input scripts
# and generates table 8 and the figure 6
#
# INPUTS: 
# - nat_exp_deidentified.parquet
# - nat_exp_full_deidentified.parquet
#
# OUTPUTS: 
# - Rplots.pdf
#
# NOTE
# - Please change file paths as per required
# ==============================================================================

#######################################################
#### Read the data
#######################################################

# Load libraries
library(arrow)
#library(showtext)
library(data.table)
library(plyr)
library(dplyr)
library(stargazer)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(pscl)
library(lmtest)
library(plm)
library(parallel)
#library(ivreg)
library(car)
#library(cragg)
library(fixest)
library(sjPlot)
library(ggplot2)
library(parameters)
library(marginaleffects)
library(psych)      # For descriptive statistics
library(Hmisc)      # For rcorr to compute correlations with p-values
library(stargazer)  # For formatted table output

#cat('fixest version')
#packageVersion("fixest")

############################################################################################

###########################################################################################
####################################################################################################################
# Table 8: diff-in-diff analysis for natural experiment shifting group polarization
####################################################################################################################

postsClean = read_parquet('/data/nat_exp_deidentified.parquet')


m1 = feols(directed_tox ~ networkSimilarity | author + receiver, postsClean, fixef.rm = 'singleton')
m2 = feols(directed_tox ~ networkSimilarity + languageSimilarity | author + receiver, postsClean, fixef.rm = 'singleton')
m3 = feols(directed_tox ~ networkSimilarity + languageSimilarity + treatment + post | author + receiver, postsClean, fixef.rm = 'singleton')
m4 = feols(directed_tox ~ networkSimilarity*treatment*post + languageSimilarity | author + receiver, postsClean, fixef.rm = 'singleton')
etable(m1,m2,m3,m4, tex = FALSE, cluster = c('author', 'receiver'), fitstat=c('n', 'r2'), signif.code = c("***"=0.001,"**"=0.01, "*"=0.05),
       digits = 'r3', digits.stats = 'r3',
       dict=c("toxicityScore" = "Toxicity", "networkSimilarity" = "Structural Similarity", "languageSimilarity" = 'Semantic Similarity',
              "post" = "Post-Interview", "treatment" = "Antiwork Stayers", "author" = "Sender", "receiver" = "Receiver",
              "participant_1" = "Participant 1", "participant_2" = "Participant 2", "directed_tox" = "Directed Toxicity"))


# ####################################################################################################################
# # Figure 6: event study representation for natural experiment shifting group polarization
####################################################################################################################

data_TreatControl = read_parquet('/data/nat_exp_full_deidentified.parquet')

# rename the variable author to sender
data_TreatControl <- rename(data_TreatControl, sender = author)

data_TreatControl$week_1 <- ifelse(data_TreatControl$week == "week1", -3,
                                     ifelse(data_TreatControl$week == "week2", -2,
                                            ifelse(data_TreatControl$week == "week3", -1, 
                                                   ifelse(data_TreatControl$week == "week4", 1,
                                                          ifelse(data_TreatControl$week == "week5",2,
                                                                 ifelse(data_TreatControl$week == "week6", 3,"NA"))))))

m1 = feols(directed_tox ~ treatment*networkSimilarity*week | sender + receiver, data_TreatControl, fixef.rm = 'singleton',cluster=c("sender","receiver"))
slopeData = plot_slopes(
  model = m1,
  variable = "networkSimilarity",
  by = c("week", "treatment"),
  conf_level = 0.95,
  draw = FALSE,
)

slopeData2 = slopeData[slopeData$week != 'week1',]
# convert the slope data into a ggplot plot
slopeData2$treatment = factor(slopeData2$treatment, levels = c("1", "0"), labels = c("Treatment", "Control"))
slopeData2$week = factor(slopeData2$week, levels = c("week2", "week3", "week0", "week4", "week5", "week6"))
slopeData2 <- bind_rows(slopeData2, data.frame(week = factor("week0", levels = c("week2", "week3", "week0", "week4", "week5","week6")), estimate = NA, treatment = "Control"))
slopeData2 <- bind_rows(slopeData2, data.frame(week = factor("week0", levels = c("week2", "week3", "week0", "week4", "week5","week6")), estimate = NA, treatment = "Treatment"))
slopeData2$treatment = factor(slopeData2$treatment, levels = c("Treatment", "Control"))

dodge <- position_dodge(width = 0.15)

ggplot(slopeData2, aes(x = week, y = estimate, color = treatment, group = treatment)) +
  geom_line(position=dodge, linewidth = 1, data=slopeData2[!is.na(slopeData2$estimate),]) +
  geom_point(position=dodge) +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high), width = 0.6, linewidth = 0.8,position=dodge) +
  labs(
    x = "Event Time (Week)",
    y = "Marginal Effect of Structural Sim. on Directed Tox.",
    color = "",
    fill = ""
  ) +
  scale_x_discrete(
    labels = c("week2" = "-2", "week3" = "-1", "week0" = "0",
               "week4" = "1", "week5" = "2", "week6" = "3"),
    drop = FALSE
  ) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "black") +
  scale_color_manual(
    values = c("Treatment" = "#1f77b4", "Control" = "darkgrey")
  ) +
  theme_minimal() +
  theme(text = element_text(family = "")
)


