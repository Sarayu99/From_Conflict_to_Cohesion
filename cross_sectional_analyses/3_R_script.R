
# ==============================================================================
# PROJECT: From Conflict to Cohesion: Structural Similarity Dampens Uncivil 
#          Discourse in Polarized Social Groups
# SCRIPT:  3_R_script.R
# CODE AUTHOR(s):  Matthew Yeaton, Sarayu Anshuman
# ==============================================================================
# DESCRIPTION:
# This R script reads the entire data set at the sender, receiver, week, subreddit, thread level, 
# and generates some table 7
#
# INPUTS: 
# - redditPanel_week_by_thread_de_identified.parquet
#
# OUTPUTS: 
# - no output file generated
#
# NOTE
# - Please change file paths as per required
# ==============================================================================



#######################################################
#### Read the data
#######################################################

# Load libraries
library(arrow)
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
#library(marginaleffects)
library(psych)      # For descriptive statistics
library(Hmisc)      # For rcorr to compute correlations with p-values
library(stargazer)  # For formatted table output


cat('fixest version')
packageVersion("fixest")

# import data
redditPanel = read_parquet("/home/sarayu_anshuman/redditdata/codes/sarayu_codes/Python_codes/generated_files_final/redditPanel_week_by_thread_de_identified.parquet")

# scale the variables
redditPanel$networkSimilarity = as.vector(scale(redditPanel$networkSimilarity))
redditPanel$languageSimilarity_commentLevel = as.vector(scale(redditPanel$languageSimilarity_commentLevel))
redditPanel$directed_tox = as.vector(scale(redditPanel$directed_tox))

# create four variables that categorize the networkSimilarity by quartile
redditPanel$networkSimilarityQ1 = ifelse(redditPanel$networkSimilarity < quantile(redditPanel$networkSimilarity, 0.25), 1, 0)
redditPanel$networkSimilarityQ2 = ifelse(redditPanel$networkSimilarity >= quantile(redditPanel$networkSimilarity, 0.25) & redditPanel$networkSimilarity < quantile(redditPanel$networkSimilarity, 0.5), 1, 0)
redditPanel$networkSimilarityQ3 = ifelse(redditPanel$networkSimilarity >= quantile(redditPanel$networkSimilarity, 0.5) & redditPanel$networkSimilarity < quantile(redditPanel$networkSimilarity, 0.75), 1, 0)
redditPanel$networkSimilarityQ4 = ifelse(redditPanel$networkSimilarity >= quantile(redditPanel$networkSimilarity, 0.75), 1, 0)

cat('variables created')

####################################################################################################################
# Table 7: main models with sender, receiver, week, subreddit, thread unit of analysis: subreddit FEs
####################################################################################################################
m1 = feols(directed_tox ~ networkSimilarity , redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m2 = feols(directed_tox ~ networkSimilarity + languageSimilarity_commentLevel , redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m3 = feols(directed_tox ~ networkSimilarity | author + receiver + week + subreddit , redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m4 = feols(directed_tox ~ networkSimilarity + languageSimilarity_commentLevel | author + receiver + week + subreddit , redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m5 = feols(directed_tox ~ networkSimilarityQ4 + networkSimilarityQ3 + networkSimilarityQ2 + languageSimilarity_commentLevel | author + receiver + week + subreddit , redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m6 = feols(directed_tox ~ networkSimilarity | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m7 = feols(directed_tox ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit , redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
etable(m1,m2,m3,m4,m5,m6,m7, tex = FALSE, cluster = c('author', 'receiver'), fitstat=c('n', 'r2'), signif.code = c("***"=0.001,"**"=0.01, "*"=0.05),
       digits = 'r4', digits.stats = 'r4',
       dict=c("toxicityScore" = "Toxicity", "networkSimilarity" = "Structural Similarity", "languageSimilarity_commentLevel" = 'Semantic Similarity',
              "author" = "Sender", "receiver" = "Receiver", "subreddit" = "Subreddit", "week" = "Week", "dyad" = "Dyad",
              "networkSimilarityQ2" = "Structural Similarity 25toMedian", "networkSimilarityQ3" = "Structural Similarity Medianto75",
              "networkSimilarityQ4" = "Structural Similarity greater75", "directed_tox" = "Directed Toxicity", "link_id" = "Thread"))



