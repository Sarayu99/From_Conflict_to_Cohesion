
# ==============================================================================
# PROJECT: From Conflict to Cohesion: Structural Similarity Dampens Uncivil 
#          Discourse in Polarized Social Groups
# SCRIPT:  4_R_script.R
# CODE AUTHOR(s):  Matthew Yeaton, Sarayu Anshuman
# ==============================================================================
# DESCRIPTION:
# This R script reads the various input scripts
# and generates the appendix tables
#
# INPUTS: 
# - redditPanel_week_by_thread_de_identified.parquet
# - redditPanel_week_drop_profanity_de_identified.parquet
# - redditPanel_week_by_thread_drop_nta_de_identified.parquet
# - redditPanel_week_structuralMeasures_all_withDirectedTox_deidentified.parquet
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

setFixest_nthreads(4)
setFixest_notes(FALSE)

to_odds_ratio <- function(model) {
  stopifnot(inherits(model, "fixest"))
  
  model_or <- model
  model_or$coefficients = exp(model_or$coefficients)
  model_or$se = model_or$coefficients * model_or$se
  
  ct   <- model_or$coeftable
  beta <- ct[, 1]
  se   <- ct[, 2]
  # delta-method transform
  est_t <- exp(beta)
  se_t  <- est_t * se
  # replace first two columns
  ct[, 1] <- est_t
  ct[, 2] <- se_t
  #colnames(ct)[1:2] <- c(label_est, label_se)
  model_or$coeftable = ct
  
  model_or
}

####################################################################################################################
# Robustness Analyses: Extreme Toxicity Models (A1)
####################################################################################################################

#import the data
redditPanel = read_parquet("/home/sarayu_anshuman/redditdata/codes/sarayu_codes/Python_codes/generated_files_final/redditPanel_week_by_thread_de_identified.parquet")
setDT(redditPanel)

# scale the variables
redditPanel$networkSimilarity = as.vector(scale(redditPanel$networkSimilarity))
redditPanel$languageSimilarity_commentLevel = as.vector(scale(redditPanel$languageSimilarity_commentLevel))
redditPanel$directed_tox = as.vector(scale(redditPanel$directed_tox))
# create a binary variable that is 1 if toxicityScore is above the 90th percentile
redditPanel$extremeToxicity = ifelse(redditPanel$toxicityScore > quantile(redditPanel$toxicityScore, 0.9), 1, 0)

gc()

m1l = feglm(extremeToxicity ~ networkSimilarity, redditPanel, cluster = c('author','receiver'), family = 'logit')
m1 = to_odds_ratio(m1l)
m2l = feglm(extremeToxicity ~ networkSimilarity + languageSimilarity_commentLevel, redditPanel, cluster = c('author','receiver'), family = 'logit')
m2 = to_odds_ratio(m2l)
m3l = feglm(extremeToxicity ~ networkSimilarity | author + receiver + week + subreddit, redditPanel, cluster = c('author','receiver'), family = 'logit')
m3 = to_odds_ratio(m3l)
m4l = feglm(extremeToxicity ~ networkSimilarity + languageSimilarity_commentLevel | author + receiver + week + subreddit, redditPanel, cluster = c('author','receiver'), family = 'logit')
m4 = to_odds_ratio(m4l)
m5l = feglm(extremeToxicity ~ networkSimilarity | dyad + week + subreddit, redditPanel, cluster = c('author','receiver'), family = 'logit')
m5 = to_odds_ratio(m5l)
m6l = feglm(extremeToxicity ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit, redditPanel, cluster = c('author','receiver'), family = 'logit')
m6 = to_odds_ratio(m6l)
etable(m1,m2,m3,m4,m5,m6, tex = FALSE, fitstat=c('n', 'pr2'), signif.code = c("***"=0.001,"**"=0.01, "*"=0.05),
       digits = 'r4', digits.stats = 'r4', cluster = c('author','receiver'),
       dict=c("toxicityScore" = "Toxicity", "networkSimilarity" = "Structural Similarity", "languageSimilarity_commentLevel" = 'Semantic Similarity',
              "author" = "Sender", "receiver" = "Receiver", "subreddit" = "Subreddit", "week" = "Week", "dyad" = "Dyad"))

# etable(m1l,m2l,m3l,m4l,m5l,m6l, tex = FALSE, fitstat=c('n', 'pr2'), signif.code = c("***"=0.001,"**"=0.01, "*"=0.05),
#        digits = 'r4', digits.stats = 'r4', cluster = c('author','receiver'),
#        dict=c("toxicityScore" = "Toxicity", "networkSimilarity" = "Structural Similarity", "languageSimilarity_commentLevel" = 'Semantic Similarity',
#               "author" = "Sender", "receiver" = "Receiver", "subreddit" = "Subreddit", "week" = "Week", "dyad" = "Dyad"))


####################################################################################################################
# Robustness Analyses: Exclusion of likly bots (Table A5)
####################################################################################################################
#import the data
redditPanel = read_parquet("/home/sarayu_anshuman/redditdata/codes/sarayu_codes/Python_codes/generated_files_final/redditPanel_week_by_thread_de_identified.parquet")
setDT(redditPanel)

gc()

# scale the variables
redditPanel$networkSimilarity = as.vector(scale(redditPanel$networkSimilarity))
redditPanel$languageSimilarity_commentLevel = as.vector(scale(redditPanel$languageSimilarity_commentLevel))
redditPanel$directed_tox = as.vector(scale(redditPanel$directed_tox))

gc()

qs = quantile(redditPanel$networkSimilarity, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
redditPanel[, networkSimilarityQ1 := as.integer(networkSimilarity < qs[1])]
redditPanel[, networkSimilarityQ2 := as.integer(networkSimilarity >= qs[1] & networkSimilarity < qs[2])]
redditPanel[, networkSimilarityQ3 := as.integer(networkSimilarity >= qs[2] & networkSimilarity < qs[3])]
redditPanel[, networkSimilarityQ4 := as.integer(networkSimilarity >= qs[3])]

# drop likely bots (likelyBot == 1)
redditPanel_noBots = subset(redditPanel, likelyBot == 0)
rm(redditPanel)
# run the models
m1 = feols(directed_tox ~ networkSimilarity , redditPanel_noBots, fixef.rm = 'singleton', cluster = c('author','receiver'))
m2 = feols(directed_tox ~ networkSimilarity + languageSimilarity_commentLevel , redditPanel_noBots, fixef.rm = 'singleton', cluster = c('author','receiver'))
m3 = feols(directed_tox ~ networkSimilarity | author + receiver + week + subreddit , redditPanel_noBots, fixef.rm = 'singleton', cluster = c('author','receiver'))
m4 = feols(directed_tox ~ networkSimilarity + languageSimilarity_commentLevel | author + receiver + week + subreddit , redditPanel_noBots, fixef.rm = 'singleton', cluster = c('author','receiver'))
m5 = feols(directed_tox ~ networkSimilarityQ4 + networkSimilarityQ3 + networkSimilarityQ2 + languageSimilarity_commentLevel | author + receiver + week + subreddit , redditPanel_noBots, fixef.rm = 'singleton', cluster = c('author','receiver'))
m6 = feols(directed_tox ~ networkSimilarity | dyad + week + subreddit, redditPanel_noBots, fixef.rm = 'singleton', cluster = c('author','receiver'))
m7 = feols(directed_tox ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit , redditPanel_noBots, fixef.rm = 'singleton', cluster = c('author','receiver'))
etable(m1,m2,m3,m4,m6,m7, tex = TRUE, cluster = c('author', 'receiver'), fitstat=c('n', 'r2'), signif.code = c("***"=0.001,"**"=0.01, "*"=0.05),
       digits = 'r4', digits.stats = 'r4',
       dict=c("toxicityScore" = "Toxicity", "networkSimilarity" = "Structural Similarity", "languageSimilarity_commentLevel" = 'Semantic Similarity',
              "author" = "Sender", "receiver" = "Receiver", "subreddit" = "Subreddit", "week" = "Week", "dyad" = "Dyad",
              "networkSimilarityQ2" = "Structural Similarity 25toMedian", "networkSimilarityQ3" = "Structural Similarity Medianto75",
              "networkSimilarityQ4" = "Structural Similarity greater75", "directed_tox" = "Directed Toxicity", "link_id" = "Thread"))

####################################################################################################################
# Robustness Analyses: dropping profanity, NTA, and alternate convex combinations of directedness (Table A6)
# ####################################################################################################################

redditPanel = read_parquet('/home/sarayu_anshuman/redditdata/codes/sarayu_codes/Python_codes/generated_files_final/redditPanel_week_drop_profanity_de_identified.parquet')

# scale the variables
redditPanel$networkSimilarity = as.vector(scale(redditPanel$networkSimilarity))
redditPanel$languageSimilarity_commentLevel = as.vector(scale(redditPanel$languageSimilarity_commentLevel))
redditPanel$directed_tox = as.vector(scale(redditPanel$directed_tox))
m1 = feols(directed_tox ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit , redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))

redditPanel = read_parquet('/home/sarayu_anshuman/redditdata/codes/sarayu_codes/Python_codes/generated_files_final/redditPanel_week_by_thread_drop_nta_de_identified.parquet')

# scale the variables
redditPanel$networkSimilarity = as.vector(scale(redditPanel$networkSimilarity))
redditPanel$languageSimilarity_commentLevel = as.vector(scale(redditPanel$languageSimilarity_commentLevel))
redditPanel$directed_tox = as.vector(scale(redditPanel$directed_tox))
m2 = feols(directed_tox ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit , redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))

redditPanel = read_parquet("/home/sarayu_anshuman/redditdata/codes/sarayu_codes/Python_codes/generated_files_final/redditPanel_week_by_thread_de_identified.parquet")

# scale the variables
redditPanel$networkSimilarity = as.vector(scale(redditPanel$networkSimilarity))
redditPanel$languageSimilarity_commentLevel = as.vector(scale(redditPanel$languageSimilarity_commentLevel))
redditPanel$directed_tox_25 = as.vector(scale(redditPanel$directed_tox_25))
redditPanel$directed_tox_50 = as.vector(scale(redditPanel$directed_tox_50))
redditPanel$directed_tox_75 = as.vector(scale(redditPanel$directed_tox_75))
m3 = feols(directed_tox_25 ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit , redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m4 = feols(directed_tox_50 ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit , redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m5 = feols(directed_tox_75 ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit , redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))

etable(m1,m2,m3,m4,m5, tex = TRUE, cluster = c('author', 'receiver'), fitstat=c('n', 'r2'), signif.code = c("***"=0.001,"**"=0.01, "*"=0.05),
       digits = 'r4', digits.stats = 'r4',
       dict=c("toxicityScore" = "Toxicity", "networkSimilarity" = "Structural Similarity", "languageSimilarity_commentLevel" = 'Semantic Similarity',
              "author" = "Sender", "receiver" = "Receiver", "subreddit" = "Subreddit", "week" = "Week", "dyad" = "Dyad",
              "networkSimilarityQ2" = "Structural Similarity 25toMedian", "networkSimilarityQ3" = "Structural Similarity Medianto75",
              "networkSimilarityQ4" = "Structural Similarity greater75", "directed_tox" = "Directed Toxicity", "link_id" = "Thread",
              "directed_tox_25" = "Directed Toxicity", "directed_tox_50" = "Directed Toxicity", "directed_tox_75" = "Directed Toxicity"))

####################################################################################################################
# Robustness Analyses: Alternative Toxicity Measures (Table A7)
####################################################################################################################
#import the data
redditPanel = read_parquet("/home/sarayu_anshuman/redditdata/codes/sarayu_codes/Python_codes/generated_files_final/redditPanel_week_by_thread_de_identified.parquet")

setDT(redditPanel)
setDT(redditPanel)

gc()

# scale the variables
redditPanel$networkSimilarity = as.vector(scale(redditPanel$networkSimilarity))
redditPanel$languageSimilarity_commentLevel = as.vector(scale(redditPanel$languageSimilarity_commentLevel))
redditPanel$directed_tox = as.vector(scale(redditPanel$directed_tox))
redditPanel$directed_severeToxicityScore = as.vector(scale(redditPanel$directed_severeToxicityScore))
redditPanel$directed_identityAttackScore = as.vector(scale(redditPanel$directed_identityAttackScore))
redditPanel$directed_insultScore = as.vector(scale(redditPanel$directed_insultScore))
redditPanel$directed_obsceneScore = as.vector(scale(redditPanel$directed_obsceneScore))
redditPanel$directed_threatScore = as.vector(scale(redditPanel$directed_threatScore))

gc()

m1 = feols(directed_tox ~ networkSimilarity + languageSimilarity_commentLevel |  author + receiver + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m2 = feols(directed_severeToxicityScore ~ networkSimilarity + languageSimilarity_commentLevel | author + receiver + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m3 = feols(directed_identityAttackScore ~ networkSimilarity + languageSimilarity_commentLevel | author + receiver + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m4 = feols(directed_insultScore ~ networkSimilarity + languageSimilarity_commentLevel | author + receiver + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m5 = feols(directed_obsceneScore ~ networkSimilarity + languageSimilarity_commentLevel | author + receiver + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m6 = feols(directed_threatScore ~ networkSimilarity + languageSimilarity_commentLevel | author + receiver + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
etable(m1,m2,m3,m4,m5,m6, tex = TRUE, cluster = c('author', 'receiver'), fitstat=c('n', 'r2'), signif.code = c("***"=0.001,"**"=0.01, "*"=0.05),
       digits = 'r4', digits.stats = 'r4',
       dict=c("directed_tox" = "Toxicity", "networkSimilarity" = "Structural Similarity", "languageSimilarity_commentLevel" = 'Semantic Similarity',
              "author" = "Sender", "receiver" = "Receiver", "subreddit" = "Subreddit", "week" = "Week", "dyad" = "Dyad",
              "directed_severeToxicityScore" = "Severe Toxicity", "directed_identityAttackScore" = "Identity Attack", "directed_insultScore" = "Insult",
              "directed_obsceneScore" = "Obscene", "directed_threatScore" = "Threat"))

rm(m1, m2, m3, m4, m5, m6)
gc()

m1d = feols(directed_tox ~ networkSimilarity + languageSimilarity_commentLevel |  dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m2d = feols(directed_severeToxicityScore ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m3d = feols(directed_identityAttackScore ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m4d = feols(directed_insultScore ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m5d = feols(directed_obsceneScore ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m6d = feols(directed_threatScore ~ networkSimilarity + languageSimilarity_commentLevel | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
etable(m1d,m2d,m3d,m4d,m5d,m6d, tex = TRUE, cluster = c('author', 'receiver'), fitstat=c('n', 'r2'), signif.code = c("***"=0.001,"**"=0.01, "*"=0.05),
       digits = 'r4', digits.stats = 'r4',
       dict=c("directed_tox" = "Toxicity", "networkSimilarity" = "Structural Similarity", "languageSimilarity_commentLevel" = 'Semantic Similarity',
              "author" = "Sender", "receiver" = "Receiver", "subreddit" = "Subreddit", "week" = "Week", "dyad" = "Dyad",
              "directed_severeToxicityScore" = "Severe Toxicity", "directed_identityAttackScore" = "Identity Attack", "directed_insultScore" = "Insult",
              "directed_obsceneScore" = "Obscene", "directed_threatScore" = "Threat"))

####################################################################################################################
# Robustness Analyses: ALternate network similarity measures TABLE A3
####################################################################################################################

#Note: there might be multicollinearity in the given model, so numerical instability in estimates is possible.

redditPanel = read_parquet('/home/sarayu_anshuman/redditdata/codes/sarayu_codes/Python_codes/generated_files_final/redditPanel_week_structuralMeasures_all_withDirectedTox_deidentified.parquet')
names(redditPanel)

# scale the variables
redditPanel$networkSimilarity = as.vector(scale(redditPanel$networkSimilarity))
redditPanel$languageSimilarity_commentLevel = as.vector(scale(redditPanel$languageSimilarity_commentLevel))
redditPanel$directed_tox = as.vector(scale(redditPanel$directed_tox))
redditPanel$sbmEquivalence_scaled = as.vector(scale(redditPanel$sbmEquivalence))
redditPanel$eigCentrSim = as.vector(scale(redditPanel$eigCentrSim))
redditPanel$clusterSim = as.vector(scale(redditPanel$clusterSim))
redditPanel$neighborOverlap = as.vector(scale(redditPanel$neighborOverlap))
redditPanel$subredditOverlap = as.vector(scale(redditPanel$subredditOverlap))
redditPanel$eigCenrAuthor = as.vector(scale(redditPanel$eigCenrAuthor))
redditPanel$eigCenrReceiver = as.vector(scale(redditPanel$eigCenrReceiver))
redditPanel$clusterAuthor = as.vector(scale(redditPanel$clusterAuthor))
redditPanel$clusterReceiver = as.vector(scale(redditPanel$clusterReceiver))

m1 = feols(directed_tox ~ networkSimilarity | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m2 = feols(directed_tox ~ sbmEquivalence_scaled | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m3 = feols(directed_tox ~ eigCentrSim | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton' , cluster = c('author','receiver'))
m4 = feols(directed_tox ~ clusterSim | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton' , cluster = c('author','receiver'))
m5 = feols(directed_tox ~ neighborOverlap | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m6 = feols(directed_tox ~ subredditOverlap | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m7 = feols(directed_tox ~ eigCenrAuthor | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m8 = feols(directed_tox ~ eigCenrReceiver | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m9 = feols(directed_tox ~ clusterAuthor | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m10 = feols(directed_tox ~ clusterReceiver | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m11 = feols(directed_tox ~ networkSimilarity + eigCenrAuthor + eigCenrReceiver + clusterAuthor + clusterReceiver | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
etable(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11, tex = TRUE, cluster = c('author', 'receiver'), fitstat=c('n', 'r2'), signif.code = c("***"=0.001,"**"=0.01, "*"=0.05),
       digits = 'r4', digits.stats = 'r4',
       dict=c("toxicityScore" = "Toxicity", "networkSimilarity" = "Structural Similarity", "languageSimilarity_commentLevel" = 'Semantic Similarity',
              "author" = "Sender", "receiver" = "Receiver", "subreddit" = "Subreddit", "week" = "Week", "dyad" = "Dyad",
              "sbmEquivalenceTRUE" = "SBM Similarity", "structuralEquivalenceTRUE" = "Structural Equivalence", "neighborOverlap" = "Neighbor Overlap",
              "linkOverlap" = "Thread Overlap", "subredditOverlap" = "Subreddit Overlap", "eigCentrSim" = "Eigencentr. Similarity",
              "eigCenrAuthor" = "Eigencentr. Author", "eigCenrReceiver" = "Eigencentr. Receiver", "clusterAuthor" = "Loc. Clustering Author",
              "clusterReceiver" = "Loc. Clustering Receiver"))

m2 = feols(directed_tox ~ sbmEquivalence_scaled | author + receiver + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
etable(m2, tex = TRUE, cluster = c('author', 'receiver'), fitstat=c('n', 'r2'), signif.code = c("***"=0.001,"**"=0.01, "*"=0.05),
       digits = 'r4', digits.stats = 'r4',
       dict=c("toxicityScore" = "Toxicity", "networkSimilarity" = "Structural Similarity", "languageSimilarity_commentLevel" = 'Semantic Similarity',
              "author" = "Sender", "receiver" = "Receiver", "subreddit" = "Subreddit", "week" = "Week", "dyad" = "Dyad",
              "sbmEquivalenceTRUE" = "SBM Similarity", "structuralEquivalenceTRUE" = "Structural Equivalence", "neighborOverlap" = "Neighbor Overlap",
              "linkOverlap" = "Thread Overlap", "subredditOverlap" = "Subreddit Overlap", "eigCentrSim" = "Eigencentr. Similarity",
              "eigCenrAuthor" = "Eigencentr. Author", "eigCenrReceiver" = "Eigencentr. Receiver", "clusterAuthor" = "Loc. Clustering Author",
              "clusterReceiver" = "Loc. Clustering Receiver"))

####################################################################################################################
# Robustness Analyses: structural sim. on alternate network sim. measures (Table A4)
####################################################################################################################

#Note: there might be multicollinearity in the given model, so numerical instability in estimates is possible.

m1 = feols(networkSimilarity ~ sbmEquivalence + eigCentrSim + clusterSim + neighborOverlap + subredditOverlap + eigCenrAuthor + eigCenrReceiver + clusterAuthor + clusterReceiver, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m2 = feols(networkSimilarity ~ sbmEquivalence + eigCentrSim + clusterSim + neighborOverlap + subredditOverlap + eigCenrAuthor + eigCenrReceiver + clusterAuthor + clusterReceiver  | author + receiver + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
m3 = feols(networkSimilarity ~ sbmEquivalence + eigCentrSim + clusterSim + neighborOverlap + subredditOverlap + eigCenrAuthor + eigCenrReceiver + clusterAuthor + clusterReceiver  | dyad + week + subreddit, redditPanel, fixef.rm = 'singleton', cluster = c('author','receiver'))
etable(m1,m2,m3, tex = FALSE, cluster = c('author', 'receiver'), fitstat=c('n', 'r2'), signif.code = c("***"=0.001,"**"=0.01, "*"=0.05),
       digits = 'r4', digits.stats = 'r4',
       dict=c("toxicityScore" = "Toxicity", "networkSimilarity" = "Structural Similarity", "languageSimilarity_commentLevel" = 'Semantic Similarity',
              "author" = "Sender", "receiver" = "Receiver", "subreddit" = "Subreddit", "week" = "Week", "dyad" = "Dyad",
              "sbmEquivalenceTRUE" = "Structural Equivalence (SBM)", "structuralEquivalenceTRUE" = "Structural Equivalence", "neighborOverlap" = "Neighbor Overlap",
              "clusterSim" = "Local Clustering Similarity", "subredditOverlap" = "Subreddit Overlap", "eigCentrSim" = "Eigenvector Similarity",
              "eigCenrAuthor" = "Eigencentr. Sender", "eigCenrReceiver" = "Eigencentr. Receiver", "clusterAuthor" = "Loc. Clustering Sender",
              "clusterReceiver" = "Loc. Clustering Receiver", "sbmEquivalence_scaled", "SBM Equivalence"))

m3 = feols(networkSimilarity ~ sbmEquivalence + eigCentrSim + clusterSim + neighborOverlap + subredditOverlap + eigCenrAuthor + eigCenrReceiver + clusterAuthor + clusterReceiver, redditPanel, cluster = c('author','receiver'))


collinearity(m3, verbose = 1) 

mSO = feols(networkSimilarity ~ subredditOverlap  | author + receiver + week, redditPanel, cluster = c('author','receiver'))
etable(mSO, tex = FALSE, cluster = c('author', 'receiver'), fitstat=c('n', 'r2'), signif.code = c("***"=0.001,"**"=0.01, "*"=0.05),
       dict=c("toxicityScore" = "Toxicity", "networkSimilarity" = "Structural Similarity", "languageSimilarity_commentLevel" = 'Semantic Similarity',
              "author" = "Sender", "receiver" = "Receiver", "subreddit" = "Subreddit", "week" = "Week", "dyad" = "Dyad",
              "sbmEquivalenceTRUE" = "SBM Equivalence", "structuralEquivalenceTRUE" = "Structural Equivalence", "neighborOverlap" = "Neighbor Overlap",
              "linkOverlap" = "Thread Overlap", "subredditOverlap" = "Subreddit Overlap", "eigCentrSim" = "Eigenvector Similarity",
              "eigCenrAuthor" = "Eigencentr. Author", "eigCenrReceiver" = "Eigencentr. Receiver", "clusterAuthor" = "Loc. Clustering Author",
              "clusterReceiver" = "Loc. Clustering Receiver", "sbmEquivalence_scaled", "SBM Equivalence"))


####################################################################################################################
# Robustness Analyses: (Table A2)
####################################################################################################################

# # Load the data
redditPanel = read_parquet('/home/sarayu_anshuman/redditdata/codes/sarayu_codes/Python_codes/generated_files_final/march2Weeks_subset_deidentified.parquet')


makeSummaryTable = function(num_data) {
  #-----------------------------
  # 1. Compute Descriptive Statistics & Correlations
  #-----------------------------
  means <- colMeans(num_data, na.rm = TRUE)
  sds   <- apply(num_data, 2, sd, na.rm = TRUE)
  
  # Compute the correlation matrix and associated p-values
  cor_results <- rcorr(as.matrix(num_data))
  cor_matrix  <- cor_results$r    # correlation coefficients
  p_matrix    <- cor_results$P    # p-values
  
  #-----------------------------
  # 2. Define a Helper Function for Significance Stars
  #-----------------------------
  getStars <- function(p) {
    if (p < 0.001) {
      return("***")
    } else if (p < 0.01) {
      return("**")
    } else if (p < 0.05) {
      return("*")
    } else {
      return("")
    }
  }
  
  #-----------------------------
  # 3. Build the Combined Table
  #-----------------------------
  # The final table will have:
  # - Column 1: "Mean (SD)" containing descriptive stats for each variable.
  # - Columns 2 to (n+1): Lower-triangular correlation coefficients (with stars) 
  #   for variables with indices less than the row variable; remaining cells are "----".
  
  n_vars <- ncol(num_data)
  # Create an empty matrix with (n_vars) rows and (n_vars + 1) columns.
  combined_table <- matrix("", nrow = n_vars, ncol = n_vars + 1)
  
  # Set column names: first column for descriptive stats, then one for each variable.
  colnames(combined_table) <- c("Mean (SD)", colnames(num_data))
  rownames(combined_table) <- colnames(num_data)
  
  roundDigits = 3
  # Fill the first column with descriptive statistics in the format "mean (sd)"
  for (i in 1:n_vars) {
    combined_table[i, 1] <- paste0(round(means[i], roundDigits), " (", round(sds[i], roundDigits), ")")
  }
  
  # Fill the correlation part:
  # For each row i and for each variable j:
  # - If i > j (i.e. lower triangle), fill with the correlation coefficient plus significance stars.
  # - Otherwise (i <= j), fill with "----"
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      # In the combined table, correlation columns start at column index 2.
      if (i > j) {
        stars <- getStars(p_matrix[i, j])
        combined_table[i, j + 1] <- paste0(round(cor_matrix[i, j], roundDigits), stars)
      } else {
        combined_table[i, j + 1] <- "-"
      }
    }
  }
  
  # Convert the matrix to a data frame for stargazer
  combined_table_df <- as.data.frame(combined_table)
  
  #-----------------------------
  # 4. Output the Combined Table using stargazer
  #-----------------------------
  stargazer(combined_table_df, type = "text", summary = FALSE, rownames = TRUE,
            title = "Robustness Analysis for Different Versions of the Structural Similarity Measure")
}

subsetForCorrelogram = subset(redditPanel, select = c(networkSimilarity_q0p5,networkSimilarity_q1,networkSimilarity_q2,networkSimilarity))
makeSummaryTable(subsetForCorrelogram)
