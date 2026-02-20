
# ==============================================================================
# PROJECT: From Conflict to Cohesion: Structural Similarity Dampens Uncivil 
#          Discourse in Polarized Social Groups
# SCRIPT:  2_R_script.R
# CODE AUTHOR(s):  Matthew Yeaton, Sarayu Anshuman
# ==============================================================================
# DESCRIPTION:
# This R script reads the entire data set at the sender, receiver, week, subreddit, thread level, 
# and generates some values for table 4, as well as tables 5 and 6.
#
# INPUTS: 
# - redditPanel_week_by_thread_de_identified.parquet
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


suppressMessages(library(arrow))
suppressMessages(library(data.table))


data_path2 <- "/home/sarayu_anshuman/redditdata/codes/sarayu_codes/Python_codes/generated_files_final/redditPanel_week_by_thread_de_identified.parquet"
data_path3 <- "/home/sarayu_anshuman/redditdata/codes/sarayu_codes/Python_codes/generated_files_final/redditPanel_week_structuralMeasures_all_withDirectedTox_deidentified.parquet"


# Note: Since our data is in .parquet format, we utilize the
# arrow package -> arrow::read_parquet
# ensure that arrows is in the environment
# install.packages("arrow")

redditPanel_week <- arrow::read_parquet(data_path2)
redditPanel_week3 <- arrow::read_parquet(data_path3)


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
            title = "Descriptive Statistics and Correlations Table")
}

#######################################################
#### Some Values for Table 4
#######################################################

#Note: for the other values to be generated, the comment level data is required

#Total number of observations
#Represents the (sender, receiver, week, subreddit, thread) level count
nrow(redditPanel_week)

#Number of unique authors
length(unique(redditPanel_week$author))

#Average value of Directed Toxicity
#Calculates the mean of 'directed_tox'
mean(redditPanel_week$directed_tox, na.rm = TRUE)

#######################################################
#### Table 5
#######################################################

subsetForCorrelogram = subset(redditPanel_week, select = c(directed_tox, networkSimilarity, languageSimilarity_commentLevel))
makeSummaryTable(subsetForCorrelogram)

#######################################################
#### Table 6
#######################################################

subsetForCorrelogram = subset(redditPanel_week3, select = c(networkSimilarity,sbmEquivalence,eigCentrSim,clusterSim,neighborOverlap,subredditOverlap))
makeSummaryTable(subsetForCorrelogram)
