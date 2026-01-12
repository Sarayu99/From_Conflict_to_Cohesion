This directory contains the code for the cross-sectional analyses. 

The comments from 5 subreddits 'aita', 'news', 'politics', 'science', 'worldnews' are pooled from January 2022 to June 2022. They data are divided into 26 weeks. The notebooks show illustrations for a given week/subset of a week. Similar code was replicated for all the weeks in the months of January, february, March, April, May and June. 

For each week/subset of a week, the following was performed-
* Step 1: For each week's comments, the aggressive language variables are computed using Detoxify. The following notebook is used - *cross_sectional_analyses/aggressiveLanguageDetection.ipynb*<br>
* Step 2: For each week's comments, the node embeddings are generated. The following notebook is used - *cross-sectional_analyses/structuralSimilarity.ipynb*<br>
Step 3: For each week's comments, the word embeddings data are generated using SBERT. The following notebook was used - *cross_sectional_analyses/semanticSimilarity.ipynb*<br>

Note: We proceed with a directed dyad modeling approach, i.e., each observation in the final cross-sectional data set is at the (sender, receiver,week,subreddit,thread) level. These observation form directed dyads because they are of the form (i,j,t,s,h)m which is different from (j,i,t,s,h).
