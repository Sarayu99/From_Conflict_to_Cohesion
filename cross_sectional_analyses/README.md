This directory contains the code for the cross-sectional analyses. 

The comments from 5 subreddits 'aita', 'news', 'politics', 'science', 'worldnews' are pooled from January 2022 to June 2022. They data are divided into 26 weeks. The programs shown are for three weeks. Similar code was replicated for all the weeks in the months of January, february, March, April, May and June. 

For each week, the following was performed-
Step 1: For each week, the aggressive language variables are computed using Detoxify. The following code file is used-
  * cross_sectional_analyses/MayJune2022Subreddits_AggressiveLanguage.ipynb
Step 2: For each week, the aggressive language dictionary obtained from the previous step is processed to create a seperate column for each toxicity output, and then node embeddings are created. The following code file is used-
  * cross-sectional_analyses/MayJuneSubreddits2022_network_Similarity.ipynb
Step 3: For each week, the languague similarity is calculated using SBERT. The following code file was used-
  * cross_sectional_analyses/MayJune2022Subreddits_Language_Similarity.ipynb

Note: We proceed with a directed dyad modeling approach, i.e., each observation in the final cross-sectional data set is at the (sender, receiver,week,subreddit,thread) level. These observation form directed dyads because they are of the form (i,j,t,s,h). 
