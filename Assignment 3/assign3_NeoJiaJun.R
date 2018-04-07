# =======================================================
# Problem 1 : Starter Code : JSON to Document-Term Matrix
# =======================================================

# Install essential packages
install.packages("jsonlite")    # read JSON file in R
install.packages("tm")          # text mining in R
install.packages("NbClust")

# Load essential packages
library(jsonlite)
library(tm)
library(cluster)
library(SnowballC)
library(tidyr)
library(factoextra)



# -------------------------------------------------------
# Cuisine Data : Loading and Pre-Processing

# Load the dataset from JSON format
cuisData <- read_json("assign3_CuisineData.json", simplifyVector = TRUE)

# Inspect the first few elements
head(cuisData)

# Remove the c() wrapper from the list of ingredients
cuisData$ingredients <- substring(cuisData$ingredients, 3, nchar(cuisData$ingredients)-1)

# Remove anything present in brackets (mostly explanations and amounts) 
cuisData$ingredients <- gsub("\\s*\\([^\\)]+\\)","",cuisData$ingredients)

# Remove the space following the comma between ingredients
cuisData$ingredients <- gsub(', ',',',cuisData$ingredients)

# Remove the space between two words of the same ingredient
cuisData$ingredients <- gsub(' ','',cuisData$ingredients)

# Remove any hyphen present within the same ingredient
cuisData$ingredients <- gsub('-','',cuisData$ingredients)

# Inspect the first few elements
head(cuisData)


# -------------------------------------------------------
# Cuisine Data : Converting to Document-Term Matrix
# Ref : https://www.youtube.com/watch?v=dE10fBCDWQc

# Create the corpus of terms (dictionary) for ingredients
cuisCorpus <- Corpus(VectorSource(cuisData$ingredients))

# Inspect the first few elements
inspect(cuisCorpus[1:5])

# Translate all letters to lower case
cuisCorpus <- tm_map(cuisCorpus, tolower)


# Further cleaning and preprocessing guide:
# https://www.youtube.com/watch?v=3putwMZpt1E

# Create the DTM from the Corpus
cuisDTM <- DocumentTermMatrix(cuisCorpus)

# Inspect the first few elements
inspect(cuisDTM[1:5,])

# Create the DTM weighting with TF-IDF from the Corpus 
cuisDTM_TFIDF <- DocumentTermMatrix(cuisCorpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
inspect(cuisDTM_TFIDF[1:5, ])

min(cuisDTM_TFIDF)
max(cuisDTM_TFIDF)

# Min Value of Weightage is 0
# Max Value of Weightage is 28.55908

# Create DataFrame from the DTM_TFIDF
cuisDF <- as.data.frame(as.matrix(cuisDTM_TFIDF))

new_df <- data.frame(names(cuisDF[1]), cuisDF[1,1], stringsAsFactors = FALSE)
colnames(new_df) <- c("Ingredients", "TFIDF")
str(new_df)

for (i in 2:length(cuisDF)){
  for (j in 1:length(cuisDF[i,])){
    if(cuisDF[i,j] != 0){
      new_df <- rbind(new_df, c(names(cuisDF[i]), cuisDF[i,j], stringsAsFactors = FALSE))
      break
    }
  }
}

new_df$TFIDF <- as.numeric(as.character(new_df$TFIDF))

# Inspect the DataFrame
str(new_df)
write.csv(new_df, file = "TFIDF.csv")

# Top 20 most important ingredients is at 9.95
ingredients_to_remove <- new_df[which(new_df$TFIDF <= 9.95),]
head(ingredients_to_remove)

final_df <- cuisDF[, -which(names(cuisDF) %in% c(ingredients_to_remove$Ingredients))]
final_df <- final_df[-which(rowSums(final_df) < 10), ]

# Removed a huge number of rows to 489 objects and 43 variables

## Now we should be able to do our cluster!

#######################################################
# K Means                                             #
#######################################################

fviz_nbclust(final_df, kmeans, method = "gap_stat")

# Suggested number of cluster is 6
set.seed(101)
final_df.res <- kmeans(final_df, 6, nstart = 25)
fviz_cluster(final_df.res, data = final_df,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# I created the 6 cluster and based on the cluster plot, there are 6 estimated of 6 cuisine based on these rows 


#########################################################
# Hierarchical Cluster                                  #
#########################################################

final_df.hc <- final_df %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

fviz_dend(final_df.hc, k = 6, # Cut in four groups
          cex = 0.2, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#8B008B", "#F0E68C"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

# There are 6 hierarchical cluster as well, I decided to cut it to 6 groups since its the optimal number of cluster
# Definitely, I will choose the hierarchical cluster to compare the distance and 
# Whether is the ingredients expected to be in the similar cluster or not.

#########################################################################################################################
# Question 2                                                                                                            #
#########################################################################################################################

# read the csv file

responses_df <- read.csv("responses.csv")

# EDA 
dim(responses_df)
head(responses_df[1,])

# Based on these data, I can see that there are a few categories

# Music (Jazz, Opera)
# Movies (Action, War)
# Interest (History, Pets)
# Fears (Flying, Spiders, Dangerous Dogs)
# Health (Smoking, Alcohol, Healhty eating)

# There's Non-Applicable Value in the dataframe
responses_df <- na.omit(responses_df)
responses_df$Gender <- as.numeric(as.factor(responses_df$Gender))
str(responses_df)

# Remove String Columns 
# Interest in Music
responses_music_df <- responses_df[, 3:19]
responses_music_df_with_gender  <- cbind(responses_music_df, Gender=(responses_df$Gender))
str(responses_music_df)
str(responses_music_df_with_gender)


# Interest in Movies 
responses_movies_df <- responses_df[,21:31]
responses_movies_df_with_gender <- cbind(responses_movies_df, Gender=(responses_df$Gender))
str(responses_movies_df)
str(responses_movies_df_with_gender)

# Interests 
responses_interests_df <- responses_df[, 32:63]
responses_interests_df_with_gender  <- cbind(responses_interests_df, Gender=(responses_df$Gender))
str(responses_interests_df)
str(responses_interests_df_with_gender)


# Fears
responses_fear_df <- responses_df[, 65:73]
responses_fear_df_with_gender  <- cbind(responses_fear_df, Gender=(responses_df$Gender))
str(responses_fear_df)
str(responses_fear_df_with_gender)


#######################################################
# K Means                                             #
#######################################################

# Optimal Number Of Clusters for Music Without Gender
fviz_nbclust(responses_music_df, kmeans, method = "gap_stat")
set.seed(101)
responses_music_df.res <- kmeans(responses_music_df, 3, nstart = 25)
fviz_cluster(responses_music_df.res, data = responses_music_df,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# Optimal Number Of Clusters for Music With Gender
fviz_nbclust(responses_music_df_with_gender, kmeans, method = "gap_stat")
set.seed(101)
responses_music_df_with_gender.res <- kmeans(responses_music_df, 3, nstart = 25)
fviz_cluster(responses_music_df_with_gender.res, data = responses_music_df_with_gender,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# We can see minimal effect on the cluster when gender is added into play for 
# Music

# Optimal Number of Clusters for Movies
fviz_nbclust(responses_movies_df, kmeans, method = "gap_stat")
set.seed(101)
responses_movies_df.res <- kmeans(responses_movies_df, 3, nstart = 25)
fviz_cluster(responses_movies_df.res, data = responses_movies_df,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

fviz_nbclust(responses_movies_df_with_gender, kmeans, method = "gap_stat")
set.seed(101)
responses_movies_df_with_gender.res <- kmeans(responses_movies_df_with_gender, 3, nstart = 25)
fviz_cluster(responses_movies_df_with_gender.res, data = responses_movies_df_with_gender,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# Again, we can see minimal effect on the cluster when gender is added into play for 
# Music

# Optimal Number of Clusters for Interests 
fviz_nbclust(responses_interests_df, kmeans, method = "gap_stat")
set.seed(101)
responses_interests_df.res <- kmeans(responses_interests_df, 6, nstart = 25)
fviz_cluster(responses_interests_df.res, data = responses_interests_df,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

fviz_nbclust(responses_interests_df_with_gender, kmeans, method = "gap_stat")
responses_interests_df_with_gender.res <- kmeans(responses_interests_df_with_gender, 6, nstart = 25)
fviz_cluster(responses_interests_df_with_gender.res, data = responses_interests_df_with_gender,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# Again, we can see minimal effect on the cluster when gender is added into play for 
# Interest

# Optimal Number of Clusters for Fear
fviz_nbclust(responses_fear_df, kmeans, method = "gap_stat")
set.seed(101)
responses_fear_df.res <- kmeans(responses_fear_df, 5, nstart = 25)
fviz_cluster(responses_fear_df.res, data = responses_fear_df,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())


fviz_nbclust(responses_fear_df_with_gender, kmeans, method = "gap_stat")
set.seed(101)
responses_fear_df_with_gender.res <- kmeans(responses_fear_df_with_gender, 2, nstart = 25)
fviz_cluster(responses_fear_df_with_gender.res, data = responses_fear_df_with_gender,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# Finally, we can see a huge effect on gender for the fears.
# Ironically, this maybe is the most true as more females are more afraid of certain
# things as compared to a man.

# For the most defining cluster, I will have to say that the cluster that provided the most information is definitely the fear
# where there's clear distinctions.

# Based on these 4 categories that I filtered out from the data.
# I can say that Gender does not cause a huge impact on each cluster tested except for fear
# I also will say that it is not a major factor in regards to clusters.

# This is also held true via the code based by where we can see the difference between female and male for each sub-sub categories: 
# https://www.kaggle.com/miroslavsabo/analyzing-gender-differences 



