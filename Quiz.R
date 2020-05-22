################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")



movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))



movielens <- left_join(ratings, movies, by = "movieId")


# Validation set will be 10% of MovieLens data
set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#*********************************
# Quiz to validate the edx dataset
#
#Q1: How many rows and columns are there in the edx dataset? 

dim(edx)
# Number of rows: 9000055 ; Number of colums: 6

#Q2: 
#How many zeros were given as ratings in the edx dataset?

edx %>% filter(rating == 0.0) %>% tally()
# ans: 0

#How many threes were given as ratings in the edx dataset?
edx %>% filter(rating == 3.0) %>% tally()
#ans: 2121240

#Q3: How many different movies are in the edx dataset?
n_distinct(edx$movieId)

#ans: 10677

#Q4: How many different users are in the edx dataset?
n_distinct(edx$userId)

#ans: 69878

#Q5: How many movie ratings are in each of the following genres in the edx dataset?
n_distinct(edx$genres)
 
genreCounts <- sapply(c('Drama', 'Comedy', 'Thriller', 'Romance'), function(g){
  edx %>% filter(str_detect(genres, g)) %>% tally()
})
genreCounts

#Drama: 3910127; Comedy: 3540930; Thriller: 2325899; Romance: 1712100

#Q6: Which movie has the greatest number of ratings?
edx %>% group_by(movieId, title) %>% summarize(count = n()) %>% top_n(5) %>% arrange(desc(count)) 

# ans: Pulp Fiction

#Q7: What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>% arrange(desc(count))


#ans: 4, 3, 5, 3.5, 2
#Q8: True or False: In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.)
half_rating <- seq(0.5, 4.5, 1)
edx %>% mutate(half_ratings = ifelse(rating %in% half_rating, 1, 0)) %>%
  group_by(half_ratings) %>%
  summarize(count = n()) %>% select(half_ratings, count)

#ans: True

#-save edx, and validation set locally, to be used in the project

save(edx, file = "rda/edx.rda") 
save(validation, file = "rda/validation.rda") 
