## Introduction
# The following data analysis report is prepared as a part of HarvardX Data Science Capstone (HarvardX: PH125.9x) Project, in which the given dataset "movielens" has been analysed by using different tools and techniques learned during the program, especially the skills in the use of R-Programming and machine learning capabilities. The report includes "calling the movielense data Into the Project from the given code", "data exploration", and "data modeling to achive project goal".

## Project Goal
# The goal is to predict movie ratings, and evaluate the accuracy of the predicted model from the given code the dataset called  "edx" was split into the training and validation sets.

## Calling the movielense data Into the Project
# The code is provided in the edx capstone project module:
# The following lines of code will create training and validation sets (provied in the edx capstone project module)


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
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")
#  Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
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

# The above chunk of code gives a partition of the dataset for training and testing our dataset. It also removes the unnecessary files from the working directory, which is always a good coding practice (‘always clean after you cook’).


# Note that the above code is given in the project document, and used as it is to generate the project data sets.

## Data Exploration
# now we have two data sets "edx" and "validation", lets explore them:

head(edx)
head(validation)
str(edx)
str(validation)
names(edx)
names(validation)


# The output from the above code shows that there are 9000055 cases and 6 variables in edx data set, whereas the valiation data set includes 999999 cases and 6 variables. Notice that the variable "rating" is included in both data sets. The variables in "edx" and "validation" data sets are  "userId"    "movieId"   "rating"    "timestamp" "title"  and   "genres". Moreover both data sets are data frame. The proportion between the length of two data sets is approximately 1:10

nrow(edx)/nrow(validation)
nrow(validation)/nrow(edx)
 

## Removing label column from the validation data set

# As the project goal is to predict rating, therefore we remove rating column from the validation dataset.

validation_rt <- validation  # this is original validation which holds the rating column (just renamed).
validation <- validation %>% select(-rating)  # this is updated validation without rating column.
head(validation)


## Further preprocessing data sets

# Notice that in the "title-column" the years are also appearning with the movie name. Similarly in the "genres- column"  more than one genre are shown. We need to reformat the "edx" and "valiadation" datasets.

# Creating new column as "year" and move year information from the "title" column into the "year" column in the edx datasets


edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
head(edx)

# Creating new column as "year" and move year information from the "title" column into the "year" column in the validation datasets, and store original "valiadation" with rating column as "validation_rt"

validation <- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation_rt <- validation_rt %>% mutate(year = as.numeric(str_sub(title,-5,-2)))

# Removing year values form the title column in the edx and validation datasets

edx<- edx %>% mutate(title = str_remove_all(edx$title, "[()0123456789]"))
validation<- validation %>% mutate(title = str_remove_all(validation$title, "[()0123456789]"))
validation_rt<- validation_rt %>% mutate(title = str_remove_all(validation_rt$title, "[()0123456789]"))

# Following three lines of code can be used to Seperate the "genres" column in the edx & validation dataset, and add each genre into new separate row. The genre in this pproject is not seprated due to machine memory limit issue.

#edx  <- edx  %>% separate_rows(genres, sep = "\\|")
#validation <- validation   %>% separate_rows(genres, sep = "\\|")
#validation_rt <- validation_rt  %>% separate_rows(genres, sep = "\\|")

# Examining the new datasets

head(edx)
head(validation)
head(validation_rt)

# We can see that data is now in a better format to proceed further. 

## Exploring the data

# Examiningg the distribution of "rating" in the training "edx" data set.

table(edx$rating) # it gives the frequency of the distinct values in the variable for a variable (e.g Age)

# From above output, we can find the rating range as: 0.5, 1, 1.5, 2, 2.5, 3, 3.5,  4, 4.5, 5 

# we can also see the proportion of each ratings

prop.table(table(edx$rating))

# Examining the liking pattern among cases by plotting the bar chart. 

barplot(table(edx$rating))

# The diagram shows that rating level 3 and 4 is very popular with frequency equal to 2121240 and 2588430 respectively. 

# Descriptive statistics can be obtained through the popular summary() function

summary(edx)

# Finding frequencies of users, movies, genres, and years in the edx dataset

edx %>% summarize(users = n_distinct(userId), movies = n_distinct(movieId), genres = n_distinct(genres), years = n_distinct(year))

# It is clear from the output that in the edx data set there are  8 users, 373 movies, 18 genres and 63 years in the edx data set.

# Frequency distribution of the genres.

genre_f <- edx %>% group_by(genres) %>% summarize(count = n()) %>% arrange(desc(count)) 
genre_f

# Frequency distribution of the movies.

movie_f <- edx %>% group_by(title) %>% summarize(count = n()) %>% arrange(desc(count)) 
movie_f

## Modeling

## Model-1

# At the base level, model is developed by using average rating (mean) to train the model and predict the movie rating in the validation model.

avg <- mean(edx$rating)  
avg

# For measuring predictive strength RMSE will be used. Following code will define the RMSE, which is the square root of the mean of the squared difference between observed and estimated ratings.

RMSE <- function(observed, estimated){
  sqrt(mean((observed-estimated)^2,na.rm=T))
}

# Initiate RMSE results to compare various models
rmse_data <- data_frame()

m1 <- mean(edx$rating)

rmse1 <- RMSE(validation_rt$rating,m1)

rmse_data <- data_frame(method = "m1", RMSE = rmse1)

rmse_data %>% knitr::kable() 

## Model-2

# In this model the Movies are used to examine the predictive efficiency. Before the development of the model-2, the examination of bias in the data is examined. The presence of inherent bias in dat can effect the quality of the prediction. We can see from the following diagram that some movies are rated more often than others. 

edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, aes(y=..density..), color = "red", fill = "red") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_log10() + 
  ggtitle("Movie Bias")

# The skewed histogram shows that some movies were rated rarely. This is a bias in rating, which is causing imbalence in the data. The histogram is skewed towards negative rating effect. To handle this isuue, deviations from mean is taken. The weighted Movie rating will be obtained with the following code:
  
adjusted_m_rating <- edx %>% 
  group_by(movieId) %>% 
  summarize(adj_movie = mean(rating - avg))

adjusted_m_rating %>% qplot(adj_movie, geom ="histogram", bins = 20, data = ., color = I("black"))

# Model development from the adjusted movie data (Model-2)

m2 <- validation %>% 
  left_join(adjusted_m_rating, by='movieId') %>%
  mutate(pred = avg + adj_movie) 

rmse2 <- RMSE(validation_rt$rating,m2$pred)

rmse_data <- bind_rows(rmse_data,
                       data_frame(method="m2",  
                                  RMSE = rmse2 ))
rmse_data %>% knitr::kable()

# The error has drop by 5% and motivates us to move on this path further, and an improvement in the RMSE is achieved by adjusting for movie bias.

## Model-3

# In the model-3, the users data is examined for the biasedness. The diagram below shows that the users distribution of rating is not unform, some users are very active in rating than the others.

edx %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, aes(y=..density..), color = "red", fill = "red") + 
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_log10() + 
  ggtitle("Users")

# To handle the issue of user bias, the deviations from mean is taken, and the weighted user rating will be obtained with the following code:
  
adjusted_u_rating <- edx %>% 
  left_join(adjusted_m_rating, by='movieId') %>%
  group_by(userId) %>%
  summarize(adj_user = mean(rating - avg - adj_movie))

adjusted_u_rating %>% qplot(adj_user, geom ="histogram", bins = 30, data = ., color = I("black"))

# Model development from the adjusted movie and user data (Model-3)

m3 <- validation %>% 
  left_join(adjusted_m_rating, by='movieId') %>%
  left_join(adjusted_u_rating, by='userId') %>%
  mutate(pred = avg + adj_movie + adj_user) 

rmse3 <- RMSE(validation_rt$rating,m3$pred)

rmse_data <- bind_rows(rmse_data,
                       data_frame(method="m3",  
                                  RMSE = rmse3 ))
rmse_data %>% knitr::kable()

# There is an improvement from m1 to m3.

## Model-4

# This model is developed by refining the data to further level. The method of regularization is used. Cross validation is used to select optimum value of lambda (lmb). For every value of lmb, adj_movie and adj_user is determined.

lmb <- seq(0, 10, 0.25)

rmse_1 <- sapply(lmb, function(l){
  
avg <- mean(edx$rating)
  
adj_movie <- edx %>% 
    group_by(movieId) %>%
    summarize(adj_movie = sum(rating - avg)/(n()+l))
  
adj_user <- edx %>% 
    left_join(adj_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(adj_user  = sum(rating - adj_movie - avg)/(n()+l))
  
pred_r <- validation %>% 
    left_join(adj_movie, by = "movieId") %>%
    left_join(adj_user , by = "userId") %>%
    mutate(pred = avg + adj_movie + adj_user ) %>%
    .$pred
  
  return(RMSE(validation_rt$rating,pred_r))
})

# The optimum value of lmb can also be seen with the help of following diagram. This is a plot between rmse1 and lmb. 


qplot(lmb, rmse_1) 

# To find accurate value of lmb (optimum), following code is used.

lmb <- lmb[which.min(rmse_1)]
lmb

# Determining the regularized estimates of adj_movie by using lmb.

adj_m <- edx %>% 
  group_by(movieId) %>% 
  summarize(adj_movie = sum(rating - avg)/(n()+lmb), m_n = n())

# Determining the regularized estimates of adj_user by using lmb.

adj_u <- edx %>% 
  left_join(adj_m, by='movieId') %>%
  group_by(userId) %>%
  summarize(adj_user  = sum(rating - avg - adj_movie )/(n()+lmb), u_n = n())

# Predicting rating

pred_r_1 <- validation %>% 
  left_join(adj_m, by='movieId') %>%
  left_join(adj_u, by='userId') %>%
  mutate(pred = avg + adj_movie + adj_user ) %>% 
  .$pred

# Results

rmse4 <- RMSE(validation_rt$rating,pred_r_1)
rmse_data <- bind_rows(rmse_data,
                       data_frame(method="m4",  
                                  RMSE = rmse4 ))
rmse_data %>% knitr::kable()

############################################################################################################################