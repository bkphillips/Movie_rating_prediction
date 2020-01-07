# Author: Bryan Phillips
# Code for training and validating prediction models for movielens datasets

#Loading all required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(tidyr)
library(ggplot2)
#Inputing Data
#When I tried running the code given to get the test and validation datasets, it did not work so I had to use the dropbox files given in the forum
#Below is the link to the dropbox files
# I was not able to load them into github because they are too large to go to rerence them locally

#set working directory
directory <-"C:/Users/bryanphillips/harv_x/movie_data"
#edx training data
edx_data<-file.path(directory,'edx.rds')
edx <- readRDS(edx_data)
#validation data
validation_data<-file.path(directory,'validation.rds')
validation <- readRDS(validation_data)

#Using code that was given in the movielens example in the machine learning course
#everything until the genre related code was taken from the course
#Getting the average rating of all movies
mu_hat <- mean(edx$rating)
mu_hat

#getting naive rmse
naive_rmse <- RMSE(edx$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(edx))
RMSE(edx$rating, predictions)

#creating rmse results matrix
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

#Creating movie average rating model "b_i"
# fit <- lm(rating ~ as.factor(userId), data = movielens)
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#looking at distribution of movie rating bias
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

#gathering b_i variable for analysis
predicted_ratings <- mu + edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

#checking rmse of first b_i model and adding to results table
model_1_rmse <- RMSE(predicted_ratings, edx$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

rmse_results %>% knitr::kable()

#Looking at distribution of average user ratings
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

#creating average user ratings data to create user bias "b_u"
# lm(rating ~ as.factor(movieId) + as.factor(userId))
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

#checking rmse of "b_i+b_u" model and adding to results table
model_2_rmse <- RMSE(predicted_ratings, edx$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()


#There are 797 distinct genres in the non-tidy format
n_distinct(edx$genres)

#looking at rating in non-tidy format
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 10000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_blank())

#Creating tidy version of data to work with genres
edxl <- edx %>% 
  separate_rows(genres,sep="\\|")

#Now only 20 genres when converting to tidy format
n_distinct(edxl$genres)

#looking at the avg rating by genre after tidy converstion
edxl %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 100) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#creating the genre average for genre bias "b_g" using tidy genre data
genre_avgs <- edxl %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

#Creating model for b_i + b_u + b_g
predicted_ratings <- edxl %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  group_by(userId,movieId,rating) %>% 
  summarize(pred = mean(pred)) 

model_3_rmse <- RMSE(predicted_ratings$pred, predicted_ratings$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects + Genre Effects Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

#Create User Genre Bias data for "b_ug"
ugenre_avgs <- edxl %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(userId, genres) %>%
  summarize(b_ug = mean(rating - mu - b_i - b_u))

# create user genre effects model b_i + b_u + b_ug
predicted_ratings <- edxl %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(ugenre_avgs, by=c('genres'='genres','userId'='userId')) %>%
  rowwise()  %>%
  mutate(pred = sum( mu,b_i, b_u, b_ug, na.rm=TRUE)) 
predicted_ratings<- predicted_ratings %>%
  group_by(userId,movieId,rating) %>% 
  summarize(pred = mean(pred)) 

model_4_rmse <- RMSE(predicted_ratings$pred, predicted_ratings$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects + User Genre Effects Model",  
                                     RMSE = model_4_rmse ))
rmse_results %>% knitr::kable()

# MODEL VALIDATION
# using validation dataset to test last three models

#adding b_i + b_u prediction model
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

#checking rmse of "b_i+b_u" model and adding to results table
val_1_rmse <- RMSE(predicted_ratings, validation$rating)
val_rmse_results <- data_frame(method = "Movie + User Effects Model", RMSE = val_1_rmse)

val_rmse_results %>% knitr::kable()

#For genre related models, creating tidy version of validation data
val_xl <-validation %>% 
  separate_rows(genres,sep="\\|")

# Adding genre effects model for validation
predicted_ratings <- val_xl %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  rowwise()  %>%
  mutate(pred = sum( mu,b_i, b_u, b_g, na.rm=TRUE)) 
predicted_ratings<- predicted_ratings %>%
  group_by(userId,movieId,rating) %>% 
  summarize(pred = mean(pred)) 

#Validate rmse of b_i + b_u + b_g model
val_2_rmse <- RMSE(predicted_ratings$pred, predicted_ratings$rating)

val_rmse_results <- bind_rows(val_rmse_results,
          data_frame(method="Movie + User Effects + Genre Effects Model",  
                     RMSE = val_2_rmse ))
val_rmse_results %>% knitr::kable()


# Validate User Genre Model FINAL MODEL VALIDATION
# Add user genre model prediction to validation data
predicted_ratings <- val_xl %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(ugenre_avgs, by=c('genres'='genres','userId'='userId')) %>%
  rowwise()  %>%
  mutate(pred = sum( mu,b_i, b_u, b_ug, na.rm=TRUE)) 
predicted_ratings<- predicted_ratings %>%
  group_by(userId,movieId,rating) %>% 
  summarize(pred = mean(pred)) 

#Validate rmse of b_i + b_u + b_g model
val_3_rmse <- RMSE(predicted_ratings$pred, predicted_ratings$rating)

val_rmse_results <- bind_rows(val_rmse_results,
                              data_frame(method="Movie + User Effects + User Genre Effects Model",  
                                         RMSE = val_3_rmse ))
val_rmse_results %>% knitr::kable()


