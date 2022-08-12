# 1. import library
library(tidyverse)
library(skimr)
library(dplyr)
library(ggpubr)
library(readr)
library(magrittr)
library(ggplot2)
library(readxl)
library(tidyr)
library(leaps)

# 2. import Airbnb NYC dataset
listings <- read_excel('listings.xlsx')

# 3.  understand the dataset from summary
dim(listings)

head(listings)

colnames(listings)

skim_without_charts(listings)

summary(listings)

## Data Cleaning
sapply(listings, function(x) sum(is.na(x)))
listings$reviews_per_month[is.na(listings$reviews_per_month)] <- 0


# 4. understand the character variables
neighbourhood_group <- c(unique(listings['neighbourhood_group']))
neighbourhood <- unique(listings['neighbourhood'])
room_type <- unique(listings['room_type'])

# 5. frequency distribution of quantitative variables

# neighbor_group
freq_neighbour_group <- as.data.frame(cbind(frequency = table(listings$neighbourhood_group),
                        percentage = prop.table(table(listings$neighbourhood_group))))

freq_neighbour_group <- freq_neighbour_group[order(freq_neighbour_group$frequency, decreasing = TRUE),]
freq_neighbour_group
barplot(freq_neighbour_group$frequency,main = 'neighbor group distribution', 
        names.arg = neighbourhood_group, ylab = 'Frequency')

# neighbor
freq_neighbour <- as.data.frame(cbind(frequency = table(listings$neighbourhood),
                                      percentage = prop.table(table(listings$neighbourhood))))

freq_neighbour <- freq_neighbour[order(freq_neighbour$frequency, decreasing = 1),]
freq_neighbour

# room_type
freq_room_type <- as.data.frame(cbind(frequency = table(listings$room_type),
                                      percentage = prop.table(table(listings$room_type))))

freq_room_type
barplot(freq_room_type$frequency,main = 'room type distribution', 
        names.arg = c('Entire home/apt','Hotel room','Private room','Shared room '), ylab = 'Frequency')

# box plot of price except outliers
boxplot(listings$price)

# remove the price outliers
listings <- listings %>% filter(price < 350 & price>0)

# Q1: Whether different neighbor show different price
# Entire home/apt Price box plot 
rt_entire <- listings %>% 
  filter(room_type == 'Entire home/apt') %>%
  group_by(neighbourhood_group)

ggplot(rt_entire, aes(x=neighbourhood_group, y=price)) +
  geom_boxplot(outlier.size = NULL) +
  ggtitle('entire apt price in neighbor groups')


# Prive Room Price box plot
rt_private <- listings %>% 
  filter(room_type == 'Private room') %>%
  group_by(neighbourhood_group)

ggplot(rt_private, aes(x=neighbourhood_group, y=price)) +
  geom_boxplot(outlier.size = NULL) +
  ggtitle('private room price in neighbor groups')

# hypothesis testing for price and neighbor_group
test <- lm(listings$price ~ listings$neighbourhood_group)
summary(test)


# Regression model--predict price 
# First, to make the model more accurate, I will drop Hotel room and Share room since their proportion are small. 
listings2 <- listings %>% 
  filter(room_type %in% c('Entire home/apt','Private room'))

# split the data 
set.seed(666)
n = nrow(listings2)
train_ind <- sample(1:n, size=round(0.7*n), replace = FALSE)
train_data <- listings[train_ind, ]
test_data <- listings2 [-train_ind,]

# regression model
model1 <- lm(price ~ availability_365 + neighbourhood_group + minimum_nights +
               number_of_reviews  + latitude + longitude, data = listings)
summary(model1)

plot(model1$finalModel)

model2 <- lm(log(price) ~ availability_365 + neighbourhood_group + minimum_nights +
               number_of_reviews  + latitude + longitude + room_type, data = train_data)
summary(model2)
plot(model2)

pred <- predict(model2, newdata = test_data)
rmse <- sqrt(mean(test_data$price - pred)**2)
sse <- sum((test_data$price - pred)**2)
ssr <- sum((pred - mean(test_data$price))**2)
r2 <- 1 - sse/(sse+ssr)
