library(tidyverse)
library(dplyr)

#source data.. Titanic?
dat <- read.csv("Titanictrain.csv")
class(dat)
str(dat)
summary(dat)

#Visualize NA data and compare to Training Data Set.

NA_dat <- dat %>%  filter(is.na(dat$Age) == TRUE)
NA_dat %>% ggplot(aes(x = Pclass, fill = Sex)) +
  geom_bar() +
  facet_grid(.~ Survived) +
  labs(title = "NA Data")

dat %>% ggplot(aes(x = Pclass, fill = Sex)) +
  geom_bar() +
  facet_grid(.~ Survived) +
  labs(title = "Training Data")

#Comparison shows NA data is proportionally relevant to 

dat %>% ggplot() +
  geom_histogram(aes(Age),na.rm = TRUE,binwidth = 2) +
  labs(title = "Age Distribution")

#replicate and sample training data to create Mean age and SD.
#use these figures to randomly create 177 ages for missing data.

age_fix <- na.omit(dat$Age)

N <- 1000

sample_distribution <- replicate(N, {
  s <- sample(age_fix, size = 891, replace = TRUE)
  m <- mean(s)
})

X_hat <- mean(sample_distribution)
standard_dev <- sd(sample_distribution)
age_fix_input <- rnorm(179, mean = X_hat, sd = standard_dev)

head(sample_distribution)
hist(sample_distribution)

#allocate 10% of training data for cross validation of training model.
dim(dat)
#10% of 891 = 89
dat_cross_val <- dat[sample(nrow(dat),89),]

