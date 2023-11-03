

options(max.print = .Machine$integer.max)

heart_attack_ds <- read.csv("E:/Courses/AIUB Courses/9th Semester/Introduction to data science/Midterm/Midterm Project/heart_attack_ds.csv")
heart_attack_ds

head(heart_attack_ds)
tail(heart_attack_ds)
summary(heart_attack_ds)
str(heart_attack_ds)
dim(heart_attack_ds)

# Finding missing values
colSums(is.na(heart_attack_ds))

#Recovering missing values using mean
mean_age <- mean(heart_attack_ds$Age, na.rm = TRUE)
heart_attack_ds$Age[is.na(heart_attack_ds$Age)] <- mean_age

#Recovering missing values using median
median_age <- median(heart_attack_ds$Age, na.rm = TRUE)
heart_attack_ds$Age[is.na(heart_attack_ds$Age)] <- median_age


#---------------------------
# Install and load the 'dplyr' package if you haven't already
# install.packages("dplyr")
library(dplyr)

# Data value transformation
# Assuming 'heart_data' is your dataset
heart_attack_ds <- heart_attack_ds %>%
  mutate(Sex = recode(Sex, "M" = 1, "F" = 0))
heart_attack_ds


----------------------
  
# Calculate the mode of the "Sex" variable
mode_sex <- as.character(which.max(table(heart_attack_ds$Sex)))

# Impute missing "Sex" values with the mode
heart_attack_ds$Sex[is.na(heart_attack_ds$Sex)] <- mode_sex
heart_attack_ds

