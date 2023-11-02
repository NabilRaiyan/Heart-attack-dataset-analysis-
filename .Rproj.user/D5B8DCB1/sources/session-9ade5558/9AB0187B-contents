
heart_attack_ds <- read.csv("E:/Courses/AIUB Courses/9th Semester/Introduction to data science/Midterm/Midterm Project/heart_attack_ds.csv")
heart_attack_ds

head(heart_attack_ds)
tail(heart_attack_ds)
summary(heart_attack_ds)
str(heart_attack_ds)
dim(heart_attack_ds)

colSums(is.na(heart_attack_ds))


mean_age <- mean(heart_attack_ds$Age, na.rm = TRUE)
heart_attack_ds$Age[is.na(heart_attack_ds$Age)] <- mean_age


median_age <- median(heart_attack_ds$Age, na.rm = TRUE)
heart_attack_ds$Age[is.na(heart_attack_ds$Age)] <- median_age

