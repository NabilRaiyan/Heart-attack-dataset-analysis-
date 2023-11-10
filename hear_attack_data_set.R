options(max.print = .Machine$integer.max)

heart_attack_ds <- read.csv("E:/Courses/AIUB Courses/9th Semester/Introduction to data science/Midterm/Midterm Project/heart_attack_ds.csv")
heart_attack_ds
head(heart_attack_ds)
tail(heart_attack_ds)
summary(heart_attack_ds)
str(heart_attack_ds)

dim(heart_attack_ds)
names(heart_attack_ds)
is.na(heart_attack_ds)



boxplot(heart_attack_ds$Age, main = "Age Box Plot", ylab = "Age")
age_outliers <- boxplot(heart_attack_ds$Age)$out
cat("Potential Outliers in Age:", outliers, "\n")

median_age <- median(heart_attack_ds$Age, na.rm = TRUE)
heart_attack_ds$Age[is.na(heart_attack_ds$Age)] <- median_age




mode_age <- as.numeric(names(sort(table(heart_attack_ds$Age), decreasing = TRUE)[1]))
cat("Mode of Age:", mode_age, "\n")
heart_attack_ds$Age[is.na(heart_attack_ds$Age)] <- mode_age


mean_age <- mean(heart_attack_ds$Age, na.rm = TRUE)
heart_attack_ds$Age[is.na(heart_attack_ds$Age)] <- mean_age

na.omit(heart_attack_ds)

age_outliers <- boxplot(heart_attack_ds$Age)$out
cat("Potential Outliers in Age:", outliers, "\n")
outliers_rows <- heart_attack_ds$Age > 120
heart_attack_ds <- heart_attack_ds[!outliers_rows, ]
heart_attack_ds




heart_attack_ds$Sex <- ifelse(heart_attack_ds$Sex == "M", 1, ifelse(heart_attack_ds$Sex == "F", 0, NA))
sex_column_missing_value_count <- sum(is.na(heart_attack_ds$Sex))
cat("Total missing values in the 'Sex' column:", sex_column_missing_value_count, "\n")

missing_rows <- which(is.na(heart_attack_ds$Sex))
cat("Row numbers with missing values in the 'Sex' column:", paste(missing_rows, collapse = ", "), "\n")

median_sex <- median(heart_attack_ds$Sex, na.rm = TRUE)
heart_attack_ds$Sex[is.na(heart_attack_ds$Sex)] <- median_sex

mode_sex <- as.numeric(names(sort(table(heart_attack_ds$Sex), decreasing = TRUE)[1]))
cat("Mode of Sex:", mode_sex, "\n")
heart_attack_ds$Sex[is.na(heart_attack_ds$Sex)] <- mode_sex

heart_attack_ds <- na.omit(heart_attack_ds)
dim(heart_attack_ds)
heart_attack_ds$Sex <- ifelse(heart_attack_ds$Sex == 1, "M", ifelse(heart_attack_ds$Sex == 0, "F", NA))



boxplot(heart_attack_ds$RestingBP, main="Resting BP Plot", ylab="RestingBP")
outliers_rows_RestingBp <- boxplot(heart_attack_ds$RestingBP)$out
cat("Potentials outliers in Resting BP column: ", outliers_rows_RestingBp, "\n")


restingBP_outliers_rows_num <- which(heart_attack_ds$RestingBP > 170 | heart_attack_ds$RestingBP < 1)
cat("Outliers rows numbers are: ", restingBP_outliers_rows_num, "\n")
heart_attack_ds <- heart_attack_ds[-c(5, 110, 124), ]


median_restingBP <- median(heart_attack_ds$RestingBP)
mean_restingBP <- round(mean(heart_attack_ds$RestingBP), digits = 0)
mode_restingBP = as.numeric(names(sort(table(heart_attack_ds$RestingBP), decreasing = TRUE)[1]))
for (i in 1:125){
  if (heart_attack_ds$RestingBP[i] > 170 | heart_attack_ds$RestingBP[i] < 1){
    heart_attack_ds$RestingBP[i] = mode_restingBP
  }
}




boxplot(heart_attack_ds$Cholesterol, main="Cholesterol Plot", ylab="Cholesterol")
outliers_rows_cholesterol <- boxplot(heart_attack_ds$Cholesterol)$out
cat("Outliers in Cholesterol Column: ", outliers_rows_cholesterol, "\n")
cholesterol_outliers_row_num <- which(heart_attack_ds$Cholesterol > 380 | heart_attack_ds$Cholesterol < 100)
cat("Rows of potential outliers in Cholesterol Column: ", cholesterol_outliers_row_num, "\n")

heart_attack_ds <- heart_attack_ds[-c(11, 12, 29, 31, 70, 77, 99, 103, 104, 124, 133, 150), ]
dim(heart_attack_ds)

median_cholesterol <- median(heart_attack_ds$Cholesterol)

mode_cholesterol <- as.numeric(names(sort(table(heart_attack_ds$Cholesterol), decreasing = TRUE)[1]))
mode_cholesterol
for(i in 1:150){
  if (heart_attack_ds$Cholesterol[i] > 360 | heart_attack_ds$Cholesterol[i] <= 100){
    heart_attack_ds$Cholesterol[i] = mode_cholesterol
  }
}



heart_attack_ds$ExerciseAngina <- ifelse(heart_attack_ds$ExerciseAngina == 'Y', 1, ifelse(heart_attack_ds$ExerciseAngina == 'N', 0, NA))

colSums(is.na(heart_attack_ds))
missing_value_rows_num_exercise_angina <- which(is.na(heart_attack_ds$ExerciseAngina))
cat("Missing value rows numbers: ", missing_value_rows_num_exercise_angina, "\n")

  
heart_attack_ds <- heart_attack_ds[-c(7, 27), ]
  

  
