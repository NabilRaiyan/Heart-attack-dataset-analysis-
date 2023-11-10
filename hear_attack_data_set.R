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













