# 'studentInfo.csv' should be in the current working directory, make sure.

# Load student dataset using base R read.csv function
student_data <- read.csv(r"(C:\Users\karth\Downloads\studentInfo.csv)")

# Preprocessing the data: convert "disability" into a factor and "final_result" into a binary variable
student_data$is_passed <- as.factor(ifelse(student_data$final_result == "Pass", 1, 0))
student_data$disability_status <- as.factor(student_data$disability)

# Make 'imd_band' into a numerical scale according to the provided categories.
imd_scale <- c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
student_data$imd_numeric <- as.numeric(factor(student_data$imd_band, levels = imd_scale))

# Creat training and test sets
set.seed(20230712)  # Setting seed for reproducibility
sample_count <- floor(0.8 * nrow(student_data))
training <- sample(seq_len(nrow(student_data)), size = sample_count)

train_data <- student_data[training]
test_data <- student_data[-training]

# Building a logistic regression model with glm (Generalized Linear Model) in base R
logit_model <- glm(is_passed ~ disability_status + imd_numeric, family = binomial(link = "logit"), data = train_data)

# Display Model Summary
summary(logit_model)

# Predicting on test data
test_predictions <- predict(logit_model, test_data, type = "response")
predicted_outcome <- ifelse(test_predictions > 0.5, 1, 0)

# Calculating the Accuracy
true_outcomes <- as.numeric(test_data$is_passed) - 1  # Adjusting factor levels from 1 to 0 and 1 for comparison
model_accuracy <- mean(predicted_outcome == true_outcomes)
print(paste("Model Accuracy:", model_accuracy))

