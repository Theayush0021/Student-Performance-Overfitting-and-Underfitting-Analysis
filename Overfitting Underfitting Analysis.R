
library(rpart)
library(randomForest)

students <- read.csv("StudentsPerformance.csv")

students$average_score <- (
  students$math.score +
    students$reading.score +
    students$writing.score
) / 3

set.seed(123)

index <- sample(1:nrow(students), 0.8 * nrow(students))

train_data <- students[index, ]
test_data <- students[-index, ]

# Underfitting Model
model1 <- rpart(
  average_score ~ gender + race.ethnicity +
    parental.level.of.education + lunch +
    test.preparation.course,
  data = train_data,
  control = rpart.control(maxdepth = 1, cp = 0.01)
)

# Medium Decision Tree
model2 <- rpart(
  average_score ~ gender + race.ethnicity +
    parental.level.of.education + lunch +
    test.preparation.course,
  data = train_data,
  control = rpart.control(maxdepth = 3, cp = 0.001)
)

# Complex Decision Tree
model3 <- rpart(
  average_score ~ gender + race.ethnicity +
    parental.level.of.education + lunch +
    test.preparation.course,
  data = train_data,
  control = rpart.control(maxdepth = 15, cp = 0)
)

# Random Forest
model4 <- randomForest(
  average_score ~ gender + race.ethnicity +
    parental.level.of.education + lunch +
    test.preparation.course,
  data = train_data,
  ntree = 100
)

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

results <- data.frame(
  Model = c(
    "Simple Tree",
    "Medium Tree",
    "Complex Tree",
    "Random Forest"
  ),
  Train_RMSE = c(
    rmse(train_data$average_score, predict(model1, train_data)),
    rmse(train_data$average_score, predict(model2, train_data)),
    rmse(train_data$average_score, predict(model3, train_data)),
    rmse(train_data$average_score, predict(model4, train_data))
  ),
  Test_RMSE = c(
    rmse(test_data$average_score, predict(model1, test_data)),
    rmse(test_data$average_score, predict(model2, test_data)),
    rmse(test_data$average_score, predict(model3, test_data)),
    rmse(test_data$average_score, predict(model4, test_data))
  )
)

results$Difference <- results$Test_RMSE - results$Train_RMSE

print(results)

# Best model based on test RMSE
best_model <- results$Model[which.min(results$Test_RMSE)]

print(paste("Best Model:", best_model))

png("Model_Comparison_Test_RMSE.png",
    width = 1000,
    height = 700)

barplot(results$Test_RMSE,
        names.arg = results$Model,
        main = "Model Comparison - Test RMSE",
        xlab = "Models",
        ylab = "Test RMSE",
        col = "skyblue",
        las = 2)

dev.off()
