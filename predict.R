library(caret)
library(caTools)

# READ DATA
data <- read.csv('data/diabetes.csv', stringsAsFactors = F)
data$Outcome <- factor(data$Outcome, labels = c("0" = "No", "1" = "Yes"))

# TRAIN MODELS
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10, search = 'grid')

# TRAIN/TEST SPLIT
acc <- c()
times <- 1 : 200
for(i in times) {
  sample.n <- sample.split(Y = data$Pregnancies, SplitRatio = 0.9)
  train <- subset(data, sample.n == TRUE)
  test <- subset(data, sample.n == FALSE)
  true <- test$Outcome
  test <- subset(test, select = -c(length(test)))
  
  print(i)
  model <- caret::train(Outcome ~ Glucose + BMI + Age + Insulin + Pregnancies + BloodPressure + SkinThickness, train, 'pam', trControl = control, metric = "Accuracy")
  model.d <- predict(model, test)
  model.acc <- accuracy(model.d, true)
  acc[i] <- model.acc
}

# AGGREGATE RESULTS
acc.df <- data.frame(as.integer(times), acc)
p <- ggplot(acc.df, aes(x = times, y = acc)) + geom_point(size = 1) + theme_hc() + xlab("Attempts") + ylab("Model Accuracy") + ggtitle("Repeated Sampling Accuracy of Model (200 trials") + geom_hline(yintercept = mean(acc.df$acc), size = 2, colour = "#25CCF7") + geom_text(colour = "#25CCF7", aes(1.5, mean(acc.df$acc), label = "Mean", vjust = -1))

p1 <- ggplot(acc.df, aes(x = acc)) + geom_histogram(binwidth = .002) + theme_hc()
p2 <- p1 + stat_function(fun = dnorm, size = 2, color = "#25CCF7", args = list(mean = mean(acc.df$acc), sd = sd(acc.df$acc))) + xlab("Classification Accuracy") + ylab("Frequency") + ggtitle("Repeated Sampling Model Accuracies vs. Normal Curve (200 trials)")
p
p2
