library(caret)
library(caTools)
library(ggplot2)
library(ggthemes)
library(Metrics)

set.seed(100)

# READ DATA
data <- read.csv('data/diabetes.csv', stringsAsFactors = F)
data$Outcome <- factor(data$Outcome, labels = c("0" = "No", "1" = "Yes"))

# TRAIN/TEST SPLIT
sample.n <- sample.split(Y = data$Pregnancies, SplitRatio = 0.9)
train <- subset(data, sample.n == TRUE)
test <- subset(data, sample.n == FALSE)
true <- test$Outcome
test <- subset(test, select = -c(length(test)))

# TRAIN MODELS
control <- trainControl(method = "repeatedcv", repeats = 10, number = 10, search = 'grid')
train.time <- c()

pm <- caret::train(Outcome ~ Glucose + BMI + Age + Insulin + Pregnancies + BloodPressure + SkinThickness + DiabetesPedigreeFunction, train, 'pam', trControl = control)
pmd <- predict(pm, test)
pma <- accuracy(pmd, true)

cm <- as.data.frame(table(model = pmd, diabetes = true))
colnames(cm) <- c("model", "diabetes", "Frequency")

p <- ggplot(data = cm, mapping = aes(x = model, y = diabetes)) +
  geom_tile(aes(fill = Frequency), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Frequency)), vjust = 1, color = "white", size = 10) +
  scale_fill_gradient(low = "#2C3A47", high = "#25CCF7") +
  theme_bw() + xlab("Model Prediction") + ylab("True Outcome") + ggtitle("Confusion Matrix for Nearest Shrunken Centroids Algorithm")
