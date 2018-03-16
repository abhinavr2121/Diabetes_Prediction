library(ggplot2)
library(ggthemes)
library(ggExtra)
library(corrplot)
library(gridExtra)

data <- read.csv('data/diabetes.csv')
data$Outcome <- factor(data$Outcome, labels = c("0" = "No Diabetes", "1" = "Diabetes"))

p1 <- ggplot(data, aes(x = data$BMI, fill = Outcome)) + geom_histogram(binwidth = 1) + theme_hc() + xlim(c(1, 80)) + xlab("Body Mass Index") + ylab("Frequency") + ggtitle("BMI")
p2 <- ggplot(data, aes(x = data$BloodPressure, fill = Outcome)) + geom_histogram(binwidth = 2) + theme_hc() + xlab("Blood Pressure") + ylab("Frequency") + ggtitle("Blood Pressure Measurements") + xlim(c(1, 125))
p3 <- ggplot(data, aes(x = data$DiabetesPedigreeFunction, fill = Outcome)) + geom_histogram(binwidth = .05) + theme_hc() + xlab("Pedigree Function") + ylab("Frequency") + ggtitle("Pedigree Function")
p4 <- ggplot(data, aes(x = data$Age, fill = Outcome)) + geom_histogram(binwidth = 1) + theme_hc() + xlab("Age [years]") + ylab("Frequency") + ggtitle("Age")
grid.arrange(p1, p2, p3, p4, nrow = 2)

col.colors <- colorRampPalette(c("#3B3B98", "white", "#F97F51"))
colnames(data) <- c("Pregnancies", "Glucose", "Blood Pressure", "Skin Thickness", "Insulin", "BMI", "Pedigree Function", "Age", "Outcome")
correlations <- cor(x = data[, -length(data)])
corrplot(correlations, method = 'shade', order = "FPC", tl.col = 'black', col = col.colors(100))
