library(tidyverse)
library(readr)
library(ggplot2)
library(GGally)
library(caret)
library(gridExtra)
library(broom)


# Attribute Information:
#    1. vendor name: 30
#       (adviser, amdahl,apollo, basf, bti, burroughs, c.r.d, cambex, cdc, dec,
#        dg, formation, four-phase, gould, honeywell, hp, ibm, ipl, magnuson,
#        microdata, nas, ncr, nixdorf, perkin-elmer, prime, siemens, sperry,
#        sratus, wang)
#    2. Model Name: many unique symbols
#    3. MYCT: machine cycle time in nanoseconds (integer)
#    4. MMIN: minimum main memory in kilobytes (integer)
#    5. MMAX: maximum main memory in kilobytes (integer)
#    6. CACH: cache memory in kilobytes (integer)
#    7. CHMIN: minimum channels in units (integer)
#    8. CHMAX: maximum channels in units (integer)
#    9. PRP: published relative performance (integer)
#   10. ERP: estimated relative performance from the original article (integer)


# ---------------------------------------DATA DESCRIPTION---------------------------------------------------------------

# fetching data
working_dir <- getwd()
data_fp <- file.path(working_dir, "data\\computer_hardware\\machine.data")
data <- read.table(data_fp, header=FALSE, sep=",")

# naming columns because orginal data has no column names
colnames(data) <- c("vendor", "model", "MYCT", "MMIN", "MMAX", "CACH", "CHMIN", "CHMAX", "PRP", "ERP")

# encoding categorical variables
data$vendor <- as.numeric(as.factor(data$vendor))
data$model <- as.numeric(as.factor(data$model))

# removing any rows with missing data
data <- na.omit(data)

# preview data
head(data)

# check structure of data
str(data)

# check summary statistics
summary(data)

# check correlation between variables
cor(data)

# visualize correlation matrix
corrplot::corrplot(cor(data))

# visualize distribution of variables

hp1 <- ggplot(data, aes(x = vendor)) +
  geom_histogram(fill = rgb(0.678, 0.847, 0.902), color = "black") +
  ggtitle("Frequency Distribution of Vendor")

hp2 <- ggplot(data, aes(x = model)) +
  geom_histogram(fill = rgb(0.678, 0.847, 0.902), color = "black") +
  ggtitle("Frequency Distribution of Model")

dp1 <- ggplot(data, aes(x = MYCT)) +
  geom_density(fill = "lightblue") +
  labs(title = "Distribution of MYCT")

dp2 <- ggplot(data, aes(x = MMIN)) +
  geom_density(fill = "lightblue") +
  labs(title = "Distribution of MMIN")

dp3 <- ggplot(data, aes(x = MMAX)) +
  geom_density(fill = "lightblue") +
  labs(title = "Distribution of MMAX")

dp4 <- ggplot(data, aes(x = CACH)) +
  geom_density(fill = "lightblue") +
  labs(title = "Distribution of CACH")

dp5 <- ggplot(data, aes(x = CHMIN)) +
  geom_density(fill = "lightblue") +
  labs(title = "Distribution of CHMIN")

dp6 <- ggplot(data, aes(x = CHMAX)) +
  geom_density(fill = "lightblue") +
  labs(title = "Distribution of CHMAX")

dp7 <- ggplot(data, aes(x = PRP)) +
  geom_density(fill = "lightblue") +
  labs(title = "Distribution of PRP")


grid.arrange(hp1, hp2, dp1, dp2, dp3, dp4, dp5, dp6, dp7)

# visualize scatterplot matrix
ggpairs(data, aes(colour = "red"))

# ---------------------------------------MODEL BUILDING-----------------------------------------------------------------

model.full <- lm(PRP ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX, data = data)
summary(model.full)
qf(0.99, 6, 202)
anova(model.full)

model.reduced <- lm(PRP ~ MYCT + MMIN + MMAX + CACH + CHMAX, data = data)
summary(model.reduced)
qf(0.99, 5, 203)
anova(model.reduced)

anova(model.reduced, model.full)

# The residuals do not seem to follow a normal distribution
split.screen(c(2,2))
screen(1)
hist(model.reduced$residuals, main = "Histogram of Residuals")
screen(2)
qqnorm(model.reduced$residuals, pch = 20)
qqline(model.reduced$residuals, col="red")
shapiro.test(model.reduced$residuals)

pairs(~ PRP + MYCT + MMIN + MMAX + CACH + CHMAX, data = data, main="Scatterplot Matrix")
datamatrix <- cbind(data$PRP, data$MYCT, data$MMIN, data$MMAX, data$CACH, data$CHMAX)
cor(datamatrix)

# MYCT has a significant linear relationship with everything in the reduced model except model
cor_test_1 <- cor.test(data$MYCT, data$MMIN)
cor_test_2 <- cor.test(data$MYCT, data$MMAX)
cor_test_3 <- cor.test(data$MYCT, data$CACH)
cor_test_4 <- cor.test(data$MYCT, data$CHMAX)

# MMIN has a signifacnt linear relationship with MMAX, CACH and CHMAX
cor_test_5 <- cor.test(data$MMIN, data$MMAX)
cor_test_6 <- cor.test(data$MMIN, data$CACH)
cor_test_7 <- cor.test(data$MMIN, data$CHMAX)

# MMAX has a significant linear relationship with CACH and CHMAX
cor_test_8 <- cor.test(data$MMAX, data$CACH)
cor_test_9 <- cor.test(data$MMAX, data$CHMAX)

# CACH has a significant linear relationship with CHMAX
cor_test_10 <- cor.test(data$CACH, data$CHMAX)

# Combine the test results into a single data frame
cor_results <- bind_rows(
  tidy(cor_test_1),
  tidy(cor_test_2),
  tidy(cor_test_3),
  tidy(cor_test_4),
  tidy(cor_test_5),
  tidy(cor_test_6),
  tidy(cor_test_7),
  tidy(cor_test_8),
  tidy(cor_test_9),
  tidy(cor_test_10),
)
cor_results

# ---------------------------------------MODEL EVALUATION---------------------------------------------------------------

ctrl <- trainControl(method = "cv", number = 10)

# Train the linear regression model with 10-fold cross-validation using only MYCT, MMIN, MMAX, CACH, and CHMAX as predictors
kfold_model <- train(PRP ~ MYCT + MMIN + MMAX + CACH + CHMAX, data = data, method = "lm", trControl = ctrl)

# View the results of the cross-validation
kfold_model$results

predictions <- predict(model.reduced, newdata = data)
rsquared <- cor(data$PRP, predictions)^2
cat("R-squared:", rsquared, "\n")
mse <- mean((data$PRP - predictions)^2)
cat("Mean Squared Error:", mse, "\n")
mae <- mean(abs(data$PRP - predictions))
cat("Mean Absolute Error:", mae, "\n")

results <- data.frame(Actual = data$PRP, Predicted = predictions)

ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  scale_x_continuous(limits = c(min(data$PRP), max(data$PRP))) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted CPU Performance",
       x = "Actual Published Relative Performance (PRP)",
       y = "Predicted Published Relative Performance (PRP)") +
  theme_minimal()
