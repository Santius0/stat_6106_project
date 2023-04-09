library(ggplot2)

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


# fetching data
working_dir <- getwd()
data_fp <- file.path(working_dir, "data\\computer_hardware\\machine.data")
data <- read.table(data_fp, header=FALSE, sep=",")
colnames(data) <- c("vendor", "model", "MYCT", "MMIN", "MMAX", "CACH", "CHMIN", "CHMAX", "PRP", "ERP")

data$vendor <- as.numeric(as.factor(data$vendor))
data$model <- as.numeric(as.factor(data$model))


model.full <- lm(PRP ~ vendor + model + MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX, data = data)
summary(model.full)
anova(model.full)

model.reduced <- lm(PRP ~ model + MYCT + MMIN + MMAX + CACH + CHMAX, data = data)
summary(model.reduced)
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

pairs(~ PRP + model + MYCT + MMIN + MMAX + CACH + CHMAX, data = data, main="Scatterplot Matrix")
datamatrix <- cbind(data$PRP, data$model, data$MYCT, data$MMIN, data$MMAX, data$CACH, data$CHMAX)
cor(datamatrix)

# model has no significant linear relationships with anything in the reduced model at a 99% significance level
# at 95% a significant linear relationship between model and MMAX and CHMAX is observed
cor.test(data$model, data$MYCT)
cor.test(data$model, data$MMIN)
cor.test(data$model, data$MMAX)
cor.test(data$model, data$CACH)
cor.test(data$model, data$CHMAX)

# MYCT has a significant linear relationship with everything in the reduced model except model
# i.e MMIN, MMAX, CACH and CHMAX
cor.test(data$MYCT, data$MMIN)
cor.test(data$MYCT, data$MMAX)
cor.test(data$MYCT, data$CACH)
cor.test(data$MYCT, data$CHMAX)

# MMIN has a signifacnt linear relationship with MMAX, CACH and CHMAX
cor.test(data$MMIN, data$MMAX)
cor.test(data$MMIN, data$CACH)
cor.test(data$MMIN, data$CHMAX)

# MMAX has a significant linear relationship with CACH and CHMAX
cor.test(data$MMAX, data$CACH)
cor.test(data$MMAX, data$CHMAX)

# CACH has a significant linear relationship with CHMAX
cor.test(data$CACH, data$CHMAX)

# Write up summary of colinearity here:
#     <Basically how theres colinearily all over the fucking place>

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
  labs(title = "Actual vs. Predicted Global Sales",
       x = "Actual Global Sales (Millions)",
       y = "Predicted Global Sales (Millions)") +
  theme_minimal()

