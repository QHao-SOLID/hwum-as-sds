# Loading library required
library(MASS)

################################################################################
# Q1
################################################################################

# Loading the data frame and attaching it
filepath = "C:/Users/marcu/OneDrive/Desktop/02_Courses/03_Stat Models B/04_Assignment/Assignment 2/datasets/MobileNetwork.txt"
mobile_network = read.table(filepath, 
                   header = TRUE)

attach(mobile_network)

# Part A
# Function to calculate and display correlations (to be looped)
panel.cor <- function(x, y, ...) {
  usr <- par("usr"); on.exit(par(usr)) # retrieve coordinates of each plot
  par(usr = c(0, 1, 0, 1)) # set x and y axis range from 0 to 1
  r <- round(cor(x, y), 2) # calculate correlations
  text(0.5, 0.5, r, cex = 1.5) # place result in middle (0.5,0.5)
}

# Pairwise plot
pairs(mobile_network, 
      lower.panel = panel.smooth, # Trend line
      upper.panel = panel.cor, # Correlation panels
      main = "Pairwise Relationships Between Variables")

# Part B
# Model 1
lm.model1 = lm(speed ~ bandwidth + signal + legacy_users)
summary(lm.model1)
summary.aov(lm.model1)

# Model 2 (Preferred model)
lm.model2 = lm(speed ~ bandwidth + signal)
summary(lm.model2)
summary.aov(lm.model2)

# Diagnostic plots for preferred model
par(mfrow=c(2,2))
plot(lm.model2, which = 1:4)

# Part C
# Model 3
lm.model3 = lm(speed ~ bandwidth +  signal * I(bandwidth^2))
summary(lm.model3)
summary.aov(lm.model3)

# Model 4 (Preferred model)
lm.model4 = lm(speed ~ bandwidth + signal + I(bandwidth^2))
summary(lm.model4)
summary.aov(lm.model4)

# Diagnostic plots for preferred model
plot(lm.model4, which = 1:4)
cooks.distance(lm.model4)[186]

# Part D
# Comparison between best models from parts b and c
# Plot of fitted values against actual values for Model 4
par(mfrow=c(1,1))

plot(speed, lm.model4$fitted.values,
     xlab = "Actual mobile data speed",
     ylab = "Fitted values of mobile data speed",
     main = "Scatter Plot of Fitted Versus Actual Mobile Data Speed for Model 4",
     col = "blue")

abline(a = 0, b = 1, col = "red")
legend("topleft", 
       legend = "Y = X", 
       col = "red", 
       lty = 1, 
       lwd = 2)

# Detaching dataset
detach(mobile_network)

################################################################################
# Q2
################################################################################

# Loading the data frame and attaching it
rent <- read.table("rent.csv",
                   header = TRUE, sep = ",")

attach(rent)

# Part A
# Numerical summaries for: 
# casual
summary(casual)
sd(casual)
hist(casual, 
     main = "Histogram of the number of casual bike rentals", 
     xlab = "Number of casual bike rentals", 
     col = "cyan")

# temp
par(mfrow=c(1,2))
summary(temp)
sd(temp)
hist(temp, 
     main = "Histogram of normalised daily temperature", 
     xlab = "normalised daily temperature", 
     col = "lightcyan")

# season
season_freq = table(season)

bar = barplot(season_freq, 
        xlab = "Seasons", 
        ylab = "Frequency", 
        main = "Bar Plot of Seasons", 
        names.arg = c("Spring", "Summer", "Fall", "Winter"), 
        ylim = c(0,110), 
        col = c("pink", "darkseagreen1", "gray76", "lightcyan"),
        border = "black")

text(x= bar, y=season_freq, pos = 3, label = season_freq, cex = 0.8)

# Part B
# Plot of casual against season
par(mfrow=c(1,2))
season_cleaned = factor(season)

boxplot(casual ~ season,
        xlab = "Season", ylab = "Casual bike rentals",
        names = c("Spring", "Summer", "Fall", "Winter"), 
        main = "Casual bike rentals by season", 
        col = c("pink", "darkseagreen1", "gray76", "lightcyan"))

# Plot of casual against temp, distinguished by season
plot(casual ~ temp, type='n', 
     main = "Scatter plot of casual against temp by seasons", 
     xlab = "Normalised daily temperature", 
     ylab = "Casual bike rentals")
points(casual[season == 1] ~ temp[season == 1], col = "red")
points(casual[season == 2] ~ temp[season == 2], col = "green")
points(casual[season == 3] ~ temp[season == 3], col = "blue")
points(casual[season == 4] ~ temp[season == 4], col = "purple")
legend("topleft", legend=c("Spring", "Summer", "Fall", "Winter"), 
       fill=c("red", "green", "blue", "purple"), 
       cex = 0.7)

# Part C
# Creating contingency matrices for both cases
# Case 1
matrix1 = matrix(c(sum(season == 1 & holiday == 0), 
                   sum(season == 1 & holiday == 1), 
                   sum(season == 2 & holiday == 0), 
                   sum(season == 2 & holiday == 1), 
                   sum(season == 3 & holiday == 0), 
                   sum(season == 3 & holiday == 1), 
                   sum(season == 4 & holiday == 0), 
                   sum(season == 4 & holiday == 1)), 2, 4)
matrix1

# Case 2
matrix2 = matrix(c(sum(workingday == 0 & holiday == 0), 
                   sum(workingday == 0 & holiday == 1), 
                   sum(workingday == 1 & holiday == 0), 
                   sum(workingday == 1 & holiday == 1)), 2, 2)
matrix2

# Since both matrices have expected frequency cells lesser than 5, 
# we use Fisher's exact test.
fisher.test(matrix1)
fisher.test(matrix2)

# Part D
# Main model
glm.model1 = glm(casual ~ temp * factor(season), family = poisson)
summary(glm.model1)
anova(glm.model1)

# Nested models
glm.model2 = glm(casual ~ temp + temp : factor(season), family = poisson)
glm.model3 = glm(casual ~ factor(season) + temp : factor(season), family = poisson)
glm.model4 = glm(casual ~ temp + factor(season), family = poisson)
glm.model5 <- glm(casual ~ 1, family = poisson)

# Making all models into a list for easier looping. 
glm.models = list(glm.model1 = glm.model1, 
                  glm.model2 = glm.model2, 
                  glm.model3 = glm.model3, 
                  glm.model4 = glm.model4, 
                  glm.model5 = glm.model5)

# Residual Deviance for each model
res_dev_glm = sapply(glm.models, deviance)
res_dev_glm

# Residual Deviance's degrees of freedom for each model
res_dev_df_glm = sapply(glm.models, df.residual)
res_dev_df_glm

# AIC values for each model
AIC_glm = sapply(glm.models, AIC)
AIC_glm

# Analysis of Deviance
anova(glm.model2, glm.model1)
anova(glm.model3, glm.model1) # Note: Essentially the same model
anova(glm.model4, glm.model1)
anova(glm.model5, glm.model1)

# p-value to test significance of nb.model against maximal model
1 - pchisq(glm.model1$deviance, glm.model1$df.residual)

# Plot of deviance residuals against fitted values
par(mfrow=c(1,1))
plot(fitted(glm.model1),
     residuals(glm.model1, type = "deviance"),
     xlab = "Fitted values",
     ylab = "Deviance residuals",
     main = "Deviance Residuals vs Fitted Values"
)

abline(h = 0, lty = 2, col = "red")

# Plot of fitted values against actual values
plot(casual, glm.model1$fitted.values,
     xlab = "Actual casual bike rentals",
     ylab = "Fitted values of casual bike rentals",
     main = "Scatter Plot of Fitted Versus Actual Casual Bike Rentals for Model A",
     col = "blue")

abline(a = 0, b = 1, col = "red")
legend("bottomright", 
       legend = "Y = X", 
       col = "red", 
       lty = 1, 
       lwd = 2)

# Conclusion: Choose glm.model1

# Part E
# Fitting same model into negative binomial model
nb.model = glm.nb(casual ~ temp * factor(season))
summary(nb.model)

# Compare against saturated model
nb.model$deviance / nb.model$df.residual

# Plot of deviance residuals against fitted values
plot(fitted(nb.model),
     residuals(nb.model, type = "deviance"),
     xlab = "Fitted values",
     ylab = "Deviance residuals",
     main = "Deviance Residuals vs Fitted Values"
)

abline(h = 0, lty = 2, col = "red")

# Conclusion, nb.model more adequate
# Part F to be explained in the report. 
