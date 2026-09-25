# Q1
# Read data
carclaims.df <- read.table("carclaims.R",header=T)
attach(carclaims.df)

# Part a is obvious

# Part b
# Fitting models
model1 = glm(numclaims ~ age, family = poisson)
model2 = glm(numclaims ~ factor(sex), family = poisson)
model3 = glm(numclaims ~ factor(cargroup), family = poisson)
model4 = glm(numclaims ~ factor(sex) + factor(cargroup), family = poisson)
model5 = glm(numclaims ~ age + factor(cargroup), family = poisson)
model6 = glm(numclaims ~ age + factor(sex) + factor(cargroup), family = poisson)

# Checking
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)

# DO NOT COMPARE P-VALUES
# Tests to see which model to choose
1- pchisq(5.16, df = 1) # At 5%, the model with age perform better than null model. 

1- pchisq(0.005, df = 1) 
# At 5%, we do not reject H_0. adding variable sex do not significantly 
# improve the model comparing to null model. 

1- pchisq(10.138, df = 2)

# We compare to model 3 because these are nested models. 
1- pchisq(0.032, df = 1) 
1- pchisq(5.96, df = 1)
1- pchisq(0.003, df = 1)

# Check for model 5
anova(model3, model5, test="Chi")

# Part c
# Comparing with the maximal model
1 - pchisq(model5$deviance, model5$df.residual)
# Comment: Model fit is adequate for Model 5

# Part d
predict(model5, data.frame(age = 36, sex="f", cargroup="A"), type="link")
predict(model5, data.frame(age = 36, sex="f", cargroup="A"), type="response")

# Although model 5 best AIC, but residual plot unsatisfactory. 


