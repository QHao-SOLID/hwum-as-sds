# Q1
# Prep data
data = scan()
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
11 14 15 12 11 14 15 19 21 24 21 28 30 34 39 50 60 69

# Set up a matrix containing the data
data.mat = matrix(data, ncol=3, byrow=F)

# Set up a data frame containing the data
df = data.frame(data.mat, row.names=1)
names(df) <- c("i", "N")
df
attach(df)

# Part A
plot(i,N, main = "Plot of N against i", col = "red")

# Part B
model1=glm(N~i, family=poisson)
summary(model1)

# Part C
fit10 = exp(2.032143 + 0.115100 * i[10])
fit10
model1$fitted.values[10]

# Part D
plot(residuals(model1,type="response") ~ i, main = "Plot of response residuals")
plot(residuals(model1,type="pear") ~ i, main = "Plot of Pearson residuals")
plot(residuals(model1) ~ i, main = "Plot of Deviance residuals")

detach(df)

# Q2
# Prep data
df2 = read.table("C:/Users/marcu/OneDrive/Desktop/02_Courses/03_Stat Models B/99_Datasets/cells.R", header = T)
attach(df2)

# Part A
propdead = nkilled/size
propdead

# Part B
logit = function(p){
  log(p/(1-p),exp(1))
}

plot(dose, propdead, main = "Propdead against dose")
plot(dose, logit(propdead), main = "logit(Propdead) against dose")

# Part C
model2 = glm(propdead ~ dose, weights = size, family = binomial)
summary(model2)

