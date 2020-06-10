raw.data <- read.csv(file.choose())

View(raw.data)
# What types of variables are in this data set?
str(raw.data)

# We won't be able to use all of these variables why?
plot(raw.data$Price ~ raw.data$Mfg_Year)
subset <- raw.data[,c(3:7)]
plot(subset)
plot(subset$KM ~ subset$Age_08_04)
# What can we learn from this correlation?
plot(subset$Price ~ subset$KM)
# What can we learn from this?
plot(subset)
plot(subset$Age_08_04 ~ subset$Mfg_Year)
plot(subset$Age_08_04 ~ subset$Mfg_Month)
# can we learn anything from this data?
plot(subset)

subset <- raw.data[,c(3:4,7,9,13:18)]
plot(subset)

# Run one linear regression
plot(subset$Price ~ subset$KM)
price_km_linear_regression <- lm(subset$Price ~ subset$KM)
# Plot this line on the graph
abline(price_km_linear_regression, col = "Green")
summary(price_km_linear_regression)

plot(price_km_linear_regression)

mlr.2factors <- lm(subset$Price ~ subset$KM + subset$Weight)
summary(mlr.2factors)

mlr.2factors <- lm(subset$Price ~ subset$KM + subset$Age_08_04)
summary(mlr.2factors)

mlr.3factors <- lm(subset$Price ~ subset$KM + subset$Age_08_04 + subset$Weight)
summary(mlr.3factors)


# Run an MLR
mlr.allfactors <- lm(subset$Price ~ 
                       subset$KM + 
                       subset$Age_08_04 + 
                       subset$HP + 
                       subset$cc + 
                       subset$Doors + 
                       subset$Cylinders +
                       subset$Gears + 
                       subset$Quarterly_Tax + 
                       subset$Weight
)
summary(mlr.allfactors)


mlr.4factors <- lm(subset$Price ~ subset$KM + subset$Age_08_04 + subset$HP + subset$Weight)
summary(mlr.4factors)

# Are the extra factors worth it? 
# .8614 - 4 factors
# .863  - All factors

# How do we calculate the predicted vs actual

coef(mlr.4factors)
plot(density(resid(mlr.4factors)))
resid(mlr.4factors)
subset["residual"] <- resid(mlr.4factors)
new_data = data.frame(KM=1000, Age_08_04=15, HP=100, Weight=1200)

#######################END################################
