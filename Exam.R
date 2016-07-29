
# ---------------------------------------------------------------------------

# Question 10, Chapter 3

library (MASS)

# a
str(Boston)
?Boston

n <- dim(Boston)
cat("Number of rows = ", n[1], "\n")
cat("Number of columns", n[2], "\n")

# b
pairs(Boston)

# lstat medv - linear
# nox dis
# age dis
# rm medv
# nox dis - non linear
pairs(Boston[c("lstat","medv","nox","dis","age","rm")])

# c
par(mfrow = c(1, 1))

sapply(2:length(Boston), function(x)
{
  plot(Boston[,c(1,x)], pch = 4, cex=0.7)
}
)

# medv chas age dis

# d
sortedBy_crime_rates <- Boston[order(Boston$crim, decreasing = T),]
sortedBy_crime_rates[1:6,]

sortedBy_tax <- Boston[order(Boston$tax, decreasing = T),]
sortedBy_tax[1:6,]

sortedBy_ptratio <- Boston[order(Boston$ptratio, decreasing = T),]
sortedBy_ptratio[1:6,]

# e
table(Boston$chas)

# f
summary(Boston$ptratio)

# g
which.min(Boston$medv)
Boston[which.min(Boston$medv),]

summary(Boston)
# Crime rates - Crime rates are high and lie between the 3rd and 4th quartile
# zn - there is no land zoned for large lots which is in the lowest point in the range with the lowest value
# indus - indus value lies at the end of the 3rd quartile of the range
# chas - It does not lie along the Charles river like most other suburbs
# nox - it lies between the 3rd and 4th quartile of teh range
# rm - it lies in the first quartile in the range
# age - it is at the highest point of the range with the max value
# dis - it lies in the first quartile of the range
# rad - it is at the highest point of the range with the max value
# tax - (full-value property-tax rate per /$10,000.) is on the higher side, and lies at the third quaritle (666)
# ptratio - (pupil-teacher ratio) is amongst the highest and lies at the third quartile of the dataset (20.20)
# black - (1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.) is the maximum value in the dataset (396.9)
# lstat - (lower status of the population in percent) is very high. Lies between 3rd quartile (16.95) and max (37.97)

# h
dim(Boston[which(Boston$rm > 7), ])[1]
dim(Boston[which(Boston$rm > 8), ])[1]

# The median of medv of these suburbs is much higher than the whole dataset
# Majority of these houses are are in areas of high crime rates
# These suburbs have lower percentage of people belonging to the lower status of the population
# There is a lower proportion of African Americans in these suburbs

# -----------------------------------------------------------------------------

# Question 15, Chapter 4

library (MASS)

# for (i in 2:length(names(Boston))) {
#   paste("model_",i, sep = "") <- lm(Boston$crim ~ as.data.frame(Boston[i]))
#   # plot(names(Boston)[i], crim, pch=19, cex=0.5,xlab=names(Boston)[i],ylab = "crim")
#   # abline(model$coef[1],model$coef[2],col=2,lwd=2)
# }

# a
lapply( Boston[,-1], function(x) {
    summary(lm(Boston$crim ~ x))
  }
)

lapply( Boston[,-1], function(x) {
    model <- lm(Boston$crim ~ x)
    plot(x, Boston$crim, pch = 4, cex=0.7)
    abline(model$coef[1],model$coef[2],col=2,lwd=2)
  }
)

# b
multilinear_model <- lm(crim ~ ., data=Boston)
summary(multilinear_model)
# t-value < 1: indus (0.766), chas (0.635), rm (0.702), age (0.08), tax (0.733)

# c 
coefficients = NULL
lapply( Boston[,-1], function(x) 
  {
    model <- lm(Boston$crim ~ x)
    coefficients <<- c(coefficients,model$coefficients[2])
    plot(x, Boston$crim)
    abline(model$coef[1],model$coef[2],col=2,lwd=2)
  }
      )
# coefficients
multi_coefficients <- multilinear_model$coefficients[2:14]

names <- NULL
lapply(seq_along(Boston[,-1]), function(x) {
  names <<- names(Boston)
  }
)
# names

names(coefficients) <- names[2:14]
plot(coefficients, multi_coefficients, pch = 4, cex=0.7, col=2)


# d
lapply( Boston[,-1], function(x) 
  {
    x2 = x^2
    x3 = x^3
    summary(lm(Boston$crim ~ x + x2 + x3))
  }
      )


