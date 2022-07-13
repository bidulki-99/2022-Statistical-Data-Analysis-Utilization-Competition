# Linear Regression
data(cars)
str(cars)

plot(cars$speed, cars$dist)
cor.test(cars$speed, cars$dist)

a <- lm(cars$dist ~ cars$speed)
summary(a)
plot(a)

# Multiple Linear Regression
data(iris)
head(iris)

plot(iris[, 1:4])
cor(iris[, 1:4])

a <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
summary(a)
plot(a)

# Optimized Regression Equation Selection
data(mtcars)
head(mtcars)

step(lm(disp ~ mpg + cyl + hp + drat + wt, data = mtcars), direction = "both")
