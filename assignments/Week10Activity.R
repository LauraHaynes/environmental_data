data(iris)
fit_species = 
  lm(
    Sepal.Length ~ Species,
    data = iris)
summary(fit_species)


boxplot(
  Sepal.Length ~ Species,
  data = iris,
  ylab = "Sepal Length (cm)",
  xlab = "Species")

shapiro.test(residuals(fit_species))


fit_petals =
  lm(
    Petal.Width ~ Petal.Length,
     data = iris
  )

summary(fit_petals)

plot(
  Petal.Width ~ Petal.Length,
  data = iris,
  ylab = "Sepal Length (cm)",
  xlab = "Species")
