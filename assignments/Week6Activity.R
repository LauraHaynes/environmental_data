x_bin = 0.5
y_bin_2 = dbinom(x_bin, size = 5, prob = 0.4)

barplot(
  height = y_bin_2,
  # the names to print with each bar:
  names.arg = x_bin,
  # Tells R to remove space between bars:
  space = 0,
  ylab = "Pr(x)",
  main = "Binomial: n = 5, p = 0.4")

#Q2

?dbinom
dbinom(x=4, size = 6, prob = 2/3, log = FALSE)

#Q3

dbinom(x=0, size = 7, prob = 2/3, log = FALSE)

#Q4
ppois(q = 7, lambda = 10.4)
1 - ppois(q = 7, lambda = 10.4)
pbinom(q = 4, size = 6, prob = 2/3, lower.tail = TRUE, log.p = FALSE) 

#Q5
pbinom(q = 4, size = 6, prob = 2/3, lower.tail = FALSE, log.p = FALSE)

#Q7
pnorm(1:2, mean = 0, sd = 1)
pnorm(q=1,mean=0,sd=1, lower.tail=TRUE, log.p=FALSE)

?pnorm
#Q8

pnorm(q=1:2,mean=0,sd=1, lower.tail=TRUE, log.p=FALSE)

#Q9
# How many points?
n = 13

# Create a vector of x-values from -4 to 4:
x = seq(from = -6, to = 6, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)

# plot!
plot(y ~ x, type = "l")


# How many points?
n = 1000

# Create a vector of x-values from -4 to 4:
x = seq(from = -6, to = 6, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)

# plot!
par(mfrow = c(1, 2))

plot(y ~ x, type = "l", ylab = "Probability Density")

y_2 = dnorm(x, mean = 0, sd = 2)


plot(y ~ x, type = "l", ylab = "Probability Density")
points(y_2 ~ x, type = "l", lty  = 2)

y_3 = dnorm(x, mean = -2, sd = 1)
plot(y ~ x, type = "l", ylab = "Probability Density")
points(y_3 ~ x, type = "l", lty  = 2)


y_2 = dnorm(x, mean = 0, sd = 2) 

y2 = dnorm(x, mean = -2, sd= 1) 





plot(y ~ x, type = "l", ylab = "Probability Density") 

points(y_2 ~ x, type = "l", lty  = 2) 

points(y2 ~ x, type = "l", ylab = "Probability Density", lty  = 2) 


y_cdf_1 = pnorm(x, mean = 0, sd = 1) 

plot(y_cdf_1 ~ x, type = "l", ylab = "cumulative density") 

y_cdf3 = pnorm(x, mean= -2, sd=1) 



y_cdf_2 = pnorm(x, mean = 0, sd = 2) 

plot(y_cdf_1 ~ x, type = "l", ylab = "Cumulative Density") 

points(y_cdf3 ~ x, type = "l", lty = 2) 

plot() 

#BINOMIAL
x_bin = 0:5
y_bin_2 = dbinom(x_bin, size = 6 prob = 2/3)

barplot(
  height = y_bin_2,
  # the names to print with each bar:
  names.arg = x_bin,
  # Tells R to remove space between bars:
  space = 0,
  ylab = "Pr(x)",
  main = "Binomial: n = 6, p = 2/3")