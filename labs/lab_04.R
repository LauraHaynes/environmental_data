# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)


n_pts        = 344
penguin_mean = 4202
penguin_sd   = 802

dat_1 = rnorm(n = 344, mean = 4202, sd = 802)
dat_2 = rnorm(n = 344, mean = 4202, sd = 802)
dat_3 = rnorm(n = 344, mean = 4202, sd = 802)
dat_4 = rnorm(n = 344, mean = 4202, sd = 802)

par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

dat_unif = runif(n = 27, min = 0, max = 4)
hist(dat_unif)

set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)


# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}


set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)


n_pts = 10
x_min = 1
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y ~ x, data = dat_random, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

head(penguins)



dat_random$y_predicted = line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)
  
head(dat_random)

dat_random$resids = y_random - dat_random$y_predicted

sum(dat_random$resids)

abs(dat_random$resids)

plot(x= dat_random$y_predicted, y = dat_random$resids)

hist(x = dat_random$resids)


#Lab Questions Begin here: 

require(here)
png(
  filename = here("lab_04_hist_01.png" ), 
  width = 1500, height = 1600, 
  res = 180, units = "px")

norm_17 = rnorm(n = 17, mean = 10.4 , sd = 2.4)
norm_30 = rnorm(n = 30, mean = 10.4 , sd = 2.4)
norm_300 = rnorm(n = 300, mean = 10.4 , sd = 2.4)
norm_3000 = rnorm(n = 3000, mean = 10.4 , sd = 2.4)

par(mfrow = c(2, 2))

hist(norm_17)
hist(norm_30)
hist(norm_300)
hist(norm_3000)


dev.off()

#Q7

require(here)
svg(
  filename = here("norm_1.svg" ))

# Generate a vector of x-values
x = seq(-20, 20, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)

plot(x, y, main = "Normal PDF: mean = 10.4 , sd = 2.4", type = "l", xlim = c(0, 20))
abline(h = 0)

dev.off()

#Q9-10 :')

require(here)
png(
  filename = here("Q10.png" ), 
  width = 1500, height = 1600, 
  res = 180, units = "px")

set.seed(46)
random_1 = runif(n = 10, min = -5, max = 4)
set.seed(1)
random_2 = runif(n = 10, min = -5, max = 4)

par(mfrow = c(2, 2))

head(random_1)

plot(x = random_1, y = random_2, main = ("Random Scatterplot"))
hist(random_2)
hist(random_1)
boxplot(random_1, main = ("Boxplot of random_1"))

dev.off()

#Q11
png(
  filename = here("Random Scatterplot with Line"),
  width = 1500, height = 1600,
  res = 180, units = "px")

par(mfrow = c(1, 1))
plot(x = random_1, y = random_2, main = ("Random Scatterplot"))

dat_randoms = data.frame(x = random_1, y = random_2)

abline(lm(y ~ x, data = dat_randoms))

#Q13

guess_x = 0
guess_y = 0
guess_slope = .7

plot(y ~ x, data = dat_randoms)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

dat_randoms$y_predicted = line_point_slope(dat_randoms$x, guess_x, guess_y, guess_slope)


dat_randoms$resids = random_2 - dat_randoms$y_predicted

head(dat_randoms)

sum(dat_randoms$resids)

png(
  filename = here("Residuals"),
  width = 1500, height = 1500,
  res = 180, units = "px")

par(mfrow = c(1, 2))
hist(dat_randoms$resids)
plot(x= dat_randoms$y_predicted, y = dat_randoms$resids)

dev.off()

