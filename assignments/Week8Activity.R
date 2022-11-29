#walkthrough
#Likelihood of 2 observations
x_observed = c(2, 6)
print(x_observed)

dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)

dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)

wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)

prod(dpois(x = wiwa_counts, lambda = 4.5))

sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

summary(dat_all$WIWA)

hist(dat_all$WIWA)

hist(dat_all$WIWA, breaks = 7)

hist(dat_all$WIWA, breaks = 0:7)

hist(dat_all$WIWA, breaks = 0:7 - .5)


par(mfrow = c(1, 2))
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")

sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))

#Q1
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4)
sum(log(dpois(x = wiwa_counts, lambda = 4))
    
#Q3
par(mfrow = c(1, 1))

hist(dat_all$WIWR)

head(dat_all$WIWR)

hist(dat_all$WIWR, breaks = 0:7 - .5)

dat = dat_all$WIWR
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Hist. Winter Wren Counts")

#Q5
sum(log(dpois(x = dat_all$WIWR, lambda = 1.46)))

#Q6

?dbinom()

summary(dat_all$WIWR)

#12
set.seed(1) 

vec_rnorm = rnorm(n = 10, mean = 0, sd = 1) 
sum(log(dnorm(vec_rnorm, mean = 0,sd = 1)))

#Q15-16

require(palmerpenguins)

