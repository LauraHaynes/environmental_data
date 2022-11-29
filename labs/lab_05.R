ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

#making the exponential function:

exp_fun = function(x, a, b)
{
  return(a * exp(-b * x)) 
}

curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()


# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)

error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

error_mean = 0
error_sd = 0.1

y_observed_3 = 
  y_pred +
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = rexp(n, rate = 1.2))


par(mfrow = c(1, 3))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_3, main = "Normally Distributed Errors\n Exponential Variance", xlab = "", ylab = "")


fit_1 = lm(y_observed ~ x_sim)
fit_2 = lm(y_observed_2 ~ x_sim)
fit_3 = lm(y_observed_3 ~ x_sim)

par(mfrow = c(1, 3))

plot(y_observed ~ x_sim); abline(fit_1)
plot(y_observed_2 ~ x_sim); abline(fit_2)
plot(y_observed_3 ~ x_sim); abline(fit_3)

#Lab 4 begins here

require(here)

dat_disp = read.csv(here("environmental_data" , "data", "dispersal.csv"))
head(dat_disp)

#Q1

exp_fun = function(x, a, b)
{
  return(a * exp(-b * x)) 
}

#Q2

png(
  filename = here("lab_05q2"),
  width = 1500, height = 1500,
  res = 180, units = "px")

par(mfrow = c(1, 1))

curve(
  exp_fun(x, 1.9, 0.1), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

curve(
  exp_fun(x, 1.9, 0.3), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", lty = 2, lwd = 1); box()

curve(
  exp_fun(x, 1.2, 0.2), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col = "red"); box()

curve(
  exp_fun(x, 1.2, 0.4), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col = "red", lty = 2, lwd = 1); box()

dev.off()

#Q5

#curve 1: a = 25, b = 0.2, line color = black, line texture = solid
#curve 2: a = 20, b = 0.2, line color = black, line texture = dotted
#curve 3: a = 10, b = 0.2, line color = black, line texture = dotted
#curve 4: a = 75, b = 0.3, line color = red, line texture = solid
#curve 5: a = 50, b = 0.3, line color = red, line texture = dotted
#curve 6: a = 40, b = 0.3, line color = red, line texture = dotted
require(here)

png(
  filename = here("lab_05q5"),
  width = 1500, height = 1500,
  res = 180, units = "px")

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 25, .2), 
  from = 0, to = 60, add = FALSE,
  main = "Ricker function: a = 25, b = 0.2",
  ylab = "f(x)", xlab = "x")

curve(
  ricker_fun(x, 20, .2), 
  from = 0, to = 60, add = TRUE, lty = 2, lwd = 1, 
  main = "Ricker function: a = 20, b = 0.2",
  ylab = "f(x)", xlab = "x")

curve(
  ricker_fun(x, 10, .2), 
  from = 0, to = 60, add = TRUE,lty = 2, lwd = 1,
  main = "Ricker function: a = 10, b = 0.2",
  ylab = "f(x)", xlab = "x")

curve(
  ricker_fun(x, 75, b = 0.3), 
  from = 0, to = 60, add = TRUE, col = "red",
  main = "Ricker function: a = 75, b = 0.3",
  ylab = "f(x)", xlab = "x")

curve(
  ricker_fun(x, 50, b = 0.3), 
  from = 0, to = 60, add = TRUE, col = "red", lty = 2, lwd = 1,
  main = "Ricker function: a = 50, b = 0.3",
  ylab = "f(x)", xlab = "x")

curve(
  ricker_fun(x, 40, b = 0.3), 
  from = 0, to = 60, add = TRUE, col = "red", lty = 2, lwd = 1,
  main = "Ricker function: a = 40, b = 0.3",
  ylab = "f(x)", xlab = "x")

dev.off()

#Q8
dat_disp = read.csv(here("environmental_data" , "data", "dispersal.csv"))

plot(
  dat_disp$dist.class,
  dat_disp$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n(Decent) linear model")
curve(line_point_slope(x, 700, 0.3, -0.0005), add = TRUE)


#Q10

exp_fun = function(x, a, b)
{
  return(a * exp(-b * x)) 
}
plot(
  dat_disp$dist.class,
  dat_disp$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n(Decent) exp. model")
  curve(
    exp_fun(x, 1.3, .003), add = TRUE, from = 0, to = 1500,
    ann = FALSE, ylab = "f(x)"); box()
  
#Q13
  ricker_fun = function(x, a, b) 
  {
    return(a * x * exp(-b * x))
  }
  
  plot(
    dat_disp$dist.class,
    dat_disp$disp.rate.ftb,
    xlim = c(0, 1500),
    ylim = c(0, 2),
    xlab = "distance class", 
    ylab = "standardized dispersal rate", 
    main = "Marbled Salamander - first time breeders\n(Decent) Ricker model")
  curve(
    ricker_fun(x, .03, .01), 
    from = 0, to = 1500, add = TRUE,
  )

#Q14
  resids_linear <- line_point_slope(dat_disp$dist.class, 700, .3, -.0005)
  resids_exp <- exp_fun(dat_disp$dist.class, 1.3, .003)
  resids_ricker <- ricker_fun(dat_disp$dist.class, .03, .01)
  
#Q15
  png(
    filename = here("lab_05q5"),
    width = 1500, height = 1500,
    res = 180, units = "px")
par(mfrow = c(1, 1))
hist(resids_linear)
hist(resids_exp)
hist(resids_ricker)
