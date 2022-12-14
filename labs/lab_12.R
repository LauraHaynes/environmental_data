require(here)
dat_bird = read.csv(
  here("environmental_data" , "data", "bird.sub.csv")
)

dat_hab = read.csv(
  here("environmental_data" , "data", "hab.sub.csv")
)

birdhab = merge(dat_bird , dat_hab, by = c("basin" , "sub"))



#lab 11 Simulating Sample Sizes
alpha = 0.05
n_sims = 30
p_vals = numeric(n_sims)

sample_sizes = seq(2, 20)
sample_size_powers = numeric(length(sample_sizes))

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)

for(j in 1:length(sample_sizes))
{
  # A sequence of equally-spaced x-values:
  x_vals = seq(0, max_x, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}


sim_sample_size = 
  data.frame(
    sample_size = sample_sizes,
    power       = sample_size_powers)

par(mfrow =c(1,1))

plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power',
  xlim = c(0,20)
  )

#LOWESS model

fit_lowess_30 = loess(power ~ sample_size, data = sim_sample_size, span = 0.3)

newdata_sample_size = data.frame(sample_size = seq(2, 20, length.out = 100)) 

png(
  filename = here("Lab12LOWESS.png" ), 
  width = 1600, height = 1400, 
  res = 180, units = "px")

plot(
  x = newdata_sample_size$sample_size,
  y = predict(fit_lowess_30, newdata = newdata_sample_size),
  type = "l",
  main = "Sample size/Power Simulation \n LOWESS: 30%",
  ylab = "Statistical Power", xlab = "Sample Size")

points(power ~ sample_size, data = sim_sample_size, pch = 20, col = "purple")


legend("bottomright", legend = c("smoothed", "original"), lty=c(1,NA),pch=c(NA,20), col = c("black", "purple"))

dev.off()


#lab 5
dat_dispersal = read.csv(here("environmental_data" , "data", "dispersal.csv"))

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  ylim = c(0, 2),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n Ricker model")


#fitting NLS model in R
fit_ricker_nls = nls(
  disp.rate.ftb ~ ricker_fun(dist.class, a, b),
  data = dat_dispersal,
  start = list(b = 0, a = 1))
summary(fit_ricker_nls)

#Plotting an NLS model
dist_newdata = data.frame(dist.class = seq(0, 1600, length.out = 1600))  

lines(predict(fit_ricker_nls, newdata = dist_newdata))
legend("topright", legend = c("nls fit", "guess"), lty = c(1, 2), col = c("black", "red"))

#add guess model, assuming this is ricker?
curve(
  ricker_fun(x, .025, .01), 
  from = 0, to = 1500, add = TRUE,
  col = "red", lty = "dashed"
)


#GCKI Presence/Absence
dat_bird = read.csv(here( "environmental_data" , "data", "bird.sta.csv"))
dat_habitat = read.csv(here("environmental_data" ,"data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

dat_all$GCKI_pres = dat_all$GCKI > 0

#Binary outcome data

# Create model fits
fit_GCKI_slope = glm(GCKI_pres ~ slope, data = dat_all, family = binomial)
fit_GCKI_ba_tot = glm(GCKI_pres ~ ba.tot, data = dat_all, family = binomial)
fit_GCKI_both_additive = glm(GCKI_pres ~ slope + ba.tot, data = dat_all, family = binomial)
fit_GCKI_both_interactive = glm(GCKI_pres ~ slope * ba.tot, data = dat_all, family = binomial)

summary(fit_GCKI_slope)
summary(fit_GCKI_ba_tot)
summary(fit_GCKI_both_additive)
summary(fit_GCKI_both_interactive)

plot(fit_GCKI_both_interactive)

#plotting a fitted simple logistic model
n = 500

slope_newdata = data.frame(
  slope = seq(
    from = min(dat_all$slope, na.rm = T),
    to = max(dat_all$slope, na.rm = T),
    length.out = n
  )
)

#make a data.frame of total basal area
ba_newdata = data.frame(
  ba.tot = seq(
    from = min(dat_all$ba.tot, na.rm = T),
    to = max(dat_all$ba.tot, na.rm = T),
    length.out = n
  )
)


#Predicted Data
slope_newdata$GCKI_predicted = 
  predict(
    fit_GCKI_slope,
    newdata = slope_newdata,
    type = "response"
  )

ba_newdata$GCKI_predicted = 
  predict(
    fit_GCKI_ba_tot,
    newdata = ba_newdata,
    type = "response"
  )


require(here)
png(
  filename = here("Lab12GCKI.png" ), 
  width = 1700, height = 1400, 
  res = 180, units = "px")

#Plotting the models 1
par(mfrow = c(2, 1))

# Presence/absence data, translucent points:
plot(
  GCKI_pres ~ slope, data = dat_all,
  xlab = "Percent Slope",
  ylab = "GCKI presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)


lines(GCKI_predicted ~ slope, data = slope_newdata)

plot(
  GCKI_pres ~ ba.tot, data = dat_all,
  xlab = "Basal Area",
  ylab = "GCKI presence/absence",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)

lines(GCKI_predicted ~ ba.tot, data = ba_newdata)

dev.off()

#^need to figure out how to make it look better

#Plotting the Parameters
AIC(
  fit_GCKI_ba_tot,
  fit_GCKI_slope,
  fit_GCKI_both_additive,
  fit_GCKI_both_interactive)

#Data Setup
n = 50

ba.tot = seq(
  from = min(dat_all$ba.tot, na.rm = T),
  to = max(dat_all$ba.tot, na.rm = T),
  length.out = n)
slope = seq(
  from = min(dat_all$slope, na.rm = T),
  to = max(dat_all$slope, na.rm = T),
  length.out = n)
new_dat_all = expand.grid(
  ba.tot = ba.tot,
  slope = slope)
head(new_dat_all)
tail(new_dat_all)

new_dat_all$pred_add = predict(
  fit_GCKI_both_additive,
  newdata = new_dat_all,
  type = "response")

new_dat_all$pred_int = predict(
  fit_GCKI_both_interactive,
  newdata = new_dat_all,
  type = "response"
)

z_GCKI_add = matrix(
  new_dat_all$pred_add,
  nrow = length(ba.tot),
  byrow = FALSE)
z_GCKI_int = matrix(
  new_dat_all$pred_int,
  nrow = length(ba.tot),
  byrow = FALSE)

require(rgl)

rgl::persp3d(
  x = ba.tot,
  y = slope,
  z = z_GCKI_add,
  col = "steelblue",
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.4)
rglwidget()


#contour plot

png(
  filename = here("Lab12Contour.png" ), 
  width = 1700, height = 1300, 
  res = 180, units = "px")

par(mfrow = c(1, 2))


contour(
  x = ba.tot, y = slope,
  z = z_GCKI_add,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Additive")

contour(
  x = ba.tot,
  y = slope,
  z = z_GCKI_int,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Interactive")

dev.off()
