require(here)
dat_hab = read.csv(here("data", "hab.sta.csv"))
head(dat_hab)

png(
  filename = here("W5_hist.png" ), 
  width = 1500, height = 1600, 
  res = 180, units = "px")

par(mfrow = c(3, 1))
hist(dat_hab$elev)
hist(dat_hab$slope)
hist(dat_hab$aspect)

dev.off()


png(
  filename = here("W5_plot.png" ), 
  width = 1500, height = 1600, 
  res = 180, units = "px")

par(mfrow = c(3, 1))
plot(x = dat_hab$elev, y = dat_hab$ba.tot, ylim=c(0,100))
abline(lm(y ~ x))

plot(x = dat_hab$slope, y = dat_hab$ba.tot, ylim=c(0,100))
abline(lm(y ~ x))

plot(x = dat_hab$aspect, y = dat_hab$ba.tot, ylim=c(0,100))
abline(lm(y ~ x))

dev.off()