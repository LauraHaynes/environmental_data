require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

install.packages("simpleboot")
require(simpleboot)
?two.boot()
a_fliplength = droplevels(subset(penguin_dat, species != "Chinstrap", na.rm = TRUE))
a_fliplength = a_fliplength$flipper_length_mm
c_fliplength = droplevels(subset(penguin_dat, species != "Adelie", na.rm = TRUE))
c_fliplength = c_fliplength$flipper_length_mm
pen_boot = two.boot(a_fliplength, c_fliplength, na.rm = TRUE, FUN = mean, R = 1000)
str(pen_boot)

#Q1
sd(pen_boot$t)

#Q2
hist(pen_boot$t,
     main = "Laura's histogram of 1000 bootstrapped 
     diff in mean penguin flipper length", 
     xlab = "Diff in mean flipper length (mm) 
     in Adelie & Chinstrap Penguins",
    )

#Q3
boot.ci(pen_boot)
quantile(pen_boot$t, c(0.025, 0.975))

#Q4
mean(pen_boot$t)
median(pen_boot$t)

?pnorm()
?ecdf()

#Q6
pen_ecdf = ecdf(pen_boot$t)

#Q7
1-pen_ecdf(-4.5)

#Q7
pen_ecdf(-8)

#Q9
require(here)
veg = read.csv(here("environmental_data", "data", "vegdata.csv"))
boxplot(pine ~ treatment, dat = veg)
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, dat = dat_tree)
table(dat_tree$pine)

#9
?t.test()
?wilcox.test()
wilcox.test(pine ~ treatment, dat = dat_tree, alternative = "two.sided", na.rm = TRUE)

control = droplevels(subset(dat_tree, treatment != "clipped", na.rm = TRUE))
control = control$pine
clipped = droplevels(subset(dat_tree, treatment != "control", na.rm = TRUE))
clipped = clipped$pine

require(boot)
tree_boot = two.boot(clipped, control, na.rm = TRUE, FUN = mean, R = 1000)
str(tree_boot)
boot.ci(tree_boot)
quantile(tree_boot$t, c(0.025, 0.975))



dat_bird = read.csv(here("environmental_data", "data", "bird.sub.csv"))
dat_hab = read.csv(here("environmental_data", "data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_hab,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)

dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd
mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)


plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))


set.seed(123)
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices (MC resampled data)",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

?sample()

#14
m = 10000 
result_mc = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2]  
  
  result_mc[i] = coef(fit_resampled_i)[2]
} 

#Q15
par(mfrow = c(1, 1))

hist(
  result_mc,
  main = "Laura's Null Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)

?abline()
#Q16
quantile(result_mc, c(.05))
#critical value
abline(v = -0.0129999, lty = 2, col = "red", lwd = 2)

set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)

dat_boot = dat_1[index_1, ]
head(dat_boot)

fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)

coef(fit_bs1)

#idk
m = 10000 
result_boot = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)

  dat_boot = dat_1[index_1, ]
  fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)

  head(dat_boot)

  coef(fit_bs1)
  
result_boot[i] = coef(fit_bs1)[2]
} 



hist(
  result_boot,
  main = "Laura's Alternative Distribution 
  of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = 0, lty = 2, col = 1, lwd = 2)


par(mfrow = c(1, 2))

hist(
  result_mc,
  main = "Null Hypothesis"
)

hist(
  result_boot,
  main = "Alt. Hypothesis"
)

par(mfrow = c(1, 1))

#Alt
plot(
  density(result_mc) ,
  main = "Laura's Alt & Null Distribution",
  xlab = "Slope Coefficient"
  )

#Null
plot(
  density(result_boot),
  xlim = c(-.05, 0.04),
  main = "Laura's Alt & Null Distribution",
  xlab = "Slope Coefficient",
  col = "blue")
polygon(density(result_boot), col = rgb(1, 0.89, 1, alpha = 0.6))


lines(density(result_mc))
polygon(density(result_mc), col = rgb(0.3, 0.97, 1, alpha = 0.6))


legend(
  'topright',
  legend=c('Alt.','Null'),
  lty=c(1,1),col=c(rgb(1, 0.89, 1, alpha = 0.6), rgb(0.3, 0.97, 1, alpha = 0.6)), inset=c(.001,.001))
