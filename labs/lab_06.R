require(palmerpenguins)
#R Function for the SE
sse_mean = function(x)sd(penguins$bill_depth_mm, na.rm=TRUE)/sqrt(length
(penguins$bill_depth_mm))

sse_mean(penguins$bill_depth_mm)

#penguin data
boxplot(
  flipper_length_mm ~ species, data = penguins,
  ylab = "Flipper length (mm)")

#2-species data
dat_pen = subset(penguins, species != "Gentoo")
boxplot(
  flipper_length_mm ~ species, data = dat_pen,
  ylab = "Flipper length (mm)")

#drop levels
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)")
}

#Resampling w/ replacement

set.seed(123)

flipper_shuffled = sample(
  penguins$flipper_length_mm, replace = TRUE)

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_shuffled ~ penguins$species,
    ylab = "Flipper length (mm)",
    main = "MonteCarlo Resampled Data",
    xlab = "species")
}

#Bootstrap Resampling and Alternative Hypotheses
penguins2 = penguins[sample(1:nrow(penguins), replace = T), ]

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_length_mm ~ species, data = penguins2,
    ylab = "Flipper length (mm)",
    main = "Bootstrap Data")
}

#repeated MC resampling
par(mfrow = c(4, 4), mar = c(1, 1, 1, 1))
for (i in 1:16)
{
  
  flipper_shuffled = sample(
    penguins$flipper_length_mm, replace = TRUE)
  
  boxplot(
    flipper_shuffled ~ penguins$species,
    ann = F, axes = F)
  box()
  
}

#Classical t-test: Adelie and Chinstrap penguins
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

#Two-sample resampling
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

par(mfrow = c(1, 1))
boxplot(flipper_shuffled ~ dat_pen$species)

#classical test on resampled data
t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

#difference of means
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

t_test$estimate

diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

#Using aggregate()

agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

table(dat_pen$species)

n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

print(c(observed = diff_observed, simulated = diff_simulated))


#Simulation function
x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

two_group_resample_diff = function(x, n_1, n_2) 
{
 x_ok = x[!is.na(x)] 
 
 x1 = sample(x_ok, size = n_1, replace = T)
 x2 = sample(x_ok, size = n_2, replace = T)
 return(mean(x1) - mean(x2))
 
}

set.seed(54321)
two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)

#resampling experiment
n = 200
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}

sum(abs(mean_differences) >= diff_observed)
hist(mean_differences)

t_test = t.test(flipper_shuffled ~ dat_pen$species)

str(t_test)

t_test$estimate

#Q1

#how many na values are there?
is.na(penguins$body_mass_g)
#what does T/F correspond to?
print(penguins$body_mass_g)

rm(list = ls())

sse_mean = function(x)
{

  sse = sd(x, na.rm=TRUE)/sqrt(length(x) - sum(is.na(x)))
  return(sse)
}

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

#Q2

?mean()

two_group_resample_diff = function(x, n_1, n_2) 
{
  x_ok = x[!is.na(x)] 
  
  x1 = sample(x_ok, size = n_1, replace = T)
  x2 = sample(x_ok, size = n_2, replace = T)
  
  difference_in_means = mean(x1) - mean(x2)
  
  return(difference_in_means)
  
}

#Q4

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 86, 152)
  )
}
?hist()
hist(mean_differences)

#Q5
sum(abs(mean_differences) > 5.8)


#Q7

dat_pen = subset(penguins, species != "Gentoo")
boxplot(
  dat_pen$bill_depth_mm ~ species, data = dat_pen,
  ylab = "Bill Depth (mm)")

#Q8
agg_means = aggregate(
  bill_depth_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_crit = diff(agg_means[, 2])

agg_means
diff_crit

#Q9
t.test(dat_pen$bill_depth_mm ~ dat_pen$species)

#Q10

n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$bill_depth_mm, 86, 152)
  )
}

sum(abs(mean_differences) > diff_crit)


#Q11
hist(mean_differences, 
     main = "Bill Depth Mean Differences")

