require(here)
dat_bird = read.csv(
  here("environmental_data" , "data", "bird.sub.csv")
)

dat_hab = read.csv(
  here("environmental_data" , "data", "hab.sub.csv")
)

birdhab = merge(dat_bird , dat_hab, by = c("basin" , "sub"))

dim(birdhab)

?plot()


plot(birdhab$ls, 
     birdhab$BRCR,
     xlab = "brown creeper abundance",
     ylab = "late-successional forest"
)

fit_1 = lm(BRCR ~ ls, data = birdhab)
abline(fit_1)

#Deterministic Model: Linear Function

linear = function(x, y_int, slope)
  
{ y = y_int+x*slope
 return(y)          
 }                 

linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 0.01)

linear_simulator = function(x, y_int, slope, st_dev)
  
{
  third = linear(x, y_int, slope)
  stoch = rnorm(n = length(x), mean = 0, sd = st_dev)
  return(third + stoch)
}

#test
n = 200

par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x,
    linear_simulator(x, y_int = 1, slope = 4.5, st_dev = 0.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2),
    axes = FALSE)
  box()
}

#test2
n = 400

par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2),
    axes = FALSE)
  box()
}

#retrieve intercept & slope coef
fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)

#retrieve sd coef
fit_1_summary = summary(fit_1)
str(fit_1_summary)
fit_1_summary$sigma

#store variables

int_obs = 0.0991
slope_obs = 0.00584
sd_obs = 0.1412668

par(mfrow = c(1,1))

plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")

#pretty plots

plot(
  birdhab$ls, birdhab$BRCR, 
  xlab = "late-successional forest extent",
  ylab = "Brown Creeper abundance",
  pch = 19)

points(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  col = adjustcolor("red", alpha = 0.3),
  pch = 16)

legend(
  "topleft",
  legend = c("data", "simulation"),
  pch = 16,
  col = c(1, adjustcolor("red", alpha = 0.3)))

#single simulation
y_sim = linear_simulator(
x = birdhab$ls,
y_int = int_obs,
slope = slope_obs,
st_dev = sd_obs
)

fit_sim = lm(y_sim ~ birdhab$ls)
summary(fit_sim)

sum_1 = summary(fit_sim)
sum_1$coefficients

sum_1$coefficients[2, 4]

#repeated simulation

n_sims = 1000
p_vals = numeric(n_sims)
alpha = 0.05
for(i in 1:n_sims)
{
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
}
sum(p_vals < alpha) / n_sims


linear_sim_fit = function(x, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}

#Simulating Effect Sizes

alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)

effect_size_powers = numeric(n_effect_sizes)

for(j in 1:n_effect_sizes)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = effect_sizes_1[j],
      st_dev = sd_obs
    )
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_size_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_effect_size = 
  data.frame(
    effect_size = effect_sizes_1,
    power       = effect_size_powers)

plot(
  power ~ effect_size, data = sim_effect_size,
  type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = slope_obs, lty = 2, col = 'red')

#Simulating Sample Sizes

alpha = 0.05
n_sims = 1000
p_vals = numeric(n_sims)

sample_sizes = seq(5, 100)
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

plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')

#Bivariate Power Analysis

alpha = 0.01
n_sims = 50

p_vals = numeric(n_sims)

n_effect_sizes = 20
effect_sizes = seq(-.01, .01, length.out = n_effect_sizes)

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)

sample_sizes = seq(10, 50)

sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))

for(k in 1:length(effect_sizes))
{
  effect_size = effect_sizes[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, max_x, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("computing effect size ", k," of ", length(effect_sizes)))
}

sim_n_effect_size = 
  list(
    power = sim_output_2,
    effect_size = effect_sizes,
    sample_size = sample_sizes
  )

#Plotting a Matrix

image(
  sim_n_effect_size$power,
  xlab = "Effect size",
  ylab = "Sample Size",
  axes = FALSE)

# add x-axis labels
axis(
  1, 
  at = c(0, 0.5, 1), 
  labels = c(-.01, 0.0, .01))

# add y=axis labels
axis(
  2, 
  at = c(0, 1), 
  labels = c(sample_sizes[1], tail(sample_sizes, 1)))

#Contour Plotting

contour(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "effect size",
  ylab = "sample size",
  main = "Contour Plot of Statistical Power",
  levels = seq(0, 1, length.out = 9),
  drawlabels = TRUE,
  # method = "simple")
  method = "edge")

#Perspective plot
persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

install.packages("rgl")
require(rgl)

persp3d(x = sim_n_effect_size$effect_size,
        y = sim_n_effect_size$sample_size,
        z = sim_n_effect_size$power,
        xlab = "beta", ylab = "n", zlab = "power",
        col = 'lightblue',
        theta = 30, phi = 30, expand = .75,
        ticktype = 'detailed')

#saving interactive plots
require(htmlwidgets)
saveWidget(
  rglwidget(),
  file = here(
    "environmental_data", "labs",
    "n_effect_size_power_sim_plot.html"),
  selfcontained = TRUE
)

#Saving R Data objects
save(
  sim_n_effect_size,
  file = here::here("environmental_data", "data", "lab_11_n_effect_sizes.Rdata"))

load(file = here::here("enviromental_data", "data", "lab_11_n_effect_sizes.Rdata"))







#Dispersion simulations

#template:
alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)

# What was the observed standard deviation?
sd_obs

# specify the number of different standard deviation values to simulate:
n_sds = 20
pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)

pop_sd_powers = numeric(length(pop_sds))

for(j in 1:length(pop_sds))
{
  pop_sd_j = pop_sds[j]
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x=birdhab$ls,
      y_int= int_obs,
      slope= slope_obs,
      st_dev= pop_sd_j
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  pop_sd_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_output_dispersion = data.frame(
  sd = pop_sds,
  power =pop_sd_powers)

# You should save your simulation results so you don't have to run it every time.
save(
  sim_output_dispersion, 
  file = here::here("environmental_data", "data" , "lab_ll_dat_dispersion_sim.RData"))

# Line plot of standard deviation (x-axis) and statistical power (y-axis)
plot(sim_output_dispersion,
     main= "Univariate graph for Q1",
     xlab= "standard deviation" ,
     ylab = "power")


# Add a dotted vertical red line at the observed population standard deviation value.
abline(v = sd_obs, lty = 2, col = 'red')


#Second Template
alpha = 0.05

# Start with a small number
n_sims = 10
p_vals = numeric(n_sims) 

# What was the observed standard deviation?
sd_obs

# specify the number of different standard deviation values to simulate:
# Start with a small number
n_sds = 20
pop_sds = seq(from = 0.05, to = , length.out = n_sds)

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)


pop_sd_powers = numeric(length(pop_sds))

sample_sizes = seq(5, 100)

sim_output_3 = matrix(nrow=length(pop_sds), ncol=length(sample_sizes))

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, max_x, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x=x_vals,
        y_int= int_obs,
        slope= slope_obs,
        st_dev= pop_sd_k
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    
    sim_output_3[k, j] = sum(p_vals < alpha) / n_sims
  }
  print(paste0("Testing standard deviation ", k, " of ", n_sds))
}

image(sim_output_3)

sim_3_dat = 
  list(
    power       = sim_output_3,
    sample_size = sample_sizes,
    pop_sd      = pop_sds)


# You should save your simulation results so you don't have to run it every time.
save(
  sim_3_dat, 
  file = here::here("environmental_data" , "data", "lab_ll_sim_output_dispersion_n_1000.RData"))
contour(
  x = sim_3_dat$pop_sd,
  y = sim_3_dat$sample_size,
  z = sim_3_dat$power,
  xlab="standard deviation",
  ylab="sample size",
  main="Q3 Contour plot",
  labcex = 0.6, drawlabels = TRUE, method = "flattest", ticktype = 'detailed')

?contour
# You should save your simulation results so you don't have to run it every time.
save(
  sim_3_dat, 
  file = here::here("environmental_data" , "data", "lab_ll_sim_output_dispersion_n_1000.RData"))

persp(
  x = sim_3_dat$pop_sd,
  y = sim_3_dat$sample_size,
  z = sim_3_dat$power,
  xlab = "sd", ylab = "sample size", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')


persp3d(
  x = sim_3_dat$pop_sd,
  y = sim_3_dat$sample_size,
  z = sim_3_dat$power,
  xlab = "sd", ylab = "sample size", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

require(htmlwidgets)
saveWidget(
  rglwidget(),
  file = here(
    "environmental_data" , "data", "_s2im_plot.html"),
  selfcontained = TRUE
)
