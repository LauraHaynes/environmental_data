x = 0:20

barplot(
  dbinom(x, size = 20, prob = 0.1),
  names.arg = x, space = 0,
  main = "Binomial PMF: n = 20, p = 0.1",
  ylab = "Pr(x)", xlab = "x = n successes")

set.seed(12345)
sim_population = rbinom(n = 1000000, size = 20, prob = 0.1)

max(sim_population)

set.seed(5431213)

sim_sample = sample(sim_population, size = 20, replace = T)
hist(sim_sample + 0.00001, main = "sample size = 20", xlab = "x")


mean_sampler = function(pop, sample_size, n_means)
{
  # pre-allocate a results vector
  means = vector(mode = "numeric", length = n_means)
  
  # sampling loop
  for (i in 1:n_means)
  {
    samp = sample(pop, size = sample_size, replace = TRUE)
    means[i] = mean(samp)
  }
  
  return(means)
}



sample_means = mean_sampler(
  pop = sim_population, 
  sample_size = 30,
  n_means = 200)

hist(
  sample_means,
  main = "Distribution of Sample Means\nsample size: 30, number of means: 200",
  xlab = "sample mean")

