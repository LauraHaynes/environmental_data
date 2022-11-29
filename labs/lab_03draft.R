require(psych)
pairs.panels(iris)
names(iris)
pairs.panels(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])

require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)

require(here)
dat_hab = read.csv(
  here("data", "hab.sta.csv")
)

dat_all = merge(dat_bird , dat_hab, by = c("basin" , "sub" , "sta"))

plot(ba.tot ~ elev, data = dat_all)


#ba.tot is basal area


#below is example from lab instructions
sample(dat_all$CEWA, 100)

dat_all$CEWA >= 1
cewa_present_absent <- as.numeric(dat_all$CEWA >= 1)
plot(x = dat_all$elev, y = cewa_present_absent)



# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
  
  
}




plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)


plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)

head(dat_all)

#this is how u can see which sp have the most p/a
apply(dat_bird[, -(1:3)], 2, sum)

#Use the pair plot function from psych to create a pair plot of the three terrain variables (slope, aspect, elevation) and basal area.
pairs.panels(dat_all[, c("slope", "aspect", "elev", "ba.tot")])

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

dat_all$WIWR >= 1
WIWR_present_absent <- as.numeric(dat_all$WIWR >= 1)
plot(x = dat_all$ba.tot, y = WIWR_present_absent,
     title(main="Likelihood of WIWR Occurance") ,
     xlab="Total Basal Area", ylab="Winter Wren Presence & Absence",
     )
curve(logistic_midpoint_slope(x, midpoint = 100, slope = 0.1), add = TRUE)


dat_all$CBCH >= 1
CBCH_present_absent <- as.numeric(dat_all$CBCH >= 1)
plot(x = dat_all$ba.tot, y = CBCH_present_absent,
     xlab="Total Basal Area", ylab="Chestnut-bk Chickadee Presence & Absence",
)

plot(x = dat_all$ba.hard , y = CBCH_present_absent,
     title(main="Likelihood of CBCH Occurance") ,
     xlab="basal area of hardwoods", ylab="Chestnut-bk Chickadee Presence & Absence"
     )
curve(logistic_midpoint_slope(x, midpoint = 75, slope = 0.1), add = TRUE)

sum(dat_all$GRJA)
sum(dat_all$GRJA >= 1)
