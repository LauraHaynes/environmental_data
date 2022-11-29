require(here)

dat_bird = read.csv(here("environmental_data" , "data", "bird.sta.csv"))
dat_hab = read.csv(here("environmental_data" , "data", "hab.sta.csv"))

class(dat_bird)
head(dat_bird)
head(dat_hab)

hist(dat_bird$PUFI  ,xlab = "Number of birds counted", breaks = 0:7 - 0.5)

pairs(dat_hab[c("elev" , "slope" , "aspect")])
