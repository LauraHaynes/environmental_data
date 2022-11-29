require(here)
dat_g=read.csv(here("environmental_data", "data", "ginkgo_data_2021.csv"))
table(dat_g$site_id)
sort(unique(dat_g$site_id))


require(here)
dat_cat = read.csv(here("environmental_data", "data", "catrate.csv"))
head(catrate)
summary(catrate)
par(mfrow = c(1,1))
hist(catrate$cat.rate,
     xlab = "Catastrophe Rate",
     main = "Histogram of Catastrophe Rates")
shapiro.test(catrate$cat.rate)
t.test(catrate$cat.rate, mu = 2/7)
t.test(catrate$cat.rate, alternative = "greater", mu = 2/7)
t.test(catrate$cat.rate, alternative = "less", mu = 2/7)
wilcox.test(catrate$cat.rate, mu = 2 / 7)

require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)
boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)

t.test(flipper_length_mm ~ species, 
        data = penguin_dat,)
wilcox.test(flipper_length_mm ~ species, 
            data = penguin_dat,)
levels(penguin_dat$species)

png(
  filename = here("Model_1_Activity.png"),
  width = 1500, height = 800,
  res = 180, units = "px")

par(mfrow = c(1 , 2))
hist(dat_adelie$flipper_length_mm, 
     xlab = "flipper length",
     main = "Adelie\nFlipper Length",
     xlim = c(170, 210),
     ylim = c(0, 50))
hist(dat_chinstrap$flipper_length_mm,
     xlab = "flipper length",
     main = "Chinstrap\nFlipper Length",
     xlim = c(170, 220),
     ylim = c(0, 20))

dev.off()
