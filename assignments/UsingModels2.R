require(here)
require(palmerpenguins)
t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)

t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))

par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")

require(palmerpenguins)
boxplot(body_mass_g ~ species, data = penguins)

dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)

shapiro.test(dat_chinstrap$body_mass_g)

aggregate(body_mass_g ~ species, data = penguins, FUN = mean)

aggregate(
  body_mass_g ~ species,
  data = penguins,
  FUN = function(x) shapiro.test(x)$p.value)

fit_species = lm(body_mass_g ~ species, data = penguins)

summary(fit_species)

anova(fit_species)

#One-Way ANOVA

fit_species = lm(body_mass_g ~ species, data = penguins)

summary(fit_species)
anova(fit_species)

par(mfrow = c(1, 1))
boxplot(body_mass_g ~ species, data = penguins)

#Q1-3
png(
  filename = here("UsingModels2.png" ), 
  width = 1500, height = 1500, 
  res = 180, units = "px")

boxplot(
  formula = body_mass_g ~ sex:species,
  data = penguins,
  xlab = "",
  ylab = "body mass (g)",
  names = c("female\nAdelie", "male\nAdelie", "female\nChinstrap",
            "male\nChinstrap", "female\nGentoo", "male\nGentoo"),
  las = "2"
)

dev.off()


fit_additive = lm(body_mass_g ~ sex + species, data = penguins)

fit_both = lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_both)

lm(bill_length_mm ~ body_mass_g, data = penguins)

#this didn't work
chinstrap = droplevels(subset(penguins, species == "chinstrap" , na.rm = TRUE))
f_chinstrap = droplevels(subset(chinstrap, sex == "female" , na.rm = TRUE))
summary(f_chinstrap)

#second try subsetting
chinstrap = droplevels(subset(penguins, species == "Chinstrap"))
f_chinstrap = droplevels(subset(penguins, sex == "female"))
summary(f_chinstrap$body_mass_g)
