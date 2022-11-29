install.packages("palmerpenguins")
install.packages("here")
require(palmerpenguins)
require(here)
class(penguins)
penguins = data.frame(penguins)
mean(penguins$body_mass_g)
head(penguins)
?mean()
na.rm = TRUE
summary(penguins)
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)
par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)
plot(penguins$bill_depth_mm)

par(mfrow = c(1, 2))
plot(penguins$bill_length_mm)
boxplot(penguins$bill_length_mm)

coplot(flipper_length_mm ~ bill_length_mm | island, data = penguins, rows = 1)

penguins
coplot(bill_depth_mm ~ body_mass_g | species, data = penguins, rows =1)

