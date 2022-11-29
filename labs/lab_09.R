require(here)
catrate = read.csv(here("environmental_data" , "data", "catrate.csv"))
head(catrate)

n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(
  x = n_success,
  n = n_years,
  p = 0.5)

late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate) 

binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate,
  alternative ='less')

t.test(catrate$cat.rate, mu = 2/7)
wilcox.test(catrate$cat.rate, mu = 2/7)

veg = read.csv(here("environmental_data" ,"data", "vegdata.csv"))
head(veg)

boxplot(pine ~ treatment, data = veg)

veg2 = droplevels(
  subset(
    veg,
    treatment %in% c('control','clipped')
  ))

# verify that treatment is factorized
veg2$treatment = factor(veg2$treatment)

var.test(
  pine ~ treatment,
  data = veg2)

shapiro.test(veg2$pine[veg2$treatment=="control"])
shapiro.test(veg2$pine[veg2$treatment=="clipped"])

fligner.test(
  pine ~ treatment,
  data = veg2)

bartlett.test(pine ~ treatment, data = veg)

fligner.test(pine ~ treatment, data = veg)

t.test(
  pine ~ treatment,
  data = veg2)

wilcox.test(
  pine ~ treatment,
  data = veg2)

install.packages("datarium")
require(datarium)
data("mice2")
head(mice2)

t.test(mice2$before, mice2$after, paired = TRUE)
wilcox.test(mice2$before, mice2$after, paired = TRUE)

t.test(mice2$before, mice2$after, paired = FALSE)

disp = read.csv(here("environmental_data" , "data", "dispersal.csv"))
disp

plot(
  disp.rate.ftb ~ disp.rate.eb,
  data = disp,
  main = "Marbled Salamander Dispersal Rates",
  xlab = "Dispersal Rate\nFirst Time Breeders",
  ylab = "Dispersal Rate\nExperienced Breeders",
  pch = 21, col = 1, bg = "steelblue"
)

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE,
  main = "Mike's Plot of Marbled Salamanders\nFirst-Time Breeders: ECDF")

plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE,
  main = "Mike's Plot of Marbled Salamanders\nFirst-Time and Experienced Breeders: ECDF")
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)
legend(
  x = 0.4, y = 0.4,
  lty = c(1, 3),
  legend = c("first-time", "experienced"),
  title = "Breeder Class")

ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

prop.test(
  x = c(4,16),
  n = c(40,250))

owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq_owls = chisq.test(owls)
chisq_owls

round(chisq_owls$expected, 1)
chisq_owls$observed

round(
  chisq_owls$observed - chisq_owls$expected,
  digits = 1)

fisher.test(owls)

birds   = read.csv(here("environmental_data" , "data", "bird.sta.csv"))
hab     = read.csv(here("environmental_data" , "data", "hab.sta.csv"))
birdhab = merge(
  birds,
  hab, by=c("basin", "sub", "sta"))

table(
  birdhab$s.edge,
  birdhab$BRCR > 0)

br_creeper_table = table(
  birdhab$s.edge, 
  birdhab$BRCR > 0)[, 2:1]

br_creeper_table

#Q1-2
chisq.test(br_creeper_table)

require(palmerpenguins)
fit_fl_sp = 
  lm(
    formula = flipper_length_mm ~ species,
    data = penguins)

#Q3
fit_species = 
  lm(
    formula = body_mass_g ~ species,
    data = penguins)

#Q4
fit_sex = 
  lm(
    formula = body_mass_g ~ sex,
    data = penguins)

#Q5
fit_both = 
  lm(
    formula = body_mass_g ~ sex:species,
    data = penguins)

#Q6
boxplot(
  formula = body_mass_g ~ species,
  data = penguins,
  main = "fit_species conditional boxplot",
  ylab = "body mass (g)"
)

#Q7
boxplot(
  formula = body_mass_g ~ sex,
  data = penguins,
  main = "fit_sex conditional boxplot",
  ylab = "body mass (g)"
)


#Q8
boxplot(
  formula = body_mass_g ~ sex:species,
  data = penguins,
  main = "Laura's Doubly \n Conditioned Boxplot",
  xlab = "",
  ylab = "body mass (g)",
  names = c("female\nAdelie", "male\nAdelie", "female\nChinstrap",
            "male\nChinstrap", "female\nGentoo", "male\nGentoo"),
  las = "2"
)

#Q11
bartlett.test(body_mass_g ~ species, data = penguins)

#Q12
bartlett.test(body_mass_g ~ sex, data = penguins)

#Q13
dat_groups = aggregate(
  body_mass_g ~ sex:species,
  data = penguins,
  FUN = c)
str(dat_groups)

dat_groups$body_mass_g

bartlett.test(dat_groups$body_mass_g, data = penguins)

#Q15
dat_fl =  read.csv(here("environmental_data" ,"data", "trees_FL.csv"))
head(dat_fl)

par(mfrow = c(2,2))

barplot(
  table(dat_fl$ProbabilityofFailure), 
  main = "dat_fl\nProbabilityofFailure",
  ylab = "tree count")

barplot(
  table(dat_fl$Failure_Standardized), 
        main = "dat_fl\nFailure_Standardized",
        ylab = "tree count")

hist(dat_fl$DBH_in,
     main = "Hist DBH")

plot(dat_fl$DBH_in, dat_fl$HeighttoTop_ft,
    main = "scatterplot",
      xlab = "DBH",
     ylab = "Height")

#Q17
head(dat_fl$DBH_in)

whole_dat_fl = droplevels(subset(dat_fl, Failure_Standardized == "whole", na.rm = TRUE))
none_dat_fl = droplevels(subset(dat_fl, Failure_Standardized == "none", na.rm = TRUE))

ks.test(whole_dat_fl$DBH_in, none_dat_fl$DBH_in)

#Q20
par(mfrow = c(1,1))
plot(dat_fl$DBH_in, dat_fl$HeighttoTop_ft,
     main = "scatterplot",
     xlab = "DBH",
     ylab = "Height")
ks.test(dat_fl$DBH_in, dat_fl$HeighttoTop_ft)

#Q21
dat_fl$fail = factor(dat_fl$Failure_Standardized != "none")

levels(dat_fl$fail) = c("No Fail", "Fail")

fl_table_2 = table(
  dat_fl$ProbabilityofFailure,
  dat_fl$fail)
fl_table_2

chisq.test(fl_table_2)

#22
chisq_fl = chisq.test(fl_table_2)

chisq_fl$expected
chisq_fl$observed

round(
  chisq_fl$observed - chisq_fl$expected,
  digits = 1)
