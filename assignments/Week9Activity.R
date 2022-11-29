require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")

#Q1
dat_ade = droplevels(subset(penguins, species == "Adelie"))
dat_ade$sex <- as.factor(dat_ade$sex)
levels(dat_ade$sex) <- c("F","M")
boxplot(dat_ade$body_mass_g ~ dat_ade$sex,
        main = "Boxplots of Penguin Body Mass by Sex",
        ylab = "Body mass (g)",
        xlab = "Sex")

#Q2
female_pen = subset(dat_ade, sex == "F" ) 
male_pen = subset(dat_ade, sex == "M")

?t.test()

t.test(female_pen$body_mass_g, alternative = "greater", mu = 0, paired = FALSE, var.equal = FALSE)

#Q4

pen_ecdf = ecdf(male_pen$body_mass_g)

t.test(pen_ecdf(4000), alternative = "greater", mu = 0, paired = FALSE, var.equal = FALSE)


t.test(dat_ade$body_mass_g ~ dat_ade$sex, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE)

t.test(female_pen$body_mass_g, male_pen$body_mass_g) 
?t.test()

#Q8 

t.test(male_pen$body_mass_g, female_pen$body_mass_g, alternative = "less", paired = TRUE, var.equal = FALSE)
