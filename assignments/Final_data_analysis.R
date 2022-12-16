#Q1-4 Data Exploration

require(here)
delomys_dat = read.csv(
  here("environmental_data" , "data", "delomys.csv")
)

head(delomys_dat)
summary(delomys_dat$body_mass)
summary(delomys_dat$body_length)

shapiro.test(delomys_dat$body_mass)
shapiro.test(delomys_dat$body_length)

del_mass = delomys_dat$body_mass
del_length = delomys_dat$body_length

plot(del_mass, del_length,
     main = "scatterplot of delomys \n body mass & length",
     xlab = "body mass",
     ylab = "body length"
     )

par(mfrow = c(1,2))

hist(del_mass,
     main = "Histogram of \n delomys body mass",
     xlab = "body mass"
     )

hist(del_length,
     main = "Histogram of \n delomys body length",
     xlab = "body length"
)

par(mfrow = c(1,1))

boxplot(
  formula = del_mass ~ binomial,
  data = delomys_dat,
  main = "delomys body mass \n conditioned on species",
  xlab = "species",
  ylab = "body mass")
  
boxplot(
    formula = del_mass ~ sex,
    data = delomys_dat,
    ylab = "body mass",
    main = "delomys body mass \n conditioned on sex")

boxplot(
  formula = del_mass ~ sex:binomial,
  data = delomys_dat,
  main = "Laura's Doubly \n Conditioned Boxplot",
  xlab = "",
  ylab = "body mass (g)",
  names = c("female\ndorsalis", "male\ndorsalis", "female\nsublineatus",
            "male\nsublineatus"),
  las = "2"
)


#Q5-6: Model Assumptions

fit1 = lm(del_mass ~ del_length, data = delomys_dat)
fit2 = lm(del_mass ~ sex, data = delomys_dat)
fit3 = (lm(del_mass ~ binomial, data = delomys_dat))
fit4 = (lm(del_mass ~ sex + binomial, data = delomys_dat))
fit5 = (lm(del_mass ~ sex * binomial, data = delomys_dat))

fit1_resid = residuals(fit1)
fit2_resid = residuals(fit2)
fit3_resid = residuals(fit3)
fit4_resid = residuals(fit4)
fit5_resid = residuals(fit5)

hist(fit1_resid)
hist(fit2_resid)
hist(fit3_resid)
hist(fit4_resid)
hist(fit5_resid)

shapiro.test(fit1_resid)
shapiro.test(fit2_resid)
shapiro.test(fit3_resid)
shapiro.test(fit4_resid)
shapiro.test(fit5_resid)

# Q7-9: Simple Linear Regression

summary(fit1)

# Q10-13

summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)

# Q14-16

anova(fit2)
anova(fit3)
anova(fit4)
anova(fit5)

# Q17-18

AIC(
  fit1,
  fit2,
  fit3,
  fit4,
  fit5
)
