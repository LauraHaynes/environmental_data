#clears environment
rm(list = ls())

require(here)
rope = read.csv(here("environmental_data" , "data", "rope.csv"))
head(rope)

rope$rope.type = factor(rope$rope.type)
levels(rope$rope.type)

n_obs = 121
n_groups = 6
                        
ss_tot = sum((rope$p.cut-mean(rope$p.cut))^2)
df_tot = n_obs - 1


agg_resids =  aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) x-mean(x))

str(agg_resids)

agg_sum_sq_resids = 
  aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum((x-mean(x))^2))
str(agg_sum_sq_resids)

ss_within = sum(agg_sum_sq_resids$x)
ss_within

df_within = n_obs - n_groups
                        
ss_among = ss_tot - ss_within
ss_among

df_among = n_groups - 1
                        
ms_among  =  ss_among / df_among
ms_within = ss_within / df_within
              
f_ratio = ms_among / ms_within

f_pval = 1 - pf(q = f_ratio, ms_within, df_within)

#ANOVA in R
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)
str(anova_fit_1)
anova_fit_1$"Sum Sq"

#Post-Hoc Testing

#Tukey Honest Significant Difference (HSD) Test
rope2 = droplevels(
  subset(
    rope,
    rope.type %in% c("PI", "VEL", "XTC"))
)

boxplot(
  p.cut ~ rope.type,
  data = rope2,
  las = 2,
  xlab = "",
  ylab = "Proportion Rope Cut",
  main = "Subset of Rope Data")
mtext("Rope Type", side = 1, line = 3)

fit_rope_2 = lm(p.cut ~ rope.type, data=rope2)
rope2_hsd = TukeyHSD(aov(fit_rope_2))
class(rope2_hsd)

round(rope2_hsd$rope.type, digits = 4)

#Q3
bartlett.test(p.cut ~ rope.type, data = rope)

#Q4
head(rope$rope.type)
summary(rope$rope.type)

#Q5

fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

#idk
fit_resids = residuals(fit_rope_1)

shapiro.test(fit_rope_1$rope.type[fit_rope_1$rope.type =="BLAZE"])
?shapiro.test()
fit_rope_1

#Q8
shapiro.test(residuals(fit_rope_1))

#Q9                                       
lapply(
  agg_resids$x,
  function(x) shapiro.test(x)$p
)
                      
#Q12
require(palmerpenguins)
pen_fem = subset(penguins, sex == "female")

boxplot(formula = body_mass_g ~ species,
        data = pen_fem,
        main = "female penguin body mass")

#14
bartlett.test(formula = body_mass_g ~ species,
              data = pen_fem)

#15
pen_fem_fit = lm(body_mass_g ~ species, data = pen_fem)
shapiro.test(residuals(pen_fem_fit))

#Q16
pen_fem_fit_hsd = TukeyHSD(aov(pen_fem_fit))
