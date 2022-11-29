?dbinom()

#Q1
dbinom(x = 3, size = 4, prob = 0.75, log = FALSE)

#Q2
pbinom(q = 3, size = 4, prob = 0.75, lower.tail = TRUE, log.p = FALSE)

#Q3
1-(pbinom(q = 3, size =5, prob = 0.75, log.p = FALSE))

#Q4
?pnorm()
pnorm(1.2, mean = 2, sd = 2)

#Q5
1-pnorm(1.2, mean = 2, sd = 2)

#Q6
(1-pnorm(1.2, mean = 2, sd =2))-(1-pnorm(3.2, mean = 2, sd =2))
#this def isn't right...


#Q7
