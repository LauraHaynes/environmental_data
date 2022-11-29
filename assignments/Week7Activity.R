qnorm(c(0.025, 0.975))
qnorm(c(0.05, 0.95))
?qt()
qt(p=c(0.025, 0.975), df = 10)

qnorm(c(0.025, 0.975))
qt(p=c(0.025, 0.975), df =1000)

#Q6
sse*qt(c(0.025, 0.975), df = 49) 

ssd = 3.14
sse = ssd/sqrt(49)
print(sse)


