rm(list=ls())

# Q4

# sample size 
n1<-9
n2<-16

# sample mean
x1 <- 2
x2 <- 3.2

# sample standard deviation
S1 <- sqrt(0.75)
S2 <- 1.00

A <- S1**2/n1
B <- S2**2/n2

#test statistic
test_statistic<-(x2-x1)/(sqrt(S1**2/n1 + S2**2/n2))
cat("Test statistic: ",test_statistic,"\n")

#degree of freedom
df <- ((A+B)**2)/(A**2/(n1-1) + B**2/(n2-1))

#critical value
test_critical_value<-(qt(p = 0.05/2,df = ((A+B)**2)/(A**2/(n1-1) + B**2/(n2-1)),lower.tail = FALSE))
cat("Test Critical Value: ",test_critical_value,"\n")

#p_value
p_value<-2 * pt(q = test_statistic,df = ((A+B)**2)/(A**2/(n1-1) + B**2/(n2-1)), lower.tail = FALSE)
cat("P value: ",p_value,"\n")

#p value < alpha,reject H0
