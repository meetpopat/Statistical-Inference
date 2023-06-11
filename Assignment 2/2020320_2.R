rm(list=ls())

# Q2

# sample size 
n<-75

# sample mean
sample_mean <- 17.4

# sample standard deviation
sample_standard_deviation <- 6.3

# given mean length of time on death row
mu.null<-15

#test statistic
test_statistic<-(sample_mean - mu.null)/(sample_standard_deviation/sqrt(n))
cat("Test statistic: ",test_statistic,"\n")

#critical value
test_critical_value<-(qt(p = 0.05/2,df = n - 1,lower.tail = FALSE))
cat("Test Critical Value: ",test_critical_value,"\n")

#p_value
p_value<-2 * pt(q = test_statistic, df = n - 1, lower.tail = FALSE)
cat("P value: ",p_value,"\n")

#p value < alpha, reject H0