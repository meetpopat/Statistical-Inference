rm(list=ls())

# Q1

# sample size 
n<-10

# sample mean
sample_mean <- 17

# sample standard deviation
population_standard_deviation <- 0.5

# given mean height
mu.null<-15

#test statistic
test_statistic<-(sample_mean - mu.null)/(population_standard_deviation/sqrt(n))
cat("Test statistic: ",test_statistic,"\n")

#critical value
test_critical_value <- qnorm(1 - 0.05)
cat("Test Critical Value: ",test_critical_value,"\n")

#p_value
p_value <- pnorm(abs(test_statistic), lower.tail = FALSE )   
cat("P value: ",p_value,"\n")

#p value < alpha,reject H0