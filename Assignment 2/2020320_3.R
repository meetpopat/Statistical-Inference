rm(list=ls())

#Q3

# sample size
n<-10

# data
x<-c(14.3, 12.6, 13.7, 10.9, 13.7, 12.0, 11.4, 12.0, 12.6, 13.1)

#sample standard deviation
sample_standard_deviation <- sd(x)

#sample standard mean
sample_mean<-mean(x)

#mean produce per hectare
mu.null<-12

#test statistic
test_statistic<-(sample_mean - mu.null)/(sample_standard_deviation/sqrt(n))
cat("Test statistic: ",test_statistic,"\n")

#critical value
test_critical_value<-(qt(p = 0.05/2,df = n - 1,lower.tail = FALSE))
cat("Test Critical Value: ",test_critical_value,"\n")

#p_value
p_value<-2 * pt(q = test_statistic, df = n - 1, lower.tail = FALSE)
cat("P value: ",p_value,"\n")

#p value > alpha, accept H0


