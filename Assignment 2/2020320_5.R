rm(list=ls())

# Q5

# sample size
n<-8

# data of FoodA
FoodA<-c(49, 53, 51, 52, 47, 50, 52, 53)

# data of FoodB
FoodB<-c(52, 55, 52, 53, 50, 54, 54, 53)


# data is given so we run inbuilt t-test
test.ans<-t.test(x = FoodA, y = FoodB, paired = TRUE, var.equal=FALSE, alternative = "two.sided")

#test statistic
test_statistic <- test.ans$statistic
cat("Test statistic: ",test_statistic,"\n")

#test critical value
test_critical_value<-qt(0.05/2, df = 7, lower.tail = FALSE)
cat("Test Critical Value: ",test_critical_value,"\n")


#p_value
p_value <- test.ans$p.value
cat("P value: ",p_value,"\n")

#p_value < alpha,reject H0