#Q1

#a part

rm(list=ls())
lambdas <- c(1,2,3,4)

for(i in lambdas){
  data <- rexp(1000,i)
  print(data)
}

#b part
lambdas <- c(1,2,3,4)
LogLikelihood <- function(lambda,x){
  -sum(log(dexp(x,lambda)))
}


for(lambda in lambdas){
  x <- rexp(1000,lambda)
  set <- c(0.33,0.65,1/mean(x))
  print("For Lamba =")
  print(lambda)
  for(i in set){
    mle <- optim(par = i, fn = LogLikelihood, x = x, method = "L-BFGS-B")
    cat("MLE with Initial Value = ",i,"is:",mle$par,"\n")
  }
}

#c part
seq1 <- seq(0,10,by = 0.25)
for (lambda in lambdas){
  data <- rexp(1000,lambda)
  y <- c()
  for(i in seq1){
    y <- c(y,-1*LogLikelihood(i,data))
  }
  plot(seq1, matrix(y, 1, length(seq1)), type="l",xlab = "Lambda", ylab = "Log Likelihood", main = "Plot")
}




