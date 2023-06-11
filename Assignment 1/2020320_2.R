#Q2 

#a part
rm(list=ls())
data = read.csv("/Users/apple/Desktop/data.csv")
data = data[,c(2)]
theta <- c(mean(data),var(data))

LogLikelihood <- function(theta, data){
  mean <- theta[1]
  var <- theta[2]
  n <- length(data)
  log_likelihood <- -(n/2)*log(2*pi*var) - (1/(2*var))*sum((data - mean)**2)
  return(-log_likelihood)
}

results <- optim(theta,LogLikelihood,data = data)
mu_hat <- results$par[1]
sigma_sq_hat <- results$par[2]
print(mu_hat)
print(sigma_sq_hat)

#b part

# fixed variance
mu <- seq(mu_hat-2,mu_hat+2,by = 0.2)
list1 <- c()
for(i in mu){
  y <- c(i,var(data))
  curr_val <- -LogLikelihood(y,data)
  list1 <- c(list1,curr_val)
}
plot(mu, matrix(list1, 1, length(mu)), type="l",xlab = "mean", ylab = "Log-Likelihood", main = "For fixed variance")


#fixed mean
vari <- seq(sigma_sq_hat-2,sigma_sq_hat+2,by = 0.2)
list2 <- c()
for(i in vari){
  y <- c(mean(data),i)
  curr_val <- -LogLikelihood(y,data)
  list2 <- c(list2,curr_val)
}
plot(vari, matrix(list2, 1, length(vari)),type="l", xlab = "variance", ylab = "Log-Likelihood", main = "For fixed mean")

#c part
rm(list=ls())
data = read.csv("/Users/apple/Desktop/data.csv")
data = data[,c(2)]

x = exp(mean(data))

theta <- c(x,var(data))

LogLikelihood <- function(theta, data){
  mean <- theta[1]
  var <- theta[2]
  n <- length(data)
  log_likelihood <- -(n/2)*log(2*pi*var) - (1/(2*var))*sum((data - mean)**2)
  return(-log_likelihood)
}

results <- optim(theta,LogLikelihood,data = data)
mu_hat <- results$par[1]
sigma_sq_hat <- results$par[2]
print(mu_hat)
print(sigma_sq_hat)





