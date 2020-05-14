#############################
# Generate true theta2 function
#############################

mocca.gen.true.theta2 <- function(x, lower, upper, by, n.times){
  points <- round(seq(lower, upper, by), digits=2) 
  theta1 <- rep(points,n.times)
  # Generate theta2
  n.theta <- length(theta1)
  u <- runif(n.theta, lower, upper)
  theta2 <- x*theta1 + (1-x)*u
  # Next approximate theta2. Get the nearest value in "points" for each theta2 and replace it.
  ret <- sapply(theta2, function(theta) {return(points[which.min(abs(theta - points))])})
  return(ret)
}

