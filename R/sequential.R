library(devtools)

# load package w/o installing
load_all("/Users/luyik/Downloads/catIrt")
library(readr)


points <- seq(-3,3,length.out=15)
theta <- rep(points,1)

item_params0 <- read.csv("FIPE for simulation.csv", header = T, fileEncoding = "UTF-8-BOM")
item_params <- as.matrix(item_params0[,1:3])

set.seed(1234)

cat_start <- list(n.start=1, init.theta=0, select="UW-FI", at="theta", it.range=c(-0.5, 0.5), n.select=1, delta=NULL, score="WLE", range=c(-3,3), step.size=NULL, leave.after.MLE= NULL)
cat_middle <- list(select="UW-FI", at="theta", it.range=NULL, n.select=1, delta=NULL, score="WLE", range=c(-3,3), expos="none")
cat_terminate <- list(term="fixed", score="WLE", n.min=1, n.max=30)

phase1.params <- list(params=item_params, mod="brm", theta=theta, resp=NULL, catStart = cat_start, catMiddle = cat_middle, catTerm=cat_terminate, progress=T, ddist=NULL)

params2 <- read.csv("Real Item params 360 items Dim 2.csv", header = T, fileEncoding = "UTF-8-BOM")

results <- do.call(catIrt, phase1.params)

points <- seq(-3,3,length.out=15)
theta2 <- rep(points,1)

N <- dim(results$full_resp)[1]
M <- dim(results$full_resp)[2]
resp <- matrix(0, nrow = N, ncol = M)
it <- matrix(0, nrow = N, ncol = M)
for(i in 1:N){
  for(n in 1:M){
    if(results$full_resp[1,n]==1){
      resp[i, n]<-2
    } else {
      params2.vec <- params2[n, ]
      p1 <- exp(params2.vec[1]*theta2[i]+params2.vec[2])/(1+exp(params2.vec[1]*theta2[i]+params2.vec[2]))
      p0 <- 1-p1
      p2 <- exp(params2.vec[1]*theta2[i]+params2.vec[3])/(1+exp(params2.vec[1]*theta2[i]+params2.vec[3]))
      p2.star <- p2/(p0+p2)
      u <- runif(n=1,min=0,max=1)
      if(p2.star > u){resp[i, n] <- 3} else {resp[i, n] <- 1}
    }
  }
  for(n in 1:length(results$cat_indiv[[i]]$cat_it)){
    it[i, results$cat_indiv[[i]]$cat_it[n]] <-  1
  }
}


cat_start2 <- list(n.start=1, init.theta=0, select="UW-FI", at="theta", n.select=1, delta=NULL, score="WLE", range=c(-3,3), step.size=NULL, leave.after.MLE= FALSE)
cat_middle2 <- list(select="UW-FI", at="theta", it.range=NULL, n.select=1, delta=NULL, score="MLE", range=c(-3,3), expos="none")
cat_terminate2 <- list(term="fixed", score="MLE", n.min=1, n.max=30)


phase2.params <- list(params=as.matrix(params2[,1:3]), mod="grm", it=it, theta=theta2, resp=resp, catStart = cat_start2, catMiddle = cat_middle2, catTerm=cat_terminate2, progress=T, ddist=NULL)

results2 <- do.call(catIrt, phase2.params)

for(i in 1:N){
  # Check if phase1 items are not administered in phase2
  if(TRUE %in% sapply(results$cat_indiv[[i]]$cat_it, function(v){return(v %in% results2$cat_indiv[[i]]$cat_it)})) {
    print("Not alright!")
  } else {
    #print("Great")
  }
}
