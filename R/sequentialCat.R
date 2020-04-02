
sequentialCat <- function( phase1.params, phase2.params )
{
  results <- do.call(catIrt, phase1.params)

  N <- dim(results$full_resp)[1]
  M <- dim(results$full_resp)[2]
  resp <- matrix(0, nrow = N, ncol = M)
  it <- matrix(0, nrow = N, ncol = M)
  for(i in 1:N){
    # Insert simulated responses in "cat_resp_phase2" 
    for(n in 1:length(results$cat_indiv[[i]]$cat_resp)){
      if(results$cat_indiv[[i]]$cat_it[n]==1){
        it[i, n] <-  1
        resp[i, n]<-1
        } else {
          # params2.vec <- params2[results$cat_indiv[[i]]$cat_it[n], ]
          # p1 <- exp(params2.vec[1]*theta2[i]+params2.vec[2])/(1+exp(params2.vec[1]*theta2[i]+params2.vec[2]))
          # p0 <- 1-p1
          # p2 <- exp(params2.vec[1]*theta2[i]+params2.vec[3])/(1+exp(params2.vec[1]*theta2[i]+params2.vec[3]))
          # p2.star <- p2/(p0+p2)
          # u <- runif(n=1,min=0,max=1)
          # if(p2.star > u){resp[i, n] <- 2} else {resp[i, n] <- 0}
          resp[i, n] <- 0
        }
    }
  }
  
  phase2.params$it = it
  phase2.params$resp = resp
  
  results2 <- do.call(catIrt, phase2.params)
  
  return(list(phase1 = results, phase2 = results2))
}