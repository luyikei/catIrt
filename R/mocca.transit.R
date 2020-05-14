#############################
# Generate item responses for phase1 for "all" response options
#############################

# n.options can be 3 or 5. Currently this function works only for 3-alternative items.
mocca.transit <- function(theta2, params2, catIrt.object){
  # N and J are specified by the original function. 
  N <- dim(catIrt.object$full_resp)[1] # N is sample size (person)
  J <- dim(catIrt.object$full_resp)[2] # J is item bank size
  # Create a blank list to fill with selected information obtained by catIrt for theta1
  cat_indiv2 <- vector(mode="list", length=N)
  cat_indiv <- catIrt.object$cat_indiv
  
  for(i in 1:N){
    # cat_indiv[[i]][c(2,5)]  gives item IDs, and examinee responses (0 or 1)
    cat_indiv2[[i]] <- cat_indiv[[i]][c(2,5)]   
    # cat_resp2 is for all responses (e.g. 0,1,2)
    cat_indiv2[[i]]$cat_resp2 <- vector("numeric", length=length(cat_indiv[[i]]$cat_resp))
    
    # Insert simulated responses in "cat_resp2" 
    for(n in 1:length(cat_indiv2[[i]]$cat_resp2)){
      # If cat_indiv[[i]]$cat_it=1, cat_indiv2[[i]]$cat_it=1
      # If cat_indiv[[i]]$cat_it=0, generate response (0 or 2).
      if(cat_indiv[[i]]$cat_resp[n]==1){
        cat_indiv2[[i]]$cat_resp2[n]<-2} else {
          # params2 is item parameters for phase2
          params2.vec <- params2[cat_indiv2[[i]]$cat_it[n], ]
          p1 <- exp(params2.vec[1]*(theta2[i]-params2.vec[2]))/(1+exp(params2.vec[1]*(theta2[i]-params2.vec[2])))
          p0 <- 1-p1
          p2 <- exp(params2.vec[1]*(theta2[i]-params2.vec[3]))/(1+exp(params2.vec[1]*(theta2[i]-params2.vec[3])))
          p2.star <- p2/(p0+p2)
          u <- runif(n=1,min=0,max=1)
          if(p2.star > u){cat_indiv2[[i]]$cat_resp2[n] <- 3} else {cat_indiv2[[i]]$cat_resp2[n] <- 1}
        }
    }
  }
  return(cat_indiv2=cat_indiv2)
}
  
  



